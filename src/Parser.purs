module Hacss.Internal.Parser (atScope, cls, combinator, context, declaration, priority, property, pseudoElement, rule, rules, selector, value) where

import Prelude
import Control.Alt ((<|>))
import Control.Lazy (fix)
import Control.Monad.Rec.Class (whileJust)
import Data.Array (cons, elem, fromFoldable, length, many, nub, reverse, some, sortWith) as A
import Data.Either (Either(..))
import Data.Foldable (null)
import Data.Lens ((^.), (.~))
import Data.List (List(Nil), (:))
import Data.List (null) as L
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.String.CodeUnits (fromCharArray, length) as S
import Data.String.Common (joinWith) as S
import Data.Tuple (Tuple(..))
import Text.Parsing.StringParser (Parser, fail, try)
import Text.Parsing.StringParser.CodeUnits (anyChar, anyDigit, char, eof, lowerCaseChar, noneOf, regex, string)
import Text.Parsing.StringParser.Combinators ((<?>), lookAhead, optionMaybe, sepBy1)
import Hacss.Internal.Data (AtScope(..), Class(..), Combinator(..), Context(..), Declaration(..), Priority(..), Property(..), PseudoElement(..), Rule, Selector, ValCtx(..), ValExpr(..), Value(..), Variable(..), emptyRule, emptySelector, ruleAtScope, ruleDeclarations, rulePriority, ruleSelector, selectorContext, selectorClasses, selectorPseudoElement)

whiteSpace :: Array Char
whiteSpace = [ ' ', '\t', '\r', '\n' ]

oneOfString :: Array String -> Parser String
oneOfString xs =
  let
    err = "Expected one of: " <> S.joinWith ", " ((\x -> "\"" <> x <> "\"") <$> xs) <> "."
  in
    do
      x <- regex $ "(" <> (S.joinWith "|" $ A.reverse $ A.sortWith S.length xs) <> ")"
      if x == "" then
        fail err
      else
        pure x
      <?> err

value :: Parser Value
value = variableOnly <|> normal
  where
  variableOnly = Value <<< Left <$> variable

  normal = Value <<< Right <$> A.some (calc <|> url <|> quoted <|> unquoted)
    where
    calc = do
      _ <- string "calc("
      x <- Tuple (Just Calc) <<< A.reverse <<< A.fromFoldable <$> calc' 1 Nil Nil
      pure x
      where
      calc' n chars exprs
        | n < 1 =
          if L.null chars then
            pure exprs
          else
            pure $ (Lit $ S.fromCharArray $ A.reverse $ A.fromFoldable chars) : exprs
        | otherwise =
          fix \_ ->
            ( do
                v <- var
                if L.null chars then
                  calc' n Nil $ v : exprs
                else
                  calc' n Nil $ v : (Lit $ S.fromCharArray $ A.reverse $ A.fromFoldable chars) : exprs
            )
              <|> ( do
                    c <- noneOf ([ '#' ] <> whiteSpace) <|> (char '#' <* lookAhead (noneOf [ '{' ]))
                    case c of
                      ')' -> calc' (n - 1) (if n > 1 then ')' : chars else chars) exprs
                      '(' -> calc' (n + 1) ('(' : chars) exprs
                      _
                        | c `A.elem` [ '+', '-', '*', '/' ] -> calc' n (' ' : c : ' ' : chars) exprs
                      _ -> calc' n (c : chars) exprs
                )

    url = do
      _ <- string "url('"
      x <- Tuple (Just URL) <$> A.some (try var <|> lit)
      _ <- string "')"
      pure x
      where
      lit = Lit <<< S.fromCharArray <$> A.some allowedChar
        where
        allowedChar =
          (string "__" *> pure ' ')
            <|> (noneOf $ whiteSpace <> [ '#', '\'' ])
            <|> try (char '#' <* lookAhead (noneOf [ '{' ]))

    quoted = do
      _ <- char '\''
      x <- Tuple (Just Quoted) <$> A.some (try var <|> lit)
      _ <- char '\''
      pure x
      where
      lit = Lit <<< S.fromCharArray <$> A.some allowedChar
        where
        allowedChar =
          (string "__" *> pure ' ')
            <|> (noneOf $ whiteSpace <> [ '#', '\'' ])
            <|> try (char '#' <* lookAhead (noneOf [ '{' ]))

    unquoted = Tuple Nothing <$> A.some (try var <|> lit)
      where
      lit = Lit <<< S.fromCharArray <$> A.some allowedChar
        where
        allowedChar = do
          nonLit <-
            (||) <$> (isJust <$> optionMaybe (lookAhead url))
              <*> (isJust <$> optionMaybe (lookAhead calc))
          if nonLit then
            fail "Start of a URL"
          else
            (string "__" *> pure ' ')
              <|> (noneOf $ whiteSpace <> [ '#', ';', '}', '\'' ])
              <|> try (char '#' <* lookAhead (noneOf [ '{' ]))

    var = do
      _ <- string "#{"
      v <- variable
      _ <- char '}'
      pure $ Var v

  variable = Variable <<< S.fromCharArray <$> ((char '$' <* lookAhead lowerCaseChar) *> A.some allowedChar)
    where
    allowedChar = lowerCaseChar <|> anyDigit <|> try (char '-' <* lookAhead (lowerCaseChar <|> anyDigit))

property :: Parser Property
property =
  ( do
      vendor <- oneOfString [ "-moz", "-ms", "-o", "-webkit" ]
      Property <<< (vendor <> _) <<< S.fromCharArray <$> A.some allowedChar
  )
    <|> ( do
          h <- lowerCaseChar
          t <- A.some allowedChar
          pure $ Property $ S.fromCharArray $ h `A.cons` t
      )
  where
  allowedChar = lowerCaseChar <|> try (char '-' <* lookAhead lowerCaseChar)

declaration :: Parser Declaration
declaration = do
  p <- property
  _ <- char ':'
  v <- value
  pure $ Declaration $ Tuple p v

cls :: Parser Class
cls =
  fix \_ ->
    (NotPseudoClass <$> (string ":not(" *> A.some cls <* char ')'))
      <|> (NamedClass <$> (char '.' *> basic (lowerCaseChar <|> anyDigit)))
      <|> ( do
            _ <- char ':'
            a <- (basic lowerCaseChar <|> (oneOfString [ "-moz-", "-ms-", "-o-", "-webkit-" ] >>= \v -> (v <> _) <$> basic lowerCaseChar)) <?> "Expected a lowercase letter or vendor prefix."
            b <- optionMaybe $ S.fromCharArray <$> (char '(' *> A.some (noneOf $ whiteSpace <> [ ')' ]) <* char ')')
            pure
              $ case b of
                  Nothing -> PseudoClass a
                  Just b' -> PseudoClass $ a <> "(" <> b' <> ")"
        )
  where
  basic c = do
    h <- c
    t <- A.some allowedChar
    pure $ S.fromCharArray $ h `A.cons` t
    where
    allowedChar = c <|> try (char '-' <* lookAhead c)

combinator :: Parser Combinator
combinator =
  do
    char '_' *> pure Ancestor
    <|> char '>'
    *> pure Parent
    <|> char '+'
    *> pure AdjSib
    <|> char '~'
    *> pure GenSib

pseudoElement :: Parser PseudoElement
pseudoElement = do
  _ <- string "::"
  prefix <- fromMaybe "" <$> (optionMaybe $ oneOfString [ "-moz-", "-ms-", "-o-", "-webkit-" ])
  h <- lowerCaseChar
  t <- A.some (lowerCaseChar <|> try (char '-' <* lookAhead lowerCaseChar))
  pure $ PseudoElement $ prefix <> S.fromCharArray (h `A.cons` t)

context :: Parser Context
context = Context <$> (Tuple <$> A.some cls <*> combinator)

selector :: Parser Selector
selector = do
  context' <- optionMaybe $ try context
  classes' <- A.many $ try cls
  pseudoElement' <- optionMaybe pseudoElement
  if null context' && null classes' && null pseudoElement' then
    fail "Expected a context, classes, and/or pseudo-element."
  else
    pure $ emptySelector # selectorContext .~ context'
      # selectorClasses
      .~ classes'
      # selectorPseudoElement
      .~ pseudoElement'

atScope :: Parser AtScope
atScope = do
  _ <- char '@'
  h <- allowedChar
  t <- A.some $ allowedChar <|> try (char '-' <* lookAhead allowedChar)
  pure $ AtScope $ S.fromCharArray $ h `A.cons` t
  where
  allowedChar = lowerCaseChar <|> anyDigit

priority :: Parser Priority
priority = Priority <<< A.length <$> A.some (char '!')

rule :: Parser Rule
rule =
  ( try
      ( ( \(Tuple atScope' (Tuple selector' declarations')) ->
            emptyRule # ruleAtScope .~ Just atScope'
              # ruleSelector
              .~ Just selector'
              # ruleDeclarations
              .~ declarations'
        )
          <$> wrap atScope (wrap selector declarations)
      )
      <|> ( ( \(Tuple atScope' declarations') ->
              emptyRule
                # ruleAtScope
                .~ Just atScope'
                # ruleDeclarations
                .~ declarations'
          )
            <$> wrap atScope declarations
        )
      <|> ( ( \(Tuple selector' declarations') ->
              emptyRule
                # ruleSelector
                .~ Just selector'
                # ruleDeclarations
                .~ declarations'
          )
            <$> wrap selector declarations
        )
      <|> ( (\declarations' -> emptyRule # ruleDeclarations .~ declarations')
            <$> A.some (declaration <* char ';')
        )
  )
    >>= \base -> priority <|> pure (Priority 0) >>= \p -> pure $ rulePriority .~ p $ base
  where
  declarations = A.fromFoldable <$> declaration `sepBy1` char ';'

  wrap :: forall o i. Parser o -> Parser i -> Parser (Tuple o i)
  wrap outer inner = do
    o <- outer
    _ <- char '{'
    i <- inner
    _ <- char '}'
    pure $ Tuple o i

rules :: Parser (Array Rule)
rules = A.sortWith (\r -> r ^. ruleAtScope) <<< A.nub <<< A.fromFoldable <$> whileJust rules'
  where
  rules' =
    (eof *> pure Nothing)
      <|> ( do
            r <- try rule
            pure $ Just (r : Nil)
        )
      <|> (anyChar *> pure (Just Nil))
