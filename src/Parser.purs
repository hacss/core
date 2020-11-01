module Hacss.Parser where

import Prelude
import Control.Alt ((<|>))
import Control.Lazy (fix)
import Control.Monad.Reader.Class (class MonadAsk, asks)
import Control.Monad.Rec.Class (class MonadRec, whileJust)
import Control.Monad.Trans.Class (lift)
import Data.Array (fromFoldable, length, many, null, reverse, some, sortWith) as A
import Data.Char.Unicode (isDigit, isLetter, isLower)
import Data.Foldable (class Foldable)
import Data.Foldable (oneOf) as F
import Data.List (List(Nil), (:))
import Data.List (many, nub) as L
import Data.List.Lazy (replicateM)
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Data.String.CodeUnits (fromCharArray, length) as S
import Data.String.Common (joinWith) as S
import Data.Tuple (Tuple(..))
import Text.Parsing.Parser (ParserT, fail)
import Text.Parsing.Parser.Combinators ((<?>), lookAhead, notFollowedBy, optionMaybe, sepBy1, try, withErrorMessage)
import Text.Parsing.Parser.String (anyChar, char, eof, noneOf, satisfy, string)
import Text.Parsing.Parser.String (oneOf) as P
import Hacss.Data
  ( AtScope(..)
  , ClassName(..)
  , Combinator(..)
  , Context
  , Declaration(..)
  , Property(..)
  , PseudoClass(..)
  , PseudoElement(..)
  , QualifiedVal(..)
  , Rule
  , Selector
  , Value(..)
  , ValExpr(..)
  , ValSeg(..)
  , Variable(..)
  , variableName
  )

className :: forall m. Monad m => ParserT String m ClassName
className = do
  _ <- char '.'
  x <- lower
  xs <- L.many $ lower <|> (char '-' <* lookAhead lower)
  pure $ ClassName $ S.fromCharArray $ A.fromFoldable $ x : xs

pseudoClass :: forall m. Monad m => ParserT String m PseudoClass
pseudoClass =
  let
    basic =
      let
        alts =
          [ "active"
          , "checked"
          , "disabled"
          , "empty"
          , "enabled"
          , "first-child"
          , "first-of-type"
          , "focus-within"
          , "focus"
          , "hover"
          , "in-range"
          , "invalid"
          , "last-child"
          , "last-of-type"
          , "link"
          , "only-of-type"
          , "only-child"
          , "optional"
          , "out-of-range"
          , "read-only"
          , "read-write"
          , "required"
          , "root"
          , "target"
          , "valid"
          , "visited"
          ]
      in
        F.oneOf (string <$> alts) `withErrorMessage` altError alts

    lang =
      let
        letter = satisfy isLetter
      in
        do
          _ <- string "lang(" <* lookAhead letter
          x <- S.fromCharArray <$> A.some (letter <|> char '-' <* lookAhead letter)
          _ <- char ')'
          pure $ "lang(" <> x <> ")"

    nth =
      let
        types = [ "child", "last-child", "last-of-type", "of-type" ]

        formula =
          flip withErrorMessage "even, odd or valid formula"
            $ string "even"
            <|> string "odd"
            <|> do
                neg <- (const true <$> char '-') <|> pure false
                f <-
                  try do
                    a <- A.many $ satisfy isDigit
                    n <- char 'n'
                    plus <- P.oneOf [ '+', '-' ]
                    b <- A.some $ satisfy isDigit
                    pure $ S.fromCharArray $ a <> [ 'n' ] <> [ plus ] <> b
                    <|> try do
                        a <- A.many $ satisfy isDigit
                        n <- char 'n'
                        pure $ S.fromCharArray a <> "n"
                    <|> (S.fromCharArray <$> A.some (satisfy isDigit))
                pure $ (if neg then "-" else "") <> f
      in
        do
          _ <- string "nth-"
          x <- F.oneOf (string <$> types) `withErrorMessage` altError types
          _ <- char '('
          f <- formula
          _ <- char ')'
          pure $ "nth-" <> x <> "(" <> f <> ")"
  in
    do
      _ <- string ":not("
      classNames <- A.many className
      pseudoClasses <- A.many pseudoClass
      if A.null classNames && A.null pseudoClasses then
        fail "class or pseudo-class"
      else do
        _ <- char ')'
        pure $ PseudoClassNot { classNames, pseudoClasses }
      <|> do
          _ <- char ':'
          PseudoClass <$> (nth <|> lang <|> basic)

pseudoElement :: forall m. Monad m => ParserT String m PseudoElement
pseudoElement =
  let
    standard =
      let
        alts =
          [ "after"
          , "before"
          , "first-letter"
          , "first-line"
          , "marker"
          , "placeholder"
          , "selection"
          ]
      in
        F.oneOf (string <$> alts) `withErrorMessage` altError alts

    prefixed =
      let
        vendors = [ "ms", "moz", "o", "webkit" ]
      in
        do
          _ <- char '-'
          prefix <- F.oneOf (string <$> vendors) `withErrorMessage` altError vendors
          _ <- char '-'
          element <-
            (:) <$> lower
              <*> L.many (lower <|> char '-' <* lookAhead lower)
          pure $ "-" <> prefix <> "-" <> (S.fromCharArray $ A.fromFoldable element)
  in
    do
      _ <- replicateM 2 $ char ':'
      PseudoElement <$> (standard <|> prefixed)

combinator :: forall m. Monad m => ParserT String m Combinator
combinator = combinator' <?> "combinator"
  where
  combinator' = ancestor <|> parent <|> adjacentSibling <|> generalSibling

  ancestor = const Ancestor <$> char '_'

  parent = const Parent <$> char '>'

  adjacentSibling = const AdjacentSibling <$> char '+'

  generalSibling = const GeneralSibling <$> char '~'

atScope ::
  forall r m.
  MonadAsk { knownAtScopes :: Array AtScope | r } m =>
  ParserT String m AtScope
atScope = do
  _ <- char '@'
  known <- lift $ asks $ map (un AtScope) <<< _.knownAtScopes
  AtScope <$> F.oneOf (string <$> known) `withErrorMessage` altError known

property ::
  forall r m.
  MonadAsk { knownProperties :: Array Property | r } m =>
  ParserT String m Property
property = do
  known <- lift $ asks $ A.reverse <<< A.sortWith S.length <<< map (un Property) <<< _.knownProperties
  Property <$> F.oneOf (string <$> known) `withErrorMessage` altError known

value ::
  forall r m.
  MonadAsk { knownVariables :: Array Variable | r } m =>
  ParserT String m Value
value = Value <$> A.some (calc <|> url <|> simple)
  where
  whitespaceChars = [ ' ', '\t', '\r', '\n' ]

  var = do
    knownVariableNames <- lift $ asks $ map variableName <<< _.knownVariables
    v <- char '$' *> (F.oneOf $ string <$> knownVariableNames)
    pure $ Var $ Variable v

  dollar = do
    knownVariableNames <- lift $ asks $ map variableName <<< _.knownVariables
    char '$' <* notFollowedBy (F.oneOf $ string <$> knownVariableNames)

  calc = Calc <<< A.reverse <<< A.fromFoldable <$> (string "calc(" *> _calc 1 Nil Nil)
    where
    _calc i acc litAcc =
      fix \_ ->
        try do
          v <- var
          let
            acc' = case litAcc of
              Nil -> v : acc
              x -> v : (Lit $ S.fromCharArray $ A.reverse $ A.fromFoldable $ x) : acc
          _calc i acc' Nil
          <|> do
              x <- (noneOf $ [ '$', ';', '}' ] <> whitespaceChars) <|> dollar
              case x of
                '(' -> _calc (i + 1) acc $ x : litAcc
                ')'
                  | i > 1 -> _calc (i - 1) acc $ x : litAcc
                ')'
                  | otherwise -> pure $ (Lit $ S.fromCharArray $ A.reverse $ A.fromFoldable $ litAcc) : acc
                '+' -> _calc i acc $ ' ' : '+' : ' ' : litAcc
                '-' -> _calc i acc $ ' ' : '-' : ' ' : litAcc
                '*' -> _calc i acc $ ' ' : '*' : ' ' : litAcc
                '/' -> _calc i acc $ ' ' : '/' : ' ' : litAcc
                _ -> _calc i acc $ x : litAcc
          <|> pure acc

  url = URL <$> (quotedURL <|> unquotedURL)
    where
    quotedURL = do
      _ <- string "url('"
      x <-
        A.some
          ( try var
              <|> ( Lit <<< S.fromCharArray
                    <$> A.some
                        ( try dollar
                            <|> (const ' ' <$> string "__")
                            <|> noneOf ([ '\'', '$' ] <> whitespaceChars)
                        )
                )
          )
      _ <- string "')"
      pure $ Quoted x

    unquotedURL = do
      _ <- string "url("
      x <-
        A.some
          ( try var
              <|> ( Lit <<< S.fromCharArray
                    <$> A.some
                        ( try dollar
                            <|> (const ' ' <$> string "__")
                            <|> noneOf ([ '\'', ')', '}', ';', '$' ] <> whitespaceChars)
                        )
                )
          )
      _ <- char ')'
      pure $ Unquoted x

  simple = Simple <$> (quotedSimple <|> unquotedSimple)
    where
    quotedSimple = do
      _ <- char '\''
      x <-
        A.some
          ( try var
              <|> ( Lit <<< S.fromCharArray
                    <$> A.some
                        ( try dollar
                            <|> (const ' ' <$> string "__")
                            <|> noneOf ([ '\'', '$' ] <> whitespaceChars)
                        )
                )
          )
      _ <- char '\''
      pure $ Quoted x

    unquotedSimple = do
      x <-
        A.some
          ( try var
              <|> ( Lit <<< S.fromCharArray
                    <$> A.some
                        ( try dollar
                            <|> (const ' ' <$> string "__")
                            <|> noneOf ([ '\'', ')', '}', ';', '$', 'c', 'u' ] <> whitespaceChars)
                            <|> do
                                _ <- lookAhead (char 'c') <* notFollowedBy calc
                                char 'c'
                            <|> do
                                _ <- lookAhead (char 'u') <* notFollowedBy url
                                char 'u'
                        )
                )
          )
      pure $ Unquoted x

declaration ::
  forall r m.
  MonadAsk
    { knownProperties :: Array Property
    , knownVariables :: Array Variable
    | r
    }
    m =>
  ParserT String m Declaration
declaration = do
  p <- property
  _ <- char ':'
  v <- value
  pure $ Declaration $ Tuple p v

context ::
  forall m.
  Monad m =>
  ParserT String m Context
context = do
  classNames <- A.many className
  pseudoClasses <- A.many pseudoClass
  if A.null classNames && A.null pseudoClasses then
    fail "class or pseudo-class"
  else do
    combinator' <- combinator
    pure { classNames, pseudoClasses, combinator: combinator' }

selector ::
  forall m.
  Monad m =>
  ParserT String m Selector
selector = do
  ctx <- optionMaybe $ try context
  classNames <- A.many className
  pseudoClasses <- A.many $ try pseudoClass
  pe <- optionMaybe pseudoElement
  pure { context: ctx, classNames, pseudoClasses, pseudoElement: pe }

rule ::
  forall r m.
  MonadAsk
    { knownAtScopes :: Array AtScope
    , knownProperties :: Array Property
    , knownVariables :: Array Variable
    | r
    }
    m =>
  ParserT String m Rule
rule = do
  r <- try full <|> withAtScope <|> withSelector <|> onlyDeclarations
  importance <- A.length <$> (A.many $ char '!')
  pure
    { atScope: r.atScope
    , selector: r.selector
    , declarations: r.declarations
    , importance
    }
  where
  onlyDeclarations = do
    declarations <- A.some $ declaration <* char ';'
    pure { atScope: Nothing, selector: Nothing, declarations }

  full = do
    atScope' <- atScope
    _ <- char '{'
    selector' <- selector
    _ <- char '{'
    declarations <- A.fromFoldable <$> declaration `sepBy1` char ';'
    _ <- string "}}"
    pure { atScope: Just atScope', selector: Just selector', declarations }

  withAtScope = do
    atScope' <- atScope
    _ <- char '{'
    declarations <- A.fromFoldable <$> declaration `sepBy1` char ';'
    _ <- char '}'
    pure { atScope: Just atScope', selector: Nothing, declarations }

  withSelector = do
    selector' <- selector
    _ <- char '{'
    declarations <- A.fromFoldable <$> declaration `sepBy1` char ';'
    _ <- char '}'
    pure { atScope: Nothing, selector: Just selector', declarations }

rules ::
  forall r m.
  MonadRec m =>
  MonadAsk
    { knownAtScopes :: Array AtScope
    , knownProperties :: Array Property
    , knownVariables :: Array Variable
    | r
    }
    m =>
  ParserT String m (Array Rule)
rules = A.reverse <<< A.fromFoldable <<< L.nub <$> whileJust rules'
  where
  rules' =
    (eof *> pure Nothing)
      <|> ( do
            r <- try rule
            pure $ Just (r : Nil)
        )
      <|> (anyChar *> pure (Just Nil))

lower :: forall m. Monad m => ParserT String m Char
lower = satisfy isLower

altError ::
  forall f a.
  Foldable f =>
  Functor f =>
  Show a =>
  f a ->
  String
altError = ("one of: " <> _) <<< S.joinWith ", " <<< A.fromFoldable <<< map show
