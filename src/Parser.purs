module Hacss.Parser where

import Prelude
import Control.Alt (class Alt, (<|>))
import Control.Alternative (class Alternative)
import Control.Lazy (class Lazy, defer, fix)
import Control.Monad.Reader.Class (class MonadAsk, ask)
import Control.Monad.Reader.Trans (ReaderT, asks, runReaderT)
import Control.Monad.Rec.Class (class MonadRec, whileJust)
import Control.Monad.Trans.Class (lift) as T
import Control.Plus (class Plus)
import Data.Array (cons, fromFoldable, length, many, reverse, some, sortWith) as A
import Data.Either (Either)
import Data.Foldable (foldl)
import Data.List (List(Nil), (:))
import Data.List (nub) as L
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing)
import Data.Newtype (class Newtype, un)
import Data.String.Common (replaceAll)
import Data.String.CodeUnits (fromCharArray, length, singleton) as S
import Data.String.Common (joinWith) as S
import Data.String.Pattern (Pattern(..), Replacement(..))
import Data.Tuple (Tuple(..))
import Text.Parsing.StringParser (ParseError, Parser, runParser)
import Text.Parsing.StringParser (fail, try) as P
import Text.Parsing.StringParser.CodeUnits (alphaNum, anyChar, anyDigit, anyLetter, char, eof, lowerCaseChar, oneOf, noneOf, regex, string) as P
import Text.Parsing.StringParser.Combinators (lookAhead, optionMaybe, withError) as P
import Hacss.Data (AtScope(..), ClassList(..), ClassName(..), Combinator(..), Context(..), Property(..), PseudoClass(..), PseudoElement(..), Rule, Selector(..), ValContext(..), ValExpr(..), Value(..), Variable(..))

whiteSpaceChars :: Array Char
whiteSpaceChars = [ ' ', '\t', '\r', '\n' ]

newtype ParseM r a
  = ParseM (ReaderT r Parser a)

derive instance newtypeParseM :: Newtype (ParseM r a) _

derive newtype instance altParseM :: Alt (ParseM r)

derive newtype instance alternativeParseM :: Alternative (ParseM r)

derive newtype instance applicativeParseM :: Applicative (ParseM r)

derive newtype instance applyParseM :: Apply (ParseM r)

derive newtype instance bindParseM :: Bind (ParseM r)

derive newtype instance functorParseM :: Functor (ParseM r)

derive newtype instance monadParseM :: Monad (ParseM r)

derive newtype instance monadAskParseM :: MonadAsk r (ParseM r)

derive newtype instance plusParseM :: Plus (ParseM r)

instance lazyParseM :: Lazy (ParseM r a) where
  defer f = do
    x <- ask
    lift $ defer \_ -> runReaderT (un ParseM (f unit)) x

derive newtype instance monadRecParseM :: MonadRec (ParseM r)

lift :: forall r a. Parser a -> ParseM r a
lift = ParseM <<< T.lift

fail :: forall r a. String -> ParseM r a
fail = lift <<< P.fail

alphaNum :: forall r. ParseM r Char
alphaNum = lift P.alphaNum

anyChar :: forall r. ParseM r Char
anyChar = lift P.anyChar

anyDigit :: forall r. ParseM r Char
anyDigit = lift P.anyDigit

anyLetter :: forall r. ParseM r Char
anyLetter = lift P.anyLetter

char :: forall r. Char -> ParseM r Char
char = lift <<< P.char

eof :: forall r. ParseM r Unit
eof = lift P.eof

lowerCaseChar :: forall r. ParseM r Char
lowerCaseChar = lift P.lowerCaseChar

noneOf :: forall r. Array Char -> ParseM r Char
noneOf = lift <<< P.noneOf

oneOf :: forall r. Array Char -> ParseM r Char
oneOf = lift <<< P.oneOf

regex :: forall r. String -> ParseM r String
regex = lift <<< P.regex

string :: forall r. String -> ParseM r String
string = lift <<< P.string

oneOfString :: forall r. Array String -> ParseM r String
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

oneOfWrapped :: forall r a. (a -> String) -> (String -> a) -> Array a -> ParseM r a
oneOfWrapped toString fromString xs = fromString <$> (oneOfString $ map toString xs)

liftCombinator :: forall r i o. (Parser i -> Parser o) -> ParseM r i -> ParseM r o
liftCombinator c p = do
  r <- ask
  lift $ c $ runReaderT (un ParseM p) r

try :: forall r a. ParseM r a -> ParseM r a
try = liftCombinator P.try

lookAhead :: forall r a. ParseM r a -> ParseM r a
lookAhead = liftCombinator P.lookAhead

optionMaybe :: forall r a. ParseM r a -> ParseM r (Maybe a)
optionMaybe = liftCombinator P.optionMaybe

withError :: forall r a. ParseM r a -> String -> ParseM r a
withError = flip $ liftCombinator <<< flip P.withError

infixl 3 withError as <?>

endBy1 :: forall r s a. ParseM r a -> ParseM r s -> ParseM r (Array a)
endBy1 x sep = A.some $ x <* sep

sepBy1 :: forall r s a. ParseM r a -> ParseM r s -> ParseM r (Array a)
sepBy1 x sep = do
  h <- x
  t <- A.many $ sep *> x
  pure $ h `A.cons` t

runParseM :: forall r a. ParseM r a -> r -> String -> Either ParseError a
runParseM (ParseM m) r = runParser $ runReaderT m r

value :: forall r. ParseM { knownVariables :: Array Variable | r } Value
value = Value <$> A.some (try (url <|> calc <|> quoted) <|> normal)
  where
  normal = do
    segments <- A.some $ (char '$' *> var) <|> lit
    pure $ Tuple Nothing segments

  lit = Lit <<< S.fromCharArray <<< A.reverse <<< A.fromFoldable <$> litChar Nil
    where
    litChar Nil = do
      notLit <- isJust <$> optionMaybe (try $ lookAhead (url <|> calc <|> quoted))
      if notLit then
        fail "literal"
      else do
        c <- space <|> (noneOf $ [ '$', ';', '}' ] <> whiteSpaceChars)
        litChar (c : Nil)

    litChar acc =
      fix \_ ->
        ( do
            c <- space <|> (noneOf $ [ '$', ';', '}' ] <> whiteSpaceChars)
            (lookAhead (try (url <|> calc <|> quoted)) *> pure (c : acc)) <|> litChar (c : acc)
        )
          <|> pure acc

  url = do
    _ <- string "url('"
    x <- A.some $ (char '$' *> var) <|> (Lit <<< S.fromCharArray <$> A.some (space <|> nonTerm))
    _ <- string "')"
    pure $ Tuple (Just URL) x
    where
    nonTerm = noneOf $ [ '$', '\'' ] <> whiteSpaceChars

  calc = do
    _ <- string "calc("
    x <- calc' 1 Nil Nil
    pure $ Tuple (Just Calc) $ A.reverse $ A.fromFoldable x
    where
    calc' n
      | n < 1 = \acc accLit -> case accLit of
        Nil -> pure acc
        _ -> pure $ calcLit accLit : acc
      | otherwise = \acc accLit ->
        fix \_ ->
          ( (char '$' *> var)
              >>= \v -> case accLit of
                  Nil -> calc' n (v : acc) Nil
                  _ -> calc' n (v : calcLit accLit : acc) Nil
          )
            <|> ( do
                  x <- alphaNum <|> oneOf [ '.', '%', '(', ')', '+', '-', '*', '/' ]
                  case x of
                    '(' -> calc' (n + 1) acc (x : accLit)
                    ')'
                      | n > 1 -> calc' (n - 1) acc (x : accLit)
                      | otherwise -> calc' (n - 1) acc accLit
                    _ -> calc' n acc (x : accLit)
              )

    calcLit = Lit <<< replaceOps <<< S.fromCharArray <<< A.reverse <<< A.fromFoldable

    replaceOps s = foldl (\x o -> replaceAll (Pattern o) (Replacement $ " " <> o <> " ") x) s [ "+", "-", "*", "/" ]

  quoted = do
    _ <- char '\''
    x <- A.some $ (char '$' *> var) <|> (Lit <<< S.fromCharArray <$> A.some ((string "__" *> pure ' ') <|> nonTerm))
    _ <- char '\''
    pure $ Tuple (Just Quoted) x
    where
    nonTerm = noneOf $ [ '$', '\'' ] <> whiteSpaceChars

  var = do
    p <- asks $ oneOfWrapped (un Variable) Variable <<< _.knownVariables
    Var <$> p

  space = string "__" *> pure ' '

property :: forall r. ParseM { knownProperties :: Array Property | r } Property
property = do
  property' <- asks $ oneOfString <<< map (un Property) <<< _.knownProperties
  Property <$> property'

declaration :: forall r. ParseM { knownProperties :: Array Property, knownVariables :: Array Variable | r } (Tuple Property Value)
declaration = do
  p <- property
  _ <- char ':'
  v <- value
  pure $ Tuple p v

className :: forall r. ParseM r ClassName
className = do
  _ <- char '.'
  h <- lowerCaseChar
  t <- A.many $ lowerCaseChar <|> try (char '-' <* lookAhead lowerCaseChar)
  pure $ ClassName $ S.fromCharArray $ A.cons h t

pseudoClass :: forall r. ParseM r PseudoClass
pseudoClass = char ':' *> (notPseudoClass <|> basicPseudoClass)
  where
  notPseudoClass = fix \_ -> NotPseudoClass <$> (string "not(" *> classList <* char ')')

  basicPseudoClass =
    BasicPseudoClass
      <$> ( ( do
              _ <- string "lang("
              h <- anyLetter
              t <- A.some (anyLetter <|> (char '-' <* lookAhead anyLetter))
              _ <- char ')'
              pure $ "lang(" <> S.fromCharArray (h `A.cons` t) <> ")"
          )
            <|> ( do
                  _ <- string "nth-"
                  t <- oneOfString [ "child", "last-child", "last-of-type", "of-type" ]
                  _ <- char '('
                  expr <-
                    string "even" <|> string "odd"
                      <|> ( do
                            a <-
                              ( do
                                  neg <- fromMaybe "" <$> (optionMaybe $ string "-")
                                  digits <- S.fromCharArray <$> A.many anyDigit
                                  pure $ neg <> digits
                              )
                                <|> (S.fromCharArray <$> A.some anyDigit)
                            nplusb <-
                              optionMaybe do
                                _ <- char 'n'
                                plus <- S.singleton <$> oneOf [ '+', '-' ]
                                b <- S.fromCharArray <$> A.some anyDigit
                                pure $ "n" <> plus <> b
                            let
                              result = a <> fromMaybe "" nplusb
                            if result == "-" then
                              fail $ "Expected a valid :nth-" <> t <> " formula."
                            else
                              pure result
                        )
                  _ <- char ')'
                  pure $ "nth-" <> t <> "(" <> expr <> ")"
              )
            <|> oneOfString
                [ "active"
                , "checked"
                , "default"
                , "defined"
                , "disabled"
                , "empty"
                , "enabled"
                , "first"
                , "first-child"
                , "first-of-type"
                , "focus"
                , "focus-within"
                , "hover"
                , "indeterminate"
                , "in-range"
                , "invalid"
                , "last-child"
                , "last-of-type"
                , "left"
                , "link"
                , "only-child"
                , "only-of-type"
                , "optional"
                , "out-of-range"
                , "picture-in-picture"
                , "read-only"
                , "read-write"
                , "required"
                , "right"
                , "root"
                , "scope"
                , "target"
                , "valid"
                , "visited"
                ]
        )

classList :: forall r. ParseM r ClassList
classList = do
  names <- A.many className
  pseudos <- A.many $ try pseudoClass
  if A.length names == 0 && A.length pseudos == 0 then
    fail "Expected a class name or pseudo-class."
  else
    pure $ ClassList $ Tuple names pseudos

pseudoElement :: forall r. ParseM r PseudoElement
pseudoElement =
  PseudoElement
    <$> ( ( do
            _ <- string "::-"
            p <- oneOfString [ "moz", "ms", "o", "webkit" ]
            _ <- char '-' <* lookAhead lowerCaseChar
            x <- A.some $ (char '-' <* lookAhead lowerCaseChar) <|> lowerCaseChar
            pure $ "-" <> p <> "-" <> S.fromCharArray x
        )
          <|> ( string "::"
                *> oneOfString
                    [ "after"
                    , "before"
                    , "first-letter"
                    , "first-line"
                    , "placeholder"
                    , "selection"
                    ]
            )
      )

combinator :: forall r. ParseM r Combinator
combinator =
  (const GeneralSibling <$> char '~')
    <|> (const AdjacentSibling <$> char '+')
    <|> (const Parent <$> char '>')
    <|> (const Ancestor <$> char '_')

context :: forall r. ParseM r Context
context = do
  cl <- classList
  c <- combinator
  pure $ Context $ Tuple cl c

selector :: forall r. ParseM r Selector
selector = do
  a <- optionMaybe $ try context
  b <- optionMaybe classList
  c <- optionMaybe pseudoElement
  if isNothing a && isNothing b && isNothing c then
    fail "Expected selector."
  else
    pure $ Selector { context: a, classes: b, pseudoElement: c }

atScope :: forall r. ParseM { knownAtScopes :: Array AtScope | r } AtScope
atScope = do
  p <- asks $ oneOfString <<< map (un AtScope) <<< _.knownAtScopes
  AtScope <$> (char '@' *> p)

priority :: forall r. ParseM r Int
priority = A.length <$> A.some (char '!')

rule :: forall r. ParseM { knownAtScopes :: Array AtScope, knownProperties :: Array Property, knownVariables :: Array Variable | r } Rule
rule = do
  core <- try full <|> withAtScope <|> withSelector <|> declarationsOnly
  p <- priority <|> pure 0
  pure
    { atScope: core.atScope
    , selector: core.selector
    , declarations: core.declarations
    , priority: p
    }
  where
  full = do
    a <- atScope
    _ <- char '{'
    s <- selector
    _ <- char '{'
    declarations <- declaration `sepBy1` char ';'
    _ <- string "}}"
    pure
      { atScope: Just a
      , selector: Just s
      , declarations
      }

  withAtScope = do
    a <- atScope
    _ <- char '{'
    declarations <- declaration `sepBy1` char ';'
    _ <- char '}'
    pure
      { atScope: Just a
      , selector: Nothing
      , declarations
      }

  withSelector = do
    s <- selector
    _ <- char '{'
    declarations <- declaration `sepBy1` char ';'
    _ <- char '}'
    pure
      { atScope: Nothing
      , selector: Just s
      , declarations
      }

  declarationsOnly = do
    declarations <- declaration `endBy1` char ';'
    pure
      { atScope: Nothing
      , selector: Nothing
      , declarations
      }

rules ::
  forall r.
  ParseM
    { knownAtScopes :: Array AtScope
    , knownProperties :: Array Property
    , knownVariables :: Array Variable
    | r
    }
    (Array Rule)
rules = A.reverse <<< A.fromFoldable <<< L.nub <$> whileJust rules'
  where
  rules' =
    (eof *> pure Nothing)
      <|> ( do
            r <- try rule
            pure $ Just (r : Nil)
        )
      <|> (anyChar *> pure (Just Nil))
