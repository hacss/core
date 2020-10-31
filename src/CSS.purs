module Hacss.CSS where

import Prelude
import Control.Monad.Reader (runReader)
import Data.Array (cons, fromFoldable, replicate) as A
import Data.Either (fromRight)
import Data.Foldable (foldl)
import Data.Map (Map, keys, lookup)
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.Newtype (un)
import Data.String.Common (joinWith) as S
import Data.Tuple (Tuple(..))
import Global (encodeURIComponent)
import Partial.Unsafe (unsafePartial)
import Text.Parsing.Parser (runParserT)
import Hacss.Data
  ( AtScope
  , Context
  , Declaration(..)
  , Property(..)
  , QualifiedVal(..)
  , Rule
  , Selector
  , ValExpr(..)
  , ValSeg(..)
  , Value(..)
  , Variable
  )
import Hacss.Parser (rules)
import Hacss.Printer (className, combinator, pseudoClass, pseudoElement, rule)

type Config r
  = { atScopes :: Map AtScope String
    , knownProperties :: Array Property
    , variables :: Map Variable String
    | r
    }

type Code
  = String

type CSS
  = String

mkCSS :: forall r. Config r -> Code -> CSS
mkCSS { atScopes, knownProperties, variables } code =
  let
    rules' =
      unsafePartial $ fromRight
        $ runReader
            (runParserT code rules)
            { knownAtScopes: A.fromFoldable $ keys atScopes
            , knownProperties
            , knownVariables: A.fromFoldable $ keys variables
            }
  in
    S.joinWith "\n" $ css <$> rules'
  where
  css :: Rule -> CSS
  css r =
    let
      selBase =
        let
          b = "." <> (cssEscape $ rule r)
        in
          foldl (<>) b $ A.replicate r.importance b
    in
      (S.joinWith ";" $ declaration <$> r.declarations)
        # case r.selector of
            Nothing -> wrap selBase
            Just s -> wrap $ selector selBase s
        # case r.atScope of
            Nothing -> identity
            Just s -> wrap $ atScope s

  wrap :: CSS -> CSS -> CSS
  wrap outer inner = outer <> "{" <> inner <> "}"

  atScope :: AtScope -> CSS
  atScope a = "@" <> unsafePartial (fromJust $ lookup a atScopes)

  selector :: CSS -> Selector -> CSS
  selector x s =
    (fromMaybe "" $ context <$> s.context)
      <> (S.joinWith "" $ A.cons x $ className <$> s.classNames)
      <> (S.joinWith "" $ pseudoClass <$> s.pseudoClasses)
      <> (fromMaybe "" $ pseudoElement <$> s.pseudoElement)

  context :: Context -> CSS
  context c =
    (S.joinWith "" $ className <$> c.classNames)
      <> (S.joinWith "" $ pseudoClass <$> c.pseudoClasses)
      <> " "
      <> combinator c.combinator
      <> " "

  declaration :: Declaration -> CSS
  declaration (Declaration (Tuple (Property p) v)) = p <> ":" <> value v

  value :: Value -> CSS
  value = foldl (<>) mempty <<< map valSeg <<< un Value

  valSeg :: ValSeg -> CSS
  valSeg = case _ of
    Simple (Quoted exprs) -> "'" <> (foldl (<>) mempty $ valExpr identity <$> exprs) <> "'"
    Simple (Unquoted exprs) -> foldl (<>) mempty $ valExpr identity <$> exprs
    Calc exprs -> "calc(" <> (foldl (<>) mempty $ valExpr identity <$> exprs) <> ")"
    URL (Quoted exprs) ->
      "url('"
        <> (foldl (<>) mempty $ valExpr (\x -> fromMaybe x $ encodeURIComponent x) <$> exprs)
        <> "')"
    URL (Unquoted exprs) ->
      "url("
        <> (foldl (<>) mempty $ valExpr (\x -> fromMaybe x $ encodeURIComponent x) <$> exprs)
        <> ")"

  valExpr :: (String -> String) -> ValExpr -> CSS
  valExpr processVarValue = case _ of
    Lit x -> x
    Var v -> processVarValue $ unsafePartial $ fromJust $ lookup v variables

foreign import cssEscape :: String -> String
