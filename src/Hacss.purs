module Hacss (HacssError, hacss) where

import Prelude
import Data.Bifunctor (lmap)
import Data.Either (Either, either)
import Data.Function.Uncurried (Fn2, mkFn2)
import Data.Maybe (Maybe, fromMaybe)
import Data.Newtype (un)
import Data.Nullable (Nullable, null)
import Data.Nullable (toMaybe) as Null
import Data.String.Common (joinWith)
import Data.Traversable (traverse)
import Effect.Exception (throw)
import Effect.Unsafe (unsafePerformEffect)
import Foreign.Object (Object, lookup)
import Text.Parsing.StringParser (ParseError, runParser)
import Hacss.Data (AtScope(..), Variable(..))
import Hacss.Parser (rules) as Parse
import Hacss.Renderer (CSS, RenderError, printRenderError, render)

type Code
  = String

type Config
  = { atScopes :: Nullable (Object String)
    , variables :: Nullable (Object String)
    }

data HacssError
  = ParseFailure ParseError
  | RenderFailure RenderError

printHacssError :: HacssError -> String
printHacssError = case _ of
  ParseFailure e -> show e
  RenderFailure e -> printRenderError e

hacss :: (AtScope -> Maybe String) -> (Variable -> Maybe String) -> Code -> Either HacssError CSS
hacss resolveAtScope resolveVariable code =
  joinWith ""
    <$> ( (lmap ParseFailure $ runParser Parse.rules code)
          >>= (lmap RenderFailure <<< traverse (render resolveAtScope resolveVariable))
      )

unsafeForeignHacss :: Fn2 Code (Nullable Config) CSS
unsafeForeignHacss =
  mkFn2 \code nullableConfig ->
    let
      emptyConfig = { atScopes: null, variables: null }

      config = fromMaybe emptyConfig $ Null.toMaybe nullableConfig

      atScopes = fromMaybe mempty $ Null.toMaybe config.atScopes

      variables = fromMaybe mempty $ Null.toMaybe config.variables
    in
      hacss
        (flip lookup atScopes <<< un AtScope)
        (flip lookup variables <<< un Variable)
        code
        # either
            (printHacssError >>> throw >>> unsafePerformEffect)
            identity
