module Hacss (hacss) where

import Prelude
import Data.Array (nub)
import Data.Either (either)
import Data.Function.Uncurried (Fn2, mkFn2)
import Data.Maybe (fromMaybe)
import Data.Newtype (un)
import Data.Nullable (Nullable, null)
import Data.Nullable (toMaybe) as Null
import Data.String.Common (joinWith)
import Foreign.Object (Object, keys, lookup)
import Hacss.Data (AtScope(..), Property(..), Variable(..))
import Hacss.Parser (runParseM)
import Hacss.Parser (rules) as Parse
import Hacss.Renderer (render)

type Code
  = String

type Config
  = { atScopes :: Nullable (Object String)
    , knownProperties :: Nullable (Array String)
    , variables :: Nullable (Object String)
    }

type CSS
  = String

hacss :: Fn2 Code (Nullable Config) CSS
hacss =
  mkFn2 \code nullableConfig ->
    let
      emptyConfig = { atScopes: null, knownProperties: null, variables: null }

      config = fromMaybe emptyConfig $ Null.toMaybe nullableConfig

      atScopes = fromMaybe mempty $ Null.toMaybe config.atScopes

      knownProperties = fromMaybe mempty $ Null.toMaybe config.knownProperties

      variables = fromMaybe mempty $ Null.toMaybe config.variables

      rules =
        runParseM
          Parse.rules
          { knownAtScopes: AtScope <$> keys atScopes
          , knownProperties: Property <$> (nub $ _knownProperties <> knownProperties)
          , knownVariables: Variable <$> keys variables
          }
          code
    in
      ( runParseM
          Parse.rules
          { knownAtScopes: AtScope <$> keys atScopes
          , knownProperties: Property <$> (nub $ _knownProperties <> knownProperties)
          , knownVariables: Variable <$> keys variables
          }
          code
      )
        # either
            (const mempty)
            (joinWith "\n" <<< map (render (flip lookup atScopes <<< un AtScope) (flip lookup variables <<< un Variable)))

foreign import _knownProperties :: Array String
