module Hacss (hacss) where

import Prelude
import Data.Array (nub)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Function.Uncurried (Fn2, mkFn2)
import Data.Map (empty, insert) as Map
import Data.Maybe (fromMaybe)
import Data.Nullable (Nullable, null)
import Data.Nullable (toMaybe) as Null
import Foreign.Object (Object)
import Hacss.Data (AtScope(..), Property(..), Variable(..))
import Hacss.CSS (CSS, mkCSS)

type Code = String

type Config =
  { atScopes :: Nullable (Object String)
  , knownProperties :: Nullable (Array String)
  , variables :: Nullable (Object String)
  }

hacss :: Fn2 Code (Nullable Config) CSS
hacss = mkFn2 \code nullableConfig ->
  let
    emptyConfig = { atScopes: null, knownProperties: null, variables: null }
    config = fromMaybe emptyConfig $ Null.toMaybe nullableConfig
    atScopes = fromMaybe mempty $ Null.toMaybe config.atScopes
    knownProperties = fromMaybe mempty $ Null.toMaybe config.knownProperties
    variables = fromMaybe mempty $ Null.toMaybe config.variables
  in
    mkCSS
      { atScopes: foldlWithIndex (\i b a -> b # Map.insert (AtScope i) a) Map.empty atScopes
      , knownProperties: Property <$> (nub $ _knownProperties <> knownProperties)
      , variables: foldlWithIndex (\i b a -> b # Map.insert (Variable i) a) Map.empty variables
      }
      code

foreign import _knownProperties :: Array String
