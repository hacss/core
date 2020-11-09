module Hacss.Renderer where

import Prelude
import Data.Array (replicate)
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (un)
import Data.String.Common (joinWith) as S
import Data.Tuple (Tuple(..))
import Global (encodeURIComponent)
import Hacss.Data (AtScope, Rule, Selector(..), ValContext(..), ValExpr(..), Value(..), Variable)
import Hacss.Printer as Print

type Resolve t
  = t -> Maybe String

render :: Resolve AtScope -> Resolve Variable -> Rule -> String
render resolveAtScope resolveVariable r =
  ( r.declarations
      <#> \(Tuple p v) ->
          let
            val (Tuple vctx exprs) =
              exprs
                # foldMap case _ of
                    Lit x -> x
                    Var x ->
                      fromMaybe (Print.variable x)
                        $ case vctx of
                            Just URL -> resolveVariable x >>= encodeURIComponent
                            _ -> resolveVariable x
                # case vctx of
                    Nothing -> identity
                    Just Quoted -> \x -> "'" <> x <> "'"
                    Just URL -> \x -> "url('" <> x <> "')"
                    Just Calc -> \x -> "calc(" <> x <> ")"
          in
            Print.property p <> ":" <> (foldMap val $ un Value v)
  )
    # S.joinWith ";"
    # \decls ->
        let
          ctx = foldMap Print.context $ r.selector >>= (\(Selector { context }) -> context)

          main = S.joinWith "" $ replicate (r.priority + 1) $ "." <> cssEscape (Print.rule r)

          cls = foldMap Print.classList $ r.selector >>= (\(Selector { classes }) -> classes)

          pe = foldMap Print.pseudoElement $ r.selector >>= (\(Selector { pseudoElement }) -> pseudoElement)
        in
          ctx <> main <> cls <> pe <> "{" <> decls <> "}"
            # case r.atScope >>= resolveAtScope of
                Nothing -> identity
                Just scope -> \x -> "@" <> scope <> "{" <> x <> "}"

foreign import cssEscape :: String -> String
