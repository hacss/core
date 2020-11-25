module Hacss.Internal.Renderer (CSS, RenderError(..), printRenderError, render) where

import Prelude
import Data.Array (replicate)
import Data.Either (Either(..), note)
import Data.Foldable (foldMap)
import Data.Traversable (traverse)
import Data.Lens ((^.))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (un)
import Data.String.Common (joinWith) as S
import Data.Tuple (Tuple(..))
import Global (encodeURIComponent)
import Hacss.Internal.Data (AtScope, Declaration(..), Priority(..), Property(..), Rule, ValCtx(..), ValExpr(..), Value(..), Variable(..), ruleAtScope, ruleDeclarations, rulePriority, ruleSelector, selectorClasses, selectorContext, selectorPseudoElement)
import Hacss.Internal.Printer as Print

type Resolve t
  = t -> Maybe String

data RenderError
  = UnresolvedAtScope AtScope
  | UnresolvedVariable Variable
  | UnencodableVariable Variable

printRenderError :: RenderError -> String
printRenderError = case _ of
  UnresolvedAtScope a -> "Unresolved at-scope \"" <> Print.atScope a <> "\""
  UnresolvedVariable (Variable v) -> "Unresolved variable \"" <> v <> "\""
  UnencodableVariable (Variable v) -> "Variable \"" <> v <> "\" contains characters that cannot be URL-encoded."

instance eqRenderError :: Eq RenderError where
  eq (UnresolvedAtScope a) (UnresolvedAtScope b) = a == b
  eq (UnresolvedVariable a) (UnresolvedVariable b) = a == b
  eq (UnencodableVariable a) (UnencodableVariable b) = a == b
  eq _ _ = false

instance showRenderError :: Show RenderError where
  show (UnresolvedAtScope x) = "(UnresolvedAtScope " <> show x <> ")"
  show (UnresolvedVariable x) = "(UnresolvedVariable " <> show x <> ")"
  show (UnencodableVariable x) = "(UnencodableVariable " <> show x <> ")"

type CSS
  = String

render :: Resolve AtScope -> Resolve Variable -> Rule -> Either RenderError CSS
render resolveAtScope resolveVariable r =
  render' <$> atScope'
    <*> selector'
    <*> declarations'
  where
  atScope' :: Either RenderError (Maybe CSS)
  atScope' = case r ^. ruleAtScope of
    Nothing -> pure Nothing
    Just a -> case resolveAtScope a of
      Nothing -> Left (UnresolvedAtScope a)
      Just b -> pure (Just b)

  render' = case _ of
    Nothing -> \sel dec -> sel <> "{" <> dec <> "}"
    Just ats -> \sel dec -> "@" <> ats <> "{" <> render' Nothing sel dec <> "}"

  selector' :: Either RenderError CSS
  selector' =
    let
      ctx = fromMaybe mempty $ Print.context <$> ((r ^. ruleSelector) >>= (_ ^. selectorContext))

      base = S.joinWith "" $ replicate (un Priority (r ^. rulePriority) + 1) $ "." <> cssEscape (Print.rule r)

      classes = Print.classes $ fromMaybe mempty $ (_ ^. selectorClasses) <$> (r ^. ruleSelector)

      pseudoEl = fromMaybe mempty $ Print.pseudoElement <$> ((r ^. ruleSelector) >>= (_ ^. selectorPseudoElement))
    in
      pure $ ctx <> base <> classes <> pseudoEl

  declarations' :: Either RenderError CSS
  declarations' =
    foldMap (_ <> ";")
      <$> traverse
          ( \(Declaration (Tuple (Property p) (Value valueInner))) ->
              (\v -> p <> ":" <> v)
                <$> ( case valueInner of
                      Left v -> note (UnresolvedVariable v) $ resolveVariable v
                      Right vs ->
                        S.joinWith ""
                          <$> traverse
                              ( \(Tuple ctx exprs) ->
                                  let
                                    default =
                                      S.joinWith ""
                                        <$> traverse
                                            ( case _ of
                                                Lit x -> pure x
                                                Var v -> note (UnresolvedVariable v) $ resolveVariable v
                                            )
                                            exprs
                                  in
                                    case ctx of
                                      Just URL ->
                                        (\x -> "url('" <> S.joinWith "" x <> "')")
                                          <$> traverse
                                              ( case _ of
                                                  Lit x -> pure x
                                                  Var v -> note (UnresolvedVariable v) (resolveVariable v) >>= encodeURIComponent >>> note (UnencodableVariable v)
                                              )
                                              exprs
                                      Just Calc -> (\x -> "calc(" <> x <> ")") <$> default
                                      Just Quoted -> (\x -> "'" <> x <> "'") <$> default
                                      Nothing -> default
                              )
                              vs
                  )
          )
          (r ^. ruleDeclarations)

foreign import cssEscape :: String -> String
