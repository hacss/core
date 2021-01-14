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
import Hacss.Internal.Data (AtScope(..), Combinator(..), Context(..), Declaration(..), Priority(..), Property(..), Rule, ValCtx(..), ValExpr(..), Value(..), Variable(..), ruleAtScope, ruleDeclarations, rulePriority, ruleSelector, selectorClasses, selectorContext, selectorPseudoElement)
import Hacss.Internal.Printer as Print

type Resolve t
  = t -> Maybe String

data RenderError
  = UnresolvedAtScope AtScope
  | UnresolvedVariable Property Variable
  | UnencodableVariable Property Variable

printRenderError :: RenderError -> String
printRenderError = case _ of
  UnresolvedAtScope (AtScope a) -> "Unresolved at-scope \"" <> a <> "\""
  UnresolvedVariable (Property p) (Variable v) -> "Unresolved variable \"" <> v <> "\" on property \"" <> p <> "\""
  UnencodableVariable (Property p) (Variable v) -> "Variable \"" <> v <> "\" on property \"" <> p <> "\" contains characters that cannot be URL-encoded."

instance eqRenderError :: Eq RenderError where
  eq (UnresolvedAtScope a) (UnresolvedAtScope b) = a == b
  eq (UnresolvedVariable p v) (UnresolvedVariable p' v') = p == p' && v == v'
  eq (UnencodableVariable p v) (UnencodableVariable p' v') = p == p' && v == v'
  eq _ _ = false

instance showRenderError :: Show RenderError where
  show (UnresolvedAtScope x) = "(UnresolvedAtScope " <> show x <> ")"
  show (UnresolvedVariable p v) = "(UnresolvedVariable " <> show p <> " " <> show v <> ")"
  show (UnencodableVariable p v) = "(UnencodableVariable " <> show p <> " " <> show v <> ")"

type CSS
  = String

render :: (Property -> Variable -> Maybe String) -> (AtScope -> Maybe String) -> Rule -> Either RenderError CSS
render resolveVariable resolveAtScope r =
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
      ctx =
        let
          ctx' (Context (Tuple a b)) =
            Print.classes a
              <> ( case b of
                    AdjSib -> "+"
                    Ancestor -> " "
                    GenSib -> "~"
                    Parent -> ">"
                )
        in
          fromMaybe mempty $ ctx' <$> ((r ^. ruleSelector) >>= (_ ^. selectorContext))

      base = "." <> cssEscape (Print.rule r) <> (S.joinWith "" $ replicate (un Priority $ r ^. rulePriority) $ ":not(.\\*)")

      classes = Print.classes $ fromMaybe mempty $ (_ ^. selectorClasses) <$> (r ^. ruleSelector)

      pseudoEl = fromMaybe mempty $ Print.pseudoElement <$> ((r ^. ruleSelector) >>= (_ ^. selectorPseudoElement))
    in
      pure $ ctx <> base <> classes <> pseudoEl

  declarations' :: Either RenderError CSS
  declarations' =
    foldMap (_ <> ";")
      <$> traverse
          ( \(Declaration (Tuple property@(Property p) (Value valueInner))) ->
              (\v -> p <> ":" <> v)
                <$> ( case valueInner of
                      Left v -> note (UnresolvedVariable property v) $ resolveVariable property v
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
                                                Var v -> note (UnresolvedVariable property v) $ resolveVariable property v
                                            )
                                            exprs
                                  in
                                    case ctx of
                                      Just URL ->
                                        (\x -> "url('" <> S.joinWith "" x <> "')")
                                          <$> traverse
                                              ( case _ of
                                                  Lit x -> pure x
                                                  Var v -> note (UnresolvedVariable property v) (resolveVariable property v) >>= encodeURIComponent >>> note (UnencodableVariable property v)
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
