module Hacss.Printer where

import Prelude
import Data.Array (replicate)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Lens ((^.))
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.Newtype (un)
import Data.String (joinWith, replaceAll) as S
import Data.String.CodeUnits (fromCharArray) as S
import Data.String.Pattern (Pattern(..), Replacement(..))
import Data.Tuple (Tuple(..))
import Hacss.Data (AtScope(..), Class(..), Combinator(..), Context(..), Declaration(..), Priority(..), Property(..), PseudoElement(..), Rule, Selector, ValCtx(..), ValExpr(..), Value(..), Variable(..), ruleAtScope, ruleDeclarations, rulePriority, ruleSelector, selectorClasses, selectorContext, selectorPseudoElement)

atScope :: AtScope -> String
atScope = ("@" <> _) <<< un AtScope

cls :: Class -> String
cls = case _ of
  NamedClass x -> "." <> x
  PseudoClass x -> ":" <> x
  NotPseudoClass xs -> ":not(" <> classes xs <> ")"

classes :: Array Class -> String
classes xs = S.joinWith "" $ cls <$> xs

combinator :: Combinator -> String
combinator = case _ of
  Ancestor -> "_"
  Parent -> ">"
  AdjSib -> "+"
  GenSib -> "~"

context :: Context -> String
context (Context (Tuple a b)) = classes a <> combinator b

declaration :: Declaration -> String
declaration (Declaration (Tuple a b)) = property a <> ":" <> value b

priority :: Priority -> String
priority = S.fromCharArray <<< flip replicate '!' <<< un Priority

property :: Property -> String
property = un Property

pseudoElement :: PseudoElement -> String
pseudoElement = ("::" <> _) <<< un PseudoElement

rule :: Rule -> String
rule r =
  ( if isNothing (r ^. ruleAtScope) && isNothing (r ^. ruleSelector) then
      S.joinWith "" $ ((_ <> ";") <<< declaration <$> (r ^. ruleDeclarations))
    else
      (S.joinWith ";" $ declaration <$> (r ^. ruleDeclarations))
        # wrap (selector <$> (r ^. ruleSelector))
        # wrap (atScope <$> (r ^. ruleAtScope))
  )
    <> priority (r ^. rulePriority)
  where
  wrap = case _ of
    Just outer -> \inner -> outer <> "{" <> inner <> "}"
    Nothing -> identity

selector :: Selector -> String
selector x =
  let
    context' = fromMaybe "" $ context <$> (x ^. selectorContext)

    classes' = classes $ x ^. selectorClasses

    pseudoElement' = fromMaybe "" $ pseudoElement <$> (x ^. selectorPseudoElement)
  in
    context' <> classes' <> pseudoElement'

value :: Value -> String
value =
  un Value
    >>> case _ of
        Left (Variable x) -> "$" <> x
        Right xs -> S.joinWith "" $ seg <$> xs
  where
  seg (Tuple ctx exprs) = wrap <<< S.joinWith "" $ expr <$> exprs
    where
    wrap = case ctx of
      Just Calc -> \x -> "calc(" <> x <> ")"
      Just Quoted -> \x -> "'" <> x <> "'"
      Just URL -> \x -> "url('" <> x <> "')"
      _ -> identity

    expr = case ctx of
      Just Calc -> case _ of
        Lit x -> x # (foldl compose identity $ (\o -> S.replaceAll (Pattern $ " " <> o <> " ") $ Replacement o) <$> [ "+", "-", "*", "/" ])
        Var (Variable x) -> "#{$" <> x <> "}"
      _ -> case _ of
        Lit x -> S.replaceAll (Pattern " ") (Replacement "__") x
        Var (Variable x) -> "#{$" <> x <> "}"
