module Hacss.Printer where

import Prelude
import Data.Array (replicate) as A
import Data.Foldable (foldl)
import Data.Maybe (fromMaybe, isNothing)
import Data.Newtype (un)
import Data.String (replaceAll)
import Data.String.CodeUnits (fromCharArray) as S
import Data.String.Common (joinWith) as S
import Data.String.Pattern (Pattern(..), Replacement(..))
import Data.Tuple (Tuple(..))
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
  , ValExpr(..)
  , ValSeg(..)
  , Value(..)
  , Variable(..)
  )

className :: ClassName -> String
className = ("." <> _) <<< un ClassName

pseudoClass :: PseudoClass -> String
pseudoClass =
  case _ of
    PseudoClassNot ({ classNames, pseudoClasses }) ->
      ":not("
      <> foldl (<>) mempty ((className <$> classNames) <> (pseudoClass <$> pseudoClasses))
      <> ")"
    PseudoClass p ->
      ":" <> p

pseudoElement :: PseudoElement -> String
pseudoElement = ("::" <> _) <<< un PseudoElement

combinator :: Combinator -> String
combinator = case _ of
  Ancestor -> "_"
  Parent -> ">"
  AdjacentSibling -> "+"
  GeneralSibling -> "~"

atScope :: AtScope -> String
atScope = ("@" <> _) <<< un AtScope

property :: Property -> String
property = un Property

value :: Value -> String
value = foldl (<>) mempty <<< map valSeg <<< un Value
  where
    valExprs = foldl (<>) mempty <<< map case _ of
                                           Lit x ->
                                             replaceAll
                                               (Pattern " ")
                                               (Replacement "__")
                                               x
                                           Var (Variable v) ->
                                             "$" <> v
    qualifiedVal = case _ of
      Unquoted x -> valExprs x
      Quoted x -> "'" <> valExprs x <> "'"
    valSeg = case _ of
      Simple qv ->
        qualifiedVal qv
      URL qv ->
        "url(" <> qualifiedVal qv <> ")"
      Calc ve ->
        let
          removeSpaces =
            case _ of
              Lit x -> Lit $ replaceAll (Pattern " ") (Replacement "") x
              x -> x
        in
          "calc(" <> valExprs (removeSpaces <$> ve) <> ")"

declaration :: Declaration -> String
declaration (Declaration (Tuple p v)) = property p <> ":" <> value v

context :: Context -> String
context { classNames, pseudoClasses, combinator: c } =
  foldl (<>) mempty $
    (className <$> classNames)
    <> (pseudoClass <$> pseudoClasses)
    <> [combinator c]

selector :: Selector -> String
selector s =
  foldl (\a b -> a <> context b) mempty s.context
  # flip (foldl (\a b -> a <> className b)) s.classNames
  # flip (foldl (\a b -> a <> pseudoClass b)) s.pseudoClasses
  # flip (foldl (\a b -> a <> pseudoElement b)) s.pseudoElement

rule :: Rule -> String
rule r =
  if isNothing r.atScope && isNothing r.selector
    then
      foldl (\a b -> a <> declaration b <> ";") mempty r.declarations
      <> S.fromCharArray (A.replicate r.importance '!')
    else
      let
        decls = S.joinWith ";" $ declaration <$> r.declarations
        sel = fromMaybe identity $ (\s -> \d -> selector s <> "{" <> d <> "}") <$> r.selector
        at = fromMaybe identity $ (\a -> \x -> atScope a <> "{" <> x <> "}") <$> r.atScope
      in
        (at $ sel $ decls) <> S.fromCharArray (A.replicate r.importance '!')
