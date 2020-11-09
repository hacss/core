module Hacss.Printer where

import Prelude
import Data.Array (replicate)
import Data.Foldable (foldl, foldMap)
import Data.Maybe (Maybe(..), isNothing)
import Data.Newtype (un)
import Data.String.CodeUnits (fromCharArray) as S
import Data.String.Common (joinWith, replaceAll)
import Data.String.Pattern (Pattern(..), Replacement(..))
import Data.Tuple (Tuple(..))
import Hacss.Data (AtScope(..), ClassList(..), ClassName(..), Combinator(..), Context(..), Property(..), PseudoClass(..), PseudoElement(..), Rule, Selector(..), ValContext(..), ValExpr(..), Value(..), Variable(..))

variable :: Variable -> String
variable = ("$" <> _) <<< un Variable

value :: Value -> String
value = foldMap valSeg <<< un Value
  where
  valSeg :: Tuple (Maybe ValContext) (Array ValExpr) -> String
  valSeg (Tuple ctx exprs) =
    let
      val =
        exprs
          # foldMap case _ of
              Lit x -> x
              Var x -> variable x

      spaces = replaceAll (Pattern " ") (Replacement "__")

      ops = foldl compose identity $ (\o -> replaceAll (Pattern $ " " <> o <> " ") $ Replacement o) <$> [ "+", "-", "*", "/" ]
    in
      case ctx of
        Nothing -> spaces val
        Just Quoted -> "'" <> spaces val <> "'"
        Just URL -> "url('" <> spaces val <> "')"
        Just Calc -> "calc(" <> ops val <> ")"

property :: Property -> String
property = un Property

className :: ClassName -> String
className (ClassName c) = "." <> c

classList :: ClassList -> String
classList (ClassList (Tuple names pseudos)) = foldMap className names <> foldMap pseudoClass pseudos

pseudoClass :: PseudoClass -> String
pseudoClass = case _ of
  BasicPseudoClass x -> ":" <> x
  NotPseudoClass x -> ":not(" <> classList x <> ")"

pseudoElement :: PseudoElement -> String
pseudoElement (PseudoElement x) = "::" <> x

combinator :: Combinator -> String
combinator = case _ of
  Ancestor -> "_"
  Parent -> ">"
  AdjacentSibling -> "+"
  GeneralSibling -> "~"

context :: Context -> String
context (Context (Tuple classes comb)) = classList classes <> combinator comb

selector :: Selector -> String
selector (Selector s) = foldMap context s.context <> foldMap classList s.classes <> foldMap pseudoElement s.pseudoElement

atScope :: AtScope -> String
atScope (AtScope x) = "@" <> x

priority :: Int -> String
priority n = S.fromCharArray $ replicate n '!'

declaration :: Tuple Property Value -> String
declaration (Tuple p v) = property p <> ":" <> value v

rule :: Rule -> String
rule r = rule' <> priority r.priority
  where
  rule' =
    if isNothing r.atScope && isNothing r.selector then
      foldMap ((_ <> ";") <<< declaration) r.declarations
    else
      let
        declarations = joinWith ";" $ declaration <$> r.declarations
      in
        declarations
          # wrap (selector <$> r.selector)
          # wrap (atScope <$> r.atScope)
    where
    wrap = case _ of
      Nothing -> identity
      Just o -> \i -> o <> "{" <> i <> "}"
