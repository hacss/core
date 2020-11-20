module Hacss.Data where

import Prelude
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Ord (genericCompare)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens', (^.), _1, _2)
import Data.Lens.Iso (Iso')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Maybe (Maybe(Nothing))
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..))

newtype AtScope
  = AtScope String

derive instance newtypeAtScope :: Newtype AtScope _

derive newtype instance eqAtScope :: Eq AtScope

derive newtype instance ordAtScope :: Ord AtScope

derive newtype instance showAtScope :: Show AtScope

data Class
  = NamedClass String
  | PseudoClass String
  | NotPseudoClass (Array Class)

instance eqClass :: Eq Class where
  eq (NamedClass a) (NamedClass b) = a == b
  eq (PseudoClass a) (PseudoClass b) = a == b
  eq (NotPseudoClass a) (NotPseudoClass b) = a == b
  eq _ _ = false

instance ordClass :: Ord Class where
  compare (NamedClass a) (NamedClass b) = compare a b
  compare (PseudoClass a) (PseudoClass b) = compare a b
  compare (NotPseudoClass a) (NotPseudoClass b) = compare a b
  compare (NamedClass _) _ = LT
  compare (PseudoClass _) (NamedClass _) = GT
  compare (PseudoClass _) (NotPseudoClass _) = LT
  compare (NotPseudoClass _) _ = GT

instance showClass :: Show Class where
  show (NamedClass x) = "(NamedClass " <> show x <> ")"
  show (PseudoClass x) = "(PseudoClass " <> show x <> ")"
  show (NotPseudoClass x) = "(NotPseudoClass " <> show x <> ")"

data Combinator
  = Ancestor
  | Parent
  | AdjSib
  | GenSib

derive instance genericCombinator :: Generic Combinator _

instance eqCombinator :: Eq Combinator where
  eq = genericEq

instance ordCombinator :: Ord Combinator where
  compare = genericCompare

instance showCombinator :: Show Combinator where
  show = genericShow

type ContextRep
  = Tuple (Array Class) Combinator

newtype Context
  = Context ContextRep

derive instance newtypeContext :: Newtype Context _

derive newtype instance ordContext :: Ord Context

derive newtype instance eqContext :: Eq Context

instance showContext :: Show Context where
  show c =
    let
      rep = _Newtype :: Iso' Context ContextRep

      classes = c ^. (rep <<< _1)

      combinator = c ^. (rep <<< _2)
    in
      "(Context "
        <> show classes
        <> " "
        <> show combinator
        <> ")"

newtype PseudoElement
  = PseudoElement String

derive instance newtypePseudoElement :: Newtype PseudoElement _

derive newtype instance eqPseudoElement :: Eq PseudoElement

derive newtype instance ordPseudoElement :: Ord PseudoElement

derive newtype instance showPseudoElement :: Show PseudoElement

type SelectorRep
  = Tuple (Maybe Context) (Tuple (Array Class) (Maybe PseudoElement))

newtype Selector
  = Selector SelectorRep

derive instance newtypeSelector :: Newtype Selector _

derive newtype instance ordSelector :: Ord Selector

derive newtype instance eqSelector :: Eq Selector

instance showSelector :: Show Selector where
  show s =
    "(Selector "
      <> show (s ^. selectorContext)
      <> " "
      <> show (s ^. selectorClasses)
      <> " "
      <> show (s ^. selectorPseudoElement)
      <> ")"

emptySelector :: Selector
emptySelector = Selector $ Tuple Nothing $ Tuple [] Nothing

selectorRep :: Iso' Selector SelectorRep
selectorRep = _Newtype

selectorContext :: Lens' Selector (Maybe Context)
selectorContext = selectorRep <<< _1

selectorClasses :: Lens' Selector (Array Class)
selectorClasses = selectorRep <<< _2 <<< _1

selectorPseudoElement :: Lens' Selector (Maybe PseudoElement)
selectorPseudoElement = selectorRep <<< _2 <<< _2

newtype Property
  = Property String

derive instance newtypeProperty :: Newtype Property _

derive newtype instance eqProperty :: Eq Property

derive newtype instance ordProperty :: Ord Property

derive newtype instance showProperty :: Show Property

newtype Variable
  = Variable String

derive instance newtypeVariable :: Newtype Variable _

derive newtype instance eqVariable :: Eq Variable

derive newtype instance ordVariable :: Ord Variable

derive newtype instance showVariable :: Show Variable

data ValCtx
  = Calc
  | Quoted
  | URL

derive instance genericValCtx :: Generic ValCtx _

instance eqValCtx :: Eq ValCtx where
  eq = genericEq

instance ordValCtx :: Ord ValCtx where
  compare = genericCompare

instance showValCtx :: Show ValCtx where
  show = genericShow

data ValExpr
  = Lit String
  | Var Variable

derive instance genericValExpr :: Generic ValExpr _

instance eqValExpr :: Eq ValExpr where
  eq = genericEq

instance ordValExpr :: Ord ValExpr where
  compare = genericCompare

instance showValExpr :: Show ValExpr where
  show = genericShow

newtype Value
  = Value (Either Variable (Array (Tuple (Maybe ValCtx) (Array ValExpr))))

derive instance newtypeValue :: Newtype Value _

derive newtype instance eqValue :: Eq Value

derive newtype instance ordValue :: Ord Value

derive newtype instance showValue :: Show Value

type DeclarationRep
  = Tuple Property Value

newtype Declaration
  = Declaration DeclarationRep

derive instance newtypeDeclaration :: Newtype Declaration _

derive newtype instance eqDeclaration :: Eq Declaration

derive newtype instance ordDeclaration :: Ord Declaration

instance showDeclaration :: Show Declaration where
  show d =
    let
      rep = _Newtype :: Iso' Declaration DeclarationRep

      property = rep <<< _1

      value = rep <<< _2
    in
      "(Declaration "
        <> show (d ^. property)
        <> " "
        <> show (d ^. value)
        <> ")"

newtype Priority
  = Priority Int

derive instance newtypePriority :: Newtype Priority _

derive newtype instance eqPriority :: Eq Priority

derive newtype instance ordPriority :: Ord Priority

derive newtype instance showPriority :: Show Priority

type RuleRep
  = Tuple (Tuple (Array Declaration) Priority) (Tuple (Maybe Selector) (Maybe AtScope))

newtype Rule
  = Rule RuleRep

derive instance newtypeRule :: Newtype Rule _

derive newtype instance eqRule :: Eq Rule

derive newtype instance ordRule :: Ord Rule

derive newtype instance showRule :: Show Rule

emptyRule :: Rule
emptyRule = Rule $ Tuple (Tuple [] $ Priority 0) $ Tuple Nothing Nothing

ruleRep :: Iso' Rule RuleRep
ruleRep = _Newtype

ruleAtScope :: Lens' Rule (Maybe AtScope)
ruleAtScope = ruleRep <<< _2 <<< _2

ruleDeclarations :: Lens' Rule (Array Declaration)
ruleDeclarations = ruleRep <<< _1 <<< _1

rulePriority :: Lens' Rule Priority
rulePriority = ruleRep <<< _1 <<< _2

ruleSelector :: Lens' Rule (Maybe Selector)
ruleSelector = ruleRep <<< _2 <<< _1
