module Hacss.Data where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..))

newtype Variable
  = Variable String

derive instance newtypeVariable :: Newtype Variable _

derive newtype instance eqVariable :: Eq Variable

derive newtype instance ordVariable :: Ord Variable

derive newtype instance showVariable :: Show Variable

data ValExpr
  = Lit String
  | Var Variable

derive instance genericValExpr :: Generic ValExpr _

instance eqValExpr :: Eq ValExpr where
  eq = genericEq

instance showValExpr :: Show ValExpr where
  show = genericShow

data ValContext
  = Calc
  | Quoted
  | URL

derive instance genericValContext :: Generic ValContext _

instance eqValContext :: Eq ValContext where
  eq = genericEq

instance showValContext :: Show ValContext where
  show = genericShow

newtype Value
  = Value (Array (Tuple (Maybe ValContext) (Array ValExpr)))

derive instance newtypeValue :: Newtype Value _

derive newtype instance eqValue :: Eq Value

derive newtype instance showValue :: Show Value

newtype Property
  = Property String

derive instance newtypeProperty :: Newtype Property _

derive newtype instance eqProperty :: Eq Property

derive newtype instance showProperty :: Show Property

newtype ClassName
  = ClassName String

derive newtype instance eqClassName :: Eq ClassName

derive newtype instance showClassName :: Show ClassName

data PseudoClass
  = BasicPseudoClass String
  | NotPseudoClass ClassList

derive instance genericPseudoClass :: Generic PseudoClass _

instance eqPseudoClass :: Eq PseudoClass where
  eq = genericEq

instance showPseudoClass :: Show PseudoClass where
  show = genericShow

newtype ClassList
  = ClassList (Tuple (Array ClassName) (Array PseudoClass))

instance eqClassList :: Eq ClassList where
  eq (ClassList a) (ClassList b) = a == b

instance showClassList :: Show ClassList where
  show (ClassList (Tuple names pseudos)) = "(ClassList " <> show names <> " " <> show pseudos <> ")"

newtype PseudoElement
  = PseudoElement String

derive newtype instance eqPseudoElement :: Eq PseudoElement

derive newtype instance showPseudoElement :: Show PseudoElement

data Combinator
  = GeneralSibling
  | AdjacentSibling
  | Parent
  | Ancestor

derive instance genericCombinator :: Generic Combinator _

instance eqCombinator :: Eq Combinator where
  eq = genericEq

instance showCombinator :: Show Combinator where
  show = genericShow

newtype Context
  = Context (Tuple ClassList Combinator)

derive newtype instance eqContext :: Eq Context

derive newtype instance showContext :: Show Context

newtype Selector
  = Selector
  { context :: Maybe Context
  , classes :: Maybe ClassList
  , pseudoElement :: Maybe PseudoElement
  }

derive newtype instance eqSelector :: Eq Selector

derive newtype instance showSelector :: Show Selector

newtype AtScope
  = AtScope String

derive instance newtypeAtScope :: Newtype AtScope _

derive newtype instance eqAtScope :: Eq AtScope

derive newtype instance ordAtScope :: Ord AtScope

derive newtype instance showAtScope :: Show AtScope

type Rule
  = { atScope :: Maybe AtScope, selector :: Maybe Selector, declarations :: Array (Tuple Property Value), priority :: Int }
