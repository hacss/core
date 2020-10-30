module Hacss.Data where
  
import Prelude
import Data.Array (fromFoldable) as A
import Data.Char.Unicode (isLower)
import Data.Enum (toEnumWithDefaults)
import Data.Foldable (foldl)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.List ((:))
import Data.List (concat) as L
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, un)
import Data.NonEmpty ((:|))
import Data.NonEmpty (oneOf) as NE
import Data.String.CodeUnits (fromCharArray) as S
import Data.String.Common (joinWith) as S
import Data.Tuple (Tuple)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (arrayOf1, chooseInt, elements, listOf, oneOf, suchThat)

newtype ClassName = ClassName String

derive instance newtypeClassName :: Newtype ClassName _
derive instance genericClassName :: Generic ClassName _

instance showClassName :: Show ClassName where
  show = genericShow

instance eqClassName :: Eq ClassName where
  eq = genericEq

instance arbitraryClassName :: Arbitrary ClassName where
  arbitrary =
    let
      char = toEnumWithDefaults bottom top <$> chooseInt 33 126
      word = chooseInt 1 8 >>= \n -> listOf n (char `suchThat` isLower)
    in do
      n <- chooseInt 0 3
      x <- (\x xs -> x : (('-' : _) <$> xs)) <$> word <*> listOf n word
      pure $ ClassName $ S.fromCharArray $ A.fromFoldable $ L.concat x

data PseudoClass = PseudoClass String | PseudoClassNot (Record (ClassesRep ()))

derive instance genericPseudoClass :: Generic PseudoClass _

instance showPseudoClass :: Show PseudoClass where
  show =
    case _ of
      PseudoClass x ->
        "PseudoClass " <> show x
      PseudoClassNot x ->
        "PseudoClassNot " <> show x

instance eqPseudoClass :: Eq PseudoClass where
  eq (PseudoClass _) (PseudoClassNot _) = false
  eq (PseudoClassNot _) (PseudoClass _) = false
  eq (PseudoClass a) (PseudoClass b) = a == b
  eq (PseudoClassNot a) (PseudoClassNot b) = a == b

instance arbitraryPseudoClass :: Arbitrary PseudoClass where
  arbitrary =
    let
      basic =
        elements $
          "active" :|
          [ "checked"
          , "disabled"
          , "empty"
          , "enabled"
          , "first-child"
          , "first-of-type"
          , "focus-within"
          , "focus"
          , "hover"
          , "in-range"
          , "invalid"
          , "last-child"
          , "last-of-type"
          , "link"
          , "only-of-type"
          , "only-child"
          , "optional"
          , "out-of-range"
          , "read-only"
          , "read-write"
          , "required"
          , "root"
          , "target"
          , "valid"
          , "visited"
          ]
      lang =
        let
          char = do
            i <- oneOf (chooseInt 65 90 :| [chooseInt 97 122])
            pure $ toEnumWithDefaults bottom top i
        in do
          words <- arrayOf1 $ S.fromCharArray <<< NE.oneOf <$> arrayOf1 char
          pure $ "lang(" <> S.joinWith "-" (NE.oneOf words) <> ")"
      nth =
        let
          formula =
            let
              digit = toEnumWithDefaults bottom top <$> chooseInt 48 57
              a = S.fromCharArray <<< NE.oneOf <$> arrayOf1 digit
              anplusb = do
                a' <- S.fromCharArray <<< NE.oneOf <$> arrayOf1 digit
                plus <- elements $ "-" :| ["+"]
                b <- S.fromCharArray <<< NE.oneOf <$> arrayOf1 digit
                pure $ a' <> "n" <> plus <> b
            in do
              (<>) <$> elements ("" :| ["-"]) <*> (oneOf $ anplusb :| [a])
        in do
          t <- elements $ "child" :| ["last-child", "last-of-type", "of-type"]
          f <- oneOf $ elements ("even" :| ["odd"]) :| [formula]
          pure $ "nth-" <> t <> "(" <> f <> ")"
      pseudoClass = PseudoClass <$> (oneOf $ basic :| [lang, nth])
    in
      oneOf $
        pseudoClass :|
        [ do
            classNames :: Array ClassName <- NE.oneOf <$> arrayOf1 arbitrary
            pseudoClasses :: Array PseudoClass <- NE.oneOf <$> arrayOf1 pseudoClass
            pure $ PseudoClassNot { classNames, pseudoClasses }
        ]

newtype PseudoElement = PseudoElement String

derive instance newtypePseudoElement :: Newtype PseudoElement _
derive instance genericPseudoElement :: Generic PseudoElement _

instance showPseudoElement :: Show PseudoElement where
  show = genericShow

instance eqPseudoElement :: Eq PseudoElement where
  eq = genericEq

instance arbitraryPseudoElement :: Arbitrary PseudoElement where
  arbitrary =
    let
      standard =
        elements $
          "after" :|
          [ "before"
          , "first-letter"
          , "first-line"
          , "marker"
          , "placeholder"
          , "selection"
          ]
      prefixed =
        let
          lower = toEnumWithDefaults bottom top <$> chooseInt 97 122
          word =
            (S.fromCharArray <<< A.fromFoldable)
              <$> (chooseInt 1 10 >>= \n -> listOf n lower)
        in
          foldl (\a b -> a <> "-" <> b)
            <$> elements (("-" <> _) <$> ("moz" :| ["ms", "o", "webkit"]))
            <*> arrayOf1 word
    in
      PseudoElement <$> (oneOf $ standard :| [prefixed])

data Combinator = Ancestor | Parent | AdjacentSibling | GeneralSibling

derive instance genericCombinator :: Generic Combinator _

instance showCombinator :: Show Combinator where
  show = genericShow

instance eqCombinator :: Eq Combinator where
  eq = genericEq

instance arbitaryCombinator :: Arbitrary Combinator where
  arbitrary = elements $ Ancestor :| [Parent, AdjacentSibling, GeneralSibling]

newtype AtScope = AtScope String

derive instance newtypeAtScope :: Newtype AtScope _
derive instance genericAtScope :: Generic AtScope _

instance showAtScope :: Show AtScope where
  show = genericShow

instance eqAtScope :: Eq AtScope where
  eq = genericEq

derive newtype instance ordAtScope :: Ord AtScope

instance arbitraryAtScope :: Arbitrary AtScope where
  arbitrary =
    let
      lower = toEnumWithDefaults bottom top <$> chooseInt 97 122
      word =
        (S.fromCharArray <<< A.fromFoldable)
          <$> (chooseInt 1 10 >>= \n -> listOf n lower)
    in
      AtScope <<< S.joinWith "-" <<< NE.oneOf <$> arrayOf1 word

newtype Property = Property String

derive instance newtypeProperty :: Newtype Property _
derive instance genericProperty :: Generic Property _

instance showProperty :: Show Property where
  show = genericShow

instance eqProperty :: Eq Property where
  eq = genericEq

instance arbitraryProperty :: Arbitrary Property where
  arbitrary = 
    let
      lower = toEnumWithDefaults bottom top <$> chooseInt 97 122
      word =
        (S.fromCharArray <<< A.fromFoldable)
          <$> (chooseInt 1 10 >>= \n -> listOf n lower)
    in
      Property <<< S.joinWith "-" <<< NE.oneOf <$> arrayOf1 word

newtype Variable = Variable String

derive instance newtypeVariable :: Newtype Variable _
derive instance genericVariable :: Generic Variable _

instance showVariable :: Show Variable where
  show = genericShow

instance eqVariable :: Eq Variable where
  eq = genericEq

derive newtype instance ordVariable :: Ord Variable

variableName :: Variable -> String
variableName = un Variable

data ValExpr = Lit String | Var Variable

derive instance genericValExpr :: Generic ValExpr _

instance showValExpr :: Show ValExpr where
  show = genericShow

instance eqValExpr :: Eq ValExpr where
  eq = genericEq

data QualifiedVal = Quoted (Array ValExpr) | Unquoted (Array ValExpr)

derive instance genericQualifiedVal :: Generic QualifiedVal _

instance showQualifiedVal :: Show QualifiedVal where
  show = genericShow

instance eqQualifiedVal :: Eq QualifiedVal where
  eq = genericEq

data ValSeg = Simple QualifiedVal | URL QualifiedVal | Calc (Array ValExpr)

derive instance genericValSeg :: Generic ValSeg _

instance showValSeg :: Show ValSeg where
  show = genericShow

instance eqValSeg :: Eq ValSeg where
  eq = genericEq

newtype Value = Value (Array ValSeg)

derive instance newtypeValue :: Newtype Value _
derive instance genericValue :: Generic Value _

instance showValue :: Show Value where
  show = genericShow

instance eqValue :: Eq Value where
  eq = genericEq

newtype Declaration = Declaration (Tuple Property Value)

derive instance genericDeclaration :: Generic Declaration _

instance showDeclaration :: Show Declaration where
  show = genericShow

instance eqDeclaration :: Eq Declaration where
  eq = genericEq

type ClassesRep r =
  (classNames :: Array ClassName, pseudoClasses :: Array PseudoClass | r)

type Context = Record (ClassesRep (combinator :: Combinator))

type Selector =
  Record
    ( ClassesRep
      ( context :: Maybe Context
      , pseudoElement :: Maybe PseudoElement
      )
    )

type Rule =
  { atScope :: Maybe AtScope
  , selector :: Maybe Selector
  , declarations :: Array Declaration
  , importance :: Int
  }
