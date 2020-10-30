module Test.Hacss.Printer where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Test.Spec (SpecT, describe, it)
import Test.Spec.Assertions (shouldEqual)

import Hacss.Data
  ( AtScope(..)
  , ClassName(..)
  , Combinator(..)
  , Declaration(..)
  , Property(..)
  , PseudoClass(..)
  , PseudoElement(..)
  , ValExpr(..)
  , QualifiedVal(..)
  , ValSeg(..)
  , Value(..)
  , Variable(..)
  )

import Hacss.Printer
  ( atScope
  , className
  , combinator
  , context
  , declaration
  , property
  , pseudoClass
  , pseudoElement
  , rule
  , selector
  , value
  )

tests :: forall m. Monad m => SpecT Aff Unit m Unit
tests = do



  describe "className" $
    it "should print a ClassName to a string" $
      className (ClassName "foo-bar") `shouldEqual` ".foo-bar"



  describe "pseudoClass" $
    it "should print a PseudoClass to a string" $
      pseudoClass (PseudoClass "nth-child(2n)") `shouldEqual` ":nth-child(2n)"



  describe "pseudoElement" $
    it "should print a PseudoElement to a string" $
      pseudoElement (PseudoElement "placeholder") `shouldEqual` "::placeholder"



  describe "combinator" do

    it "should print an ancestor combinator to a string" $
      combinator Ancestor `shouldEqual` "_"

    it "should print a parent combinator to a string" $
      combinator Parent `shouldEqual` ">"

    it "should print an adjacent sibling combinator to a string" $
      combinator AdjacentSibling `shouldEqual` "+"

    it "should print a general sibling combinator to a string" $
      combinator GeneralSibling `shouldEqual` "~"



  describe "atScope" $
    it "should print an AtScope to a string" $
      atScope (AtScope "foo") `shouldEqual` "@foo"



  describe "property" $
    it "should print a Property to a string" $
      property (Property "foo") `shouldEqual` "foo"



  describe "value" do

    it "should print an unquoted URL value to a string" $
      value (Value [URL $ Unquoted [Lit "https://xyz.xyz/xyz.gif"]])
      `shouldEqual`
      "url(https://xyz.xyz/xyz.gif)"

    it "should print a quoted URL value to a string" $
      value (Value [URL $ Quoted [Lit "https://xyz.xyz/xyz.gif"]])
      `shouldEqual`
      "url('https://xyz.xyz/xyz.gif')"

    it "should print a calc value to a string" $
      value (Value [Calc [Lit "1rem + (", Var $ Variable "foo", Lit " - 1px)"]])
      `shouldEqual`
      "calc(1rem+($foo-1px))"

    it "should print an unquoted simple value to a string" $
      value (Value [Simple $ Unquoted [Lit "#ccc"]])
      `shouldEqual`
      "#ccc"

    it "should print a quoted simple value to a string" $
      value (Value [Simple $ Quoted [Lit "hello", Var $ Variable "world"]])
      `shouldEqual`
      "'hello$world'"

    it "should print a complex value to a string" $
      value
        ( Value
          [ Simple $ Unquoted [Lit "#000 "]
          , URL $ Quoted [Lit "https://xyz.xyz/logo.gif"]
          ]
        )
      `shouldEqual`
      "#000__url('https://xyz.xyz/logo.gif')"



  describe "declaration" do

    it "should print a Declaration to a string" $
      declaration (Declaration $ Tuple (Property "font-size")
                                       (Value [Simple $ Unquoted [Lit "16px"]]))
      `shouldEqual`
      "font-size:16px"



  describe "context" do

    it "should print a Context to a string" $
      context
        { classNames: ClassName <$> ["foo", "bar"]
        , pseudoClasses: PseudoClass <$> ["nth-child(2n+1)", "hover"]
        , combinator: GeneralSibling
        }
      `shouldEqual`
      ".foo.bar:nth-child(2n+1):hover~"



  describe "selector" do

    it "should print a Selector to a string" $
      selector
        { context:
            Just
              { classNames: ClassName <$> ["foo", "bar"]
              , pseudoClasses: PseudoClass <$> ["hover", "active"]
              , combinator: GeneralSibling
              }
        , classNames: ClassName <$> ["abc", "def"]
        , pseudoClasses: PseudoClass <$> ["focus", "disabled"]
        , pseudoElement: Just $ PseudoElement "before"
        }
      `shouldEqual`
      ".foo.bar:hover:active~.abc.def:focus:disabled::before"



  describe "rule" do

    let
      declarations =
        Declaration <$>
          [ Tuple
              (Property "background")
              (Value [Simple $ Unquoted [Lit "red"]])
          , Tuple
              (Property "color")
              (Value [Simple $ Unquoted [Lit "white"]])
          ]

    it "should print a declarations-only Rule to a string" $
      rule
        { atScope: Nothing
        , selector: Nothing
        , declarations
        , importance: 0
        }
      `shouldEqual`
      "background:red;color:white;"

    it "should print a Rule with an at-scope to a string" $
      rule
        { atScope: Just $ AtScope "sm"
        , selector: Nothing
        , declarations
        , importance: 0
        }
      `shouldEqual`
      "@sm{background:red;color:white}"

    it "should print a Rule with a selector to a string" $
      rule
        { atScope: Nothing
        , selector:
            Just
              { context: Nothing
              , classNames: [ClassName "err"]
              , pseudoClasses: []
              , pseudoElement: Nothing
              }
        , declarations
        , importance: 0
        }
      `shouldEqual`
      ".err{background:red;color:white}"

    it "should print a Rule with an at-scope and a selector to a string" $
      rule
        { atScope: Just $ AtScope "sm"
        , selector:
            Just
              { context: Nothing
              , classNames: [ClassName "err"]
              , pseudoClasses: []
              , pseudoElement: Nothing
              }
        , declarations
        , importance: 0
        }
      `shouldEqual`
      "@sm{.err{background:red;color:white}}"

    it "should print a declarations-only Rule with importance to a string" $
      rule
        { atScope: Nothing
        , selector: Nothing
        , declarations
        , importance: 2
        }
      `shouldEqual`
      "background:red;color:white;!!"

    it "should print a Rule with an at-scope, a selector, and importance to a string" $
      rule
        { atScope: Just $ AtScope "sm"
        , selector:
            Just
              { context: Nothing
              , classNames: [ClassName "err"]
              , pseudoClasses: []
              , pseudoElement: Nothing
              }
        , declarations
        , importance: 1
        }
      `shouldEqual`
      "@sm{.err{background:red;color:white}}!"
