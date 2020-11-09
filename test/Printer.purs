module Test.Hacss.Printer where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Test.Spec (SpecT, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Hacss.Data (AtScope(..), ClassList(..), ClassName(..), Combinator(..), Context(..), Property(..), PseudoClass(..), PseudoElement(..), Selector(..), ValContext(..), ValExpr(..), Value(..), Variable(..))
import Hacss.Printer (atScope, className, combinator, context, declaration, priority, property, pseudoClass, pseudoElement, rule, selector, value, variable)

tests :: forall m. Monad m => SpecT Aff Unit m Unit
tests = do
  describe "variable"
    $ it "prints a variable"
    $ variable (Variable "foo") `shouldEqual` "$foo"
  describe "value" do
    it "prints a simple value"
      $ value (Value [ Tuple Nothing [ Lit "red" ] ]) `shouldEqual` "red"
    it "prints a variable value"
      $ value (Value [ Tuple Nothing [ Var (Variable "red500") ] ]) `shouldEqual` "$red500"
    it "prints a simple value containing variables"
      $ value (Value [ Tuple Nothing [ Lit "foo", Var (Variable "asdf"), Lit "bar" ] ])
          `shouldEqual`
            "foo$asdfbar"
    it "prints white space as double underscores in a simple value"
      $ value (Value [ Tuple Nothing [ Lit "foo ", Var (Variable "asdf"), Lit " bar" ] ])
          `shouldEqual`
            "foo__$asdf__bar"
    it "prints a quoted value"
      $ value (Value [ Tuple (Just Quoted) [ Lit "foo", Var (Variable "bar") ] ])
          `shouldEqual`
            "'foo$bar'"
    it "prints white space as double underscores in a quoted value"
      $ value (Value [ Tuple (Just Quoted) [ Lit "a b c" ] ]) `shouldEqual` "'a__b__c'"
    it "prints a combination of quoted and unquoted segments"
      $ value (Value [ Tuple (Just Quoted) [ Lit "ab" ], Tuple Nothing [ Lit "cd" ], Tuple (Just Quoted) [ Lit "ef" ] ])
          `shouldEqual`
            "'ab'cd'ef'"
    it "prints a URL value"
      $ value (Value [ Tuple (Just URL) [ Lit "https://bomb.com/logo.gif" ] ])
          `shouldEqual`
            "url('https://bomb.com/logo.gif')"
    it "prints a value containing a URL"
      $ value (Value [ Tuple Nothing [ Lit "#000 " ], Tuple (Just URL) [ Lit "https://bomb.com/logo.gif" ] ])
          `shouldEqual`
            "#000__url('https://bomb.com/logo.gif')"
    it "prints variables within a URL"
      $ value (Value [ Tuple (Just URL) [ Lit "https://bomb.com/", Var (Variable "logo"), Lit ".gif" ] ])
          `shouldEqual`
            "url('https://bomb.com/$logo.gif')"
    it "prints white space within a URL as double underscores"
      $ value (Value [ Tuple (Just URL) [ Lit "https://bomb.com/logo 1.gif" ] ])
          `shouldEqual`
            "url('https://bomb.com/logo__1.gif')"
    it "prints a calc value"
      $ value (Value [ Tuple (Just Calc) [ Lit "(100% / 3) + 16px" ] ])
          `shouldEqual`
            "calc((100%/3)+16px)"
    it "prints a calc value containing a variable"
      $ value (Value [ Tuple (Just Calc) [ Lit "(100% / 3) + ", Var (Variable "spacing") ] ])
          `shouldEqual`
            "calc((100%/3)+$spacing)"
    it "prints a value containing a calc expression"
      $ value (Value [ Tuple (Just Calc) [ Lit "(100% / 3) + 16px" ], Tuple Nothing [ Lit " " ], Tuple (Just Calc) [ Lit "100% / 3" ] ])
          `shouldEqual`
            "calc((100%/3)+16px)__calc(100%/3)"
  describe "property"
    $ it "prints a property"
    $ property (Property "foo") `shouldEqual` "foo"
  describe "className"
    $ it "prints a class name"
    $ className (ClassName "foo") `shouldEqual` ".foo"
  describe "pseudoClass" do
    it "prints a basic pseudo-class"
      $ pseudoClass (BasicPseudoClass "nth-child(2n+5)") `shouldEqual` ":nth-child(2n+5)"
    it "prints a not pseudo-class"
      $ pseudoClass (NotPseudoClass $ ClassList $ Tuple [ ClassName "foo" ] [ NotPseudoClass $ ClassList $ Tuple [] [ BasicPseudoClass "focus" ] ])
          `shouldEqual`
            ":not(.foo:not(:focus))"
  describe "pseudoElement"
    $ it "prints a pseudo-element"
    $ pseudoElement (PseudoElement "after") `shouldEqual` "::after"
  describe "combinator" do
    it "prints an ancestor combinator"
      $ combinator Ancestor `shouldEqual` "_"
    it "prints a parent combinator"
      $ combinator Parent `shouldEqual` ">"
    it "prints an adjacent sibling"
      $ combinator AdjacentSibling `shouldEqual` "+"
    it "prints a general sibling"
      $ combinator GeneralSibling `shouldEqual` "~"
  describe "context"
    $ it "prints a context including class list and combinator"
    $ context (Context $ Tuple (ClassList $ Tuple [ ClassName "foo" ] [ BasicPseudoClass "hover" ]) AdjacentSibling) `shouldEqual` ".foo:hover+"
  describe "selector"
    $ let
        defaults = { context: Nothing, classes: Nothing, pseudoElement: Nothing }

        ctx = Just $ Context $ Tuple (ClassList $ Tuple [ ClassName "bar" ] [ BasicPseudoClass "nth-child(1)" ]) Parent

        classes = Just $ ClassList $ Tuple (ClassName <$> [ "foo", "bar" ]) [ BasicPseudoClass "hover", NotPseudoClass $ ClassList $ Tuple [ ClassName "asdf" ] [ BasicPseudoClass "disabled" ] ]

        pseudoEl = Just $ PseudoElement "first-line"
      in
        do
          it "prints a selector with context only"
            $ selector (Selector defaults { context = ctx }) `shouldEqual` ".bar:nth-child(1)>"
          it "prints a selector with classes only"
            $ selector (Selector defaults { classes = classes }) `shouldEqual` ".foo.bar:hover:not(.asdf:disabled)"
          it "prints a selector with pseudo-element only"
            $ selector (Selector defaults { pseudoElement = pseudoEl }) `shouldEqual` "::first-line"
          it "prints a selector with context and classes"
            $ selector (Selector defaults { context = ctx, classes = classes }) `shouldEqual` ".bar:nth-child(1)>.foo.bar:hover:not(.asdf:disabled)"
          it "prints a selector with classes and pseudo-element"
            $ selector (Selector defaults { classes = classes, pseudoElement = pseudoEl }) `shouldEqual` ".foo.bar:hover:not(.asdf:disabled)::first-line"
          it "prints a selector with context and pseudo-element"
            $ selector (Selector defaults { context = ctx, pseudoElement = pseudoEl }) `shouldEqual` ".bar:nth-child(1)>::first-line"
          it "prints a selector with all components"
            $ selector (Selector defaults { context = ctx, classes = classes, pseudoElement = pseudoEl }) `shouldEqual` ".bar:nth-child(1)>.foo.bar:hover:not(.asdf:disabled)::first-line"
  describe "atScope"
    $ it "should print an at-scope"
    $ atScope (AtScope "small") `shouldEqual` "@small"
  describe "priority"
    $ it "should print a priority"
    $ priority 5 `shouldEqual` "!!!!!"
  describe "declaration"
    $ it "should print a declaration"
    $ declaration (Tuple (Property "background") (Value [ Tuple Nothing [ Lit "red" ] ])) `shouldEqual` "background:red"
  describe "rule" do
    it "should print a declarations-only rule"
      $ rule { atScope: Nothing, selector: Nothing, declarations: [ Tuple (Property "color") (Value [ Tuple Nothing [ Lit "red" ] ]), Tuple (Property "font-weight") (Value [ Tuple Nothing [ Lit "bold" ] ]) ], priority: 0 }
          `shouldEqual`
            "color:red;font-weight:bold;"
    it "should print a declarations-only rule with priority"
      $ rule { atScope: Nothing, selector: Nothing, declarations: [ Tuple (Property "color") (Value [ Tuple Nothing [ Lit "red" ] ]) ], priority: 4 }
          `shouldEqual`
            "color:red;!!!!"
    it "should print a rule with a selector"
      $ rule { atScope: Nothing, selector: Just (Selector { context: Nothing, classes: Just $ ClassList $ Tuple [ ClassName "foo" ] [], pseudoElement: Nothing }), declarations: [ Tuple (Property "color") (Value [ Tuple Nothing [ Lit "red" ] ]) ], priority: 0 }
          `shouldEqual`
            ".foo{color:red}"
    it "should print a rule with a selector and priority"
      $ rule { atScope: Nothing, selector: Just (Selector { context: Nothing, classes: Just $ ClassList $ Tuple [ ClassName "foo" ] [], pseudoElement: Nothing }), declarations: [ Tuple (Property "color") (Value [ Tuple Nothing [ Lit "red" ] ]) ], priority: 1 }
          `shouldEqual`
            ".foo{color:red}!"
    it "should print a rule with an at-scope and selector"
      $ rule { atScope: Just $ AtScope "small", selector: Just (Selector { context: Nothing, classes: Just $ ClassList $ Tuple [ ClassName "foo" ] [], pseudoElement: Nothing }), declarations: [ Tuple (Property "color") (Value [ Tuple Nothing [ Lit "red" ] ]) ], priority: 0 }
          `shouldEqual`
            "@small{.foo{color:red}}"
    it "should print a rule with an at-scope, selector, and priority"
      $ rule { atScope: Just $ AtScope "small", selector: Just (Selector { context: Nothing, classes: Just $ ClassList $ Tuple [ ClassName "foo" ] [], pseudoElement: Nothing }), declarations: [ Tuple (Property "color") (Value [ Tuple Nothing [ Lit "red" ] ]) ], priority: 1 }
          `shouldEqual`
            "@small{.foo{color:red}}!"
