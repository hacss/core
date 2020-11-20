module Test.Hacss.Printer where

import Prelude
import Data.Either (Either(..))
import Data.Lens ((.~))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Test.Spec (SpecT, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Hacss.Data (AtScope(..), Class(..), Combinator(..), Context(..), Declaration(..), Priority(..), Property(..), PseudoElement(..), ValCtx(..), ValExpr(..), Value(..), Variable(..), emptyRule, emptySelector, ruleAtScope, ruleDeclarations, rulePriority, ruleSelector, selectorClasses, selectorContext, selectorPseudoElement)
import Hacss.Printer (atScope, cls, combinator, context, declaration, priority, property, pseudoElement, rule, selector, value)

tests :: forall m. Monad m => SpecT Aff Unit m Unit
tests = do
  describe "value" do
    it "prints a simple value"
      $ value (Value $ Right [ Tuple Nothing [ Lit "red" ] ]) `shouldEqual` "red"
    it "prints a variable value"
      $ value (Value $ Left $ Variable "red500") `shouldEqual` "$red500"
    it "prints a simple value containing variables"
      $ value (Value $ Right [ Tuple Nothing [ Lit "foo", Var (Variable "asdf"), Lit "bar" ] ])
          `shouldEqual`
            "foo#{$asdf}bar"
    it "prints white space as double underscores in a simple value"
      $ value (Value $ Right [ Tuple Nothing [ Lit "foo ", Var (Variable "asdf"), Lit " bar" ] ])
          `shouldEqual`
            "foo__#{$asdf}__bar"
    it "prints a quoted value"
      $ value (Value $ Right [ Tuple (Just Quoted) [ Lit "foo", Var (Variable "bar") ] ])
          `shouldEqual`
            "'foo#{$bar}'"
    it "prints white space as double underscores in a quoted value"
      $ value (Value $ Right [ Tuple (Just Quoted) [ Lit "a b c" ] ]) `shouldEqual` "'a__b__c'"
    it "prints a combination of quoted and unquoted segments"
      $ value (Value $ Right [ Tuple (Just Quoted) [ Lit "ab" ], Tuple Nothing [ Lit "cd" ], Tuple (Just Quoted) [ Lit "ef" ] ])
          `shouldEqual`
            "'ab'cd'ef'"
    it "prints a URL value"
      $ value (Value $ Right [ Tuple (Just URL) [ Lit "https://bomb.com/logo.gif" ] ])
          `shouldEqual`
            "url('https://bomb.com/logo.gif')"
    it "prints a value containing a URL"
      $ value (Value $ Right [ Tuple Nothing [ Lit "#000 " ], Tuple (Just URL) [ Lit "https://bomb.com/logo.gif" ] ])
          `shouldEqual`
            "#000__url('https://bomb.com/logo.gif')"
    it "prints variables within a URL"
      $ value (Value $ Right [ Tuple (Just URL) [ Lit "https://bomb.com/", Var (Variable "logo"), Lit ".gif" ] ])
          `shouldEqual`
            "url('https://bomb.com/#{$logo}.gif')"
    it "prints white space within a URL as double underscores"
      $ value (Value $ Right [ Tuple (Just URL) [ Lit "https://bomb.com/logo 1.gif" ] ])
          `shouldEqual`
            "url('https://bomb.com/logo__1.gif')"
    it "prints a calc value"
      $ value (Value $ Right [ Tuple (Just Calc) [ Lit "(100% / 3) + 16px" ] ])
          `shouldEqual`
            "calc((100%/3)+16px)"
    it "prints a calc value containing a variable"
      $ value (Value $ Right [ Tuple (Just Calc) [ Lit "(100% / 3) + ", Var (Variable "spacing") ] ])
          `shouldEqual`
            "calc((100%/3)+#{$spacing})"
    it "prints a value containing a calc expression"
      $ value (Value $ Right [ Tuple (Just Calc) [ Lit "(100% / 3) + 16px" ], Tuple Nothing [ Lit " " ], Tuple (Just Calc) [ Lit "100% / 3" ] ])
          `shouldEqual`
            "calc((100%/3)+16px)__calc(100%/3)"
  describe "property"
    $ it "prints a property"
    $ property (Property "foo") `shouldEqual` "foo"
  describe "cls" do
    it "prints a named class"
      $ cls (NamedClass "foo") `shouldEqual` ".foo"
    it "prints a pseudo-class"
      $ cls (PseudoClass "foo") `shouldEqual` ":foo"
    it "prints a :not() pseudo-class"
      $ cls (NotPseudoClass [ NamedClass "foo", PseudoClass "hover" ]) `shouldEqual` ":not(.foo:hover)"
    it "prints a nested :not() pseudo-class"
      $ cls (NotPseudoClass [ NotPseudoClass [ NamedClass "foo" ] ]) `shouldEqual` ":not(:not(.foo))"
  describe "pseudoElement"
    $ it "prints a pseudo-element"
    $ pseudoElement (PseudoElement "after") `shouldEqual` "::after"
  describe "combinator" do
    it "prints an ancestor combinator"
      $ combinator Ancestor `shouldEqual` "_"
    it "prints a parent combinator"
      $ combinator Parent `shouldEqual` ">"
    it "prints an adjacent sibling"
      $ combinator AdjSib `shouldEqual` "+"
    it "prints a general sibling"
      $ combinator GenSib `shouldEqual` "~"
  describe "context"
    $ it "prints a context including class list and combinator"
    $ context (Context $ Tuple [ NamedClass "foo", PseudoClass "hover" ] AdjSib) `shouldEqual` ".foo:hover+"
  describe "selector"
    $ let
        context' = Just $ Context $ Tuple [ NamedClass "bar", PseudoClass "nth-child(1)" ] Parent

        classes = [ NamedClass "foo", NamedClass "bar", PseudoClass "hover", NotPseudoClass [ NamedClass "asdf", PseudoClass "disabled" ] ]

        pseudoElement' = Just $ PseudoElement "first-line"
      in
        do
          it "prints a selector with context only"
            $ selector (emptySelector # selectorContext .~ context') `shouldEqual` ".bar:nth-child(1)>"
          it "prints a selector with classes only"
            $ selector (emptySelector # selectorClasses .~ classes) `shouldEqual` ".foo.bar:hover:not(.asdf:disabled)"
          it "prints a selector with pseudo-element only"
            $ selector (emptySelector # selectorPseudoElement .~ pseudoElement') `shouldEqual` "::first-line"
          it "prints a selector with all components"
            $ selector
                ( emptySelector
                    # selectorContext
                    .~ context'
                    # selectorClasses
                    .~ classes
                    # selectorPseudoElement
                    .~ pseudoElement'
                )
                `shouldEqual`
                  ".bar:nth-child(1)>.foo.bar:hover:not(.asdf:disabled)::first-line"
  describe "atScope"
    $ it "prints an at-scope"
    $ atScope (AtScope "small") `shouldEqual` "@small"
  describe "priority"
    $ it "prints a priority"
    $ priority (Priority 5) `shouldEqual` "!!!!!"
  describe "declaration"
    $ it "prints a declaration"
    $ declaration (Declaration $ Tuple (Property "background") (Value $ Right [ Tuple Nothing [ Lit "red" ] ])) `shouldEqual` "background:red"
  describe "rule" do
    it "prints a declarations-only rule"
      $ rule (emptyRule # ruleDeclarations .~ (Declaration <$> [ Tuple (Property "color") (Value $ Right [ Tuple Nothing [ Lit "red" ] ]), Tuple (Property "font-weight") (Value $ Right [ Tuple Nothing [ Lit "bold" ] ]) ]))
          `shouldEqual`
            "color:red;font-weight:bold;"
    it "prints a declarations-only rule with priority"
      $ rule (emptyRule # ruleDeclarations .~ (Declaration <$> [ Tuple (Property "color") (Value $ Right [ Tuple Nothing [ Lit "red" ] ]), Tuple (Property "font-weight") (Value $ Right [ Tuple Nothing [ Lit "bold" ] ]) ]) # rulePriority .~ Priority 4)
          `shouldEqual`
            "color:red;font-weight:bold;!!!!"
    it "prints a rule with a selector"
      $ rule (emptyRule # ruleSelector .~ Just (emptySelector # selectorClasses .~ [ NamedClass "foo" ]) # ruleDeclarations .~ ([ Declaration $ Tuple (Property "color") (Value $ Right [ Tuple Nothing [ Lit "red" ] ]) ]))
          `shouldEqual`
            ".foo{color:red}"
    it "prints a rule with a selector and priority"
      $ rule (emptyRule # ruleSelector .~ Just (emptySelector # selectorClasses .~ [ NamedClass "foo" ]) # ruleDeclarations .~ ([ Declaration $ Tuple (Property "color") (Value $ Right [ Tuple Nothing [ Lit "red" ] ]) ]) # rulePriority .~ Priority 1)
          `shouldEqual`
            ".foo{color:red}!"
    it "prints a rule with an at-scope and selector"
      $ rule (emptyRule # ruleAtScope .~ Just (AtScope "small") # ruleSelector .~ Just (emptySelector # selectorClasses .~ [ NamedClass "foo" ]) # ruleDeclarations .~ [ Declaration $ Tuple (Property "color") (Value $ Right [ Tuple Nothing [ Lit "red" ] ]) ])
          `shouldEqual`
            "@small{.foo{color:red}}"
    it "prints a rule with an at-scope, selector, and priority"
      $ rule (emptyRule # ruleAtScope .~ Just (AtScope "small") # ruleSelector .~ Just (emptySelector # selectorClasses .~ [ NamedClass "foo" ]) # rulePriority .~ Priority 1 # ruleDeclarations .~ [ Declaration $ Tuple (Property "color") (Value $ Right [ Tuple Nothing [ Lit "red" ] ]) ])
          `shouldEqual`
            "@small{.foo{color:red}}!"
