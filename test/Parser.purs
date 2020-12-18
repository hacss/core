module Test.Hacss.Internal.Parser (tests) where

import Prelude
import Data.Either (Either(..), isLeft)
import Data.Lens ((.~))
import Data.Maybe (Maybe(..))
import Data.Traversable (for_)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Test.Spec (SpecT, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Text.Parsing.StringParser (runParser)
import Hacss.Internal.Data (AtScope(..), Class(..), Combinator(..), Context(..), Declaration(..), Priority(..), Property(..), PseudoElement(..), ValCtx(..), ValExpr(..), Value(..), Variable(..), emptyRule, emptySelector, ruleAtScope, ruleDeclarations, rulePriority, ruleSelector, selectorContext, selectorClasses, selectorPseudoElement)
import Hacss.Internal.Parser (atScope, cls, combinator, context, declaration, priority, property, pseudoElement, rule, selector, value)

tests :: forall m. Monad m => SpecT Aff Unit m Unit
tests = do
  describe "value" do
    it "accepts a variable-only value"
      $ runParser value "$foo-bar"
          `shouldEqual`
            Right (Value (Left (Variable "foo-bar")))
    it "accepts a variable-only value containing digits"
      $ runParser value "$red700"
          `shouldEqual`
            Right (Value (Left (Variable "red700")))
    it "rejects invalid characters in variable-only values" do
      runParser value "$upperCaseNotAllowed"
        `shouldEqual`
          Right (Value (Left (Variable "upper")))
      runParser value "$under_not_allowed"
        `shouldEqual`
          Right (Value (Left (Variable "under")))
    it "rejects leading hyphens in variable-only values"
      $ runParser value "$-hello" `shouldSatisfy` isLeft
    it "rejects trailing hyphens in variable-only values"
      $ runParser value "$hello-"
          `shouldEqual`
            Right (Value (Left (Variable "hello")))
    it "rejects consecutive hyphens in variable-only values"
      $ runParser value "$he--llo"
          `shouldEqual`
            Right (Value (Left (Variable "he")))
    it "accepts a simple unquoted value"
      $ runParser value "#000"
          `shouldEqual`
            Right (Value (Right [ Tuple Nothing [ Lit "#000" ] ]))
    it "accepts an unquoted value containing a variable"
      $ runParser value "asdf#{$asdf-fdsa}fdsa"
          `shouldEqual`
            Right
              ( Value
                  ( Right
                      [ Tuple
                          Nothing
                          [ Lit "asdf", Var (Variable "asdf-fdsa"), Lit "fdsa" ]
                      ]
                  )
              )
    it "parses double underscores as spaces within an unquoted value"
      $ runParser value "hello__world"
          `shouldEqual`
            Right (Value (Right [ Tuple Nothing [ Lit "hello world" ] ]))
    it "rejects invalid characters in interpolated variables within an unquoted value"
      $ for_ [ "asdfFoo", "asd_f" ] \x ->
          runParser value ("asdf#{$" <> x <> "}fdsa")
            `shouldEqual`
              Right (Value (Right [ Tuple Nothing [ Lit "asdf" ] ]))
    for_
      [ Tuple "leading" "-hi", Tuple "trailing" "hi-", Tuple "consecutive" "hi--hi" ] \(Tuple a b) ->
      it ("rejects " <> a <> " hyphens in interpolated variables within an unquoted value")
        $ runParser value ("foo#{$" <> b <> "}")
            `shouldEqual`
              Right (Value (Right [ Tuple Nothing [ Lit "foo" ] ]))
    it "accepts a simple quoted value"
      $ runParser value "'#000'"
          `shouldEqual`
            Right (Value (Right [ Tuple (Just Quoted) [ Lit "#000" ] ]))
    it "accepts an empty-string quoted value"
      $ runParser value "''"
          `shouldEqual`
            Right (Value (Right [ Tuple (Just Quoted) [] ]))
    it "accepts a quoted value containing a variable"
      $ runParser value "'asdf#{$asdf-fdsa}fdsa'"
          `shouldEqual`
            Right
              ( Value
                  ( Right
                      [ Tuple
                          (Just Quoted)
                          [ Lit "asdf", Var (Variable "asdf-fdsa"), Lit "fdsa" ]
                      ]
                  )
              )
    it "parses double underscores as spaces within a quoted value"
      $ runParser value "'hello__world'"
          `shouldEqual`
            Right (Value (Right [ Tuple (Just Quoted) [ Lit "hello world" ] ]))
    it "rejects invalid characters in interpolated variables within a quoted value"
      $ for_ [ "asdfFoo", "asd_f" ] \x ->
          runParser value ("'asdf#{$" <> x <> "}fdsa'") `shouldSatisfy` isLeft
    for_
      [ Tuple "leading" "-hi", Tuple "trailing" "hi-", Tuple "consecutive" "hi--hi" ] \(Tuple a b) ->
      it ("rejects " <> a <> " hyphens in interpolated variables within a quoted value")
        $ runParser value ("'foo#{$" <> b <> "}'") `shouldSatisfy` isLeft
    it "accepts a simple URL value"
      $ runParser value "url('https://xyz.abc/logo.gif')"
          `shouldEqual`
            Right (Value (Right [ Tuple (Just URL) [ Lit "https://xyz.abc/logo.gif" ] ]))
    it "accepts a URL value containing a variable"
      $ runParser value "url('https://bomb.com/logo.gif?c=#{$red500}&a=0')"
          `shouldEqual`
            Right
              ( Value
                  ( Right
                      [ Tuple
                          (Just URL)
                          [ Lit "https://bomb.com/logo.gif?c="
                          , Var (Variable "red500")
                          , Lit "&a=0"
                          ]
                      ]
                  )
              )
    it "parses double underscores as spaces within a URL value"
      $ runParser value "url('https://abc.xyz/logo__1.gif')"
          `shouldEqual`
            Right (Value (Right [ Tuple (Just URL) [ Lit "https://abc.xyz/logo 1.gif" ] ]))
    it "rejects invalid characters in interpolated variables within a URL value"
      $ for_ [ "asdfFoo", "asd_f" ] \x ->
          runParser value ("url('http://foo.bar/#{$" <> x <> "}.gif')")
            `shouldSatisfy`
              isLeft
    for_
      [ Tuple "leading" "-hi", Tuple "trailing" "hi-", Tuple "consecutive" "hi--hi" ] \(Tuple a b) ->
      it ("rejects " <> a <> " hyphens in interpolated variables within a URL value")
        $ runParser value ("url('http://a.bc/#{$" <> b <> "}.gif')")
            `shouldSatisfy`
              isLeft
    for_ [ Tuple "semicolon" ";", Tuple "curly brace" "}" ] \(Tuple a b) -> do
      it ("rejects a " <> a <> " in an unquoted value")
        $ runParser value ("black" <> b <> "yellow")
            `shouldEqual`
              Right (Value (Right [ Tuple Nothing [ Lit "black" ] ]))
      it ("accepts a " <> a <> " in a quoted value")
        $ runParser value ("'black" <> b <> "yellow'")
            `shouldEqual`
              Right (Value (Right [ Tuple (Just Quoted) [ Lit ("black" <> b <> "yellow") ] ]))
      it ("accepts a " <> a <> " in a URL value")
        $ runParser value ("url('https://abc.xyz/logo.gif?" <> b <> "abc')")
            `shouldEqual`
              Right (Value (Right [ Tuple (Just URL) [ Lit ("https://abc.xyz/logo.gif?" <> b <> "abc") ] ]))
    it "accepts a calc value"
      $ runParser value "calc(5px+1rem)"
          `shouldEqual`
            Right (Value (Right [ Tuple (Just Calc) [ Lit "5px + 1rem" ] ]))
    it "accepts a variable within a calc value"
      $ runParser value "calc(#{$space1}-1px)"
          `shouldEqual`
            Right (Value (Right [ Tuple (Just Calc) [ Var $ Variable "space1", Lit " - 1px" ] ]))
    it "accepts nested expressions within a calc value"
      $ runParser value "calc((#{$space1}/2)-1px)"
          `shouldEqual`
            Right (Value (Right [ Tuple (Just Calc) [ Lit "(", Var $ Variable "space1", Lit " / 2) - 1px" ] ]))
    it "accepts a series of quoted and unquoted segments"
      $ runParser value "'a'b'c'd"
          `shouldEqual`
            Right
              ( Value
                  ( Right
                      [ Tuple (Just Quoted) [ Lit "a" ]
                      , Tuple Nothing [ Lit "b" ]
                      , Tuple (Just Quoted) [ Lit "c" ]
                      , Tuple Nothing [ Lit "d" ]
                      ]
                  )
              )
    it "accepts a series of unquoted and URL segments"
      $ runParser value "#000__url('https://abc.xyz/bg.gif')"
          `shouldEqual`
            Right (Value (Right [ Tuple Nothing [ Lit "#000 " ], Tuple (Just URL) [ Lit "https://abc.xyz/bg.gif" ] ]))
    it "accepts a series of unquoted and calc segments"
      $ runParser value "10px__calc(1rem+1px)"
          `shouldEqual`
            Right (Value (Right [ Tuple Nothing [ Lit "10px " ], Tuple (Just Calc) [ Lit "1rem + 1px" ] ]))
  describe "property" do
    it "accepts a property consisting of lowercase letters and hyphens"
      $ runParser property "background-color" `shouldEqual` Right (Property "background-color")
    it "does not consume trailing colon"
      $ runParser property "font-size:" `shouldEqual` Right (Property "font-size")
    it "rejects capital letters" do
      runParser property "boxShadow" `shouldEqual` Right (Property "box")
      runParser property "Box-shadow" `shouldSatisfy` isLeft
    it "rejects consecutive hyphens"
      $ runParser property "max--width" `shouldEqual` Right (Property "max")
    it "rejects trailing hyphens"
      $ runParser property "max-width-" `shouldEqual` Right (Property "max-width")
    it "rejects leading hyphens"
      $ runParser property "-padding-left" `shouldSatisfy` isLeft
    it "accepts leading hyphens for vendor prefixes"
      $ for_ [ "moz", "ms", "o", "webkit" ] \v ->
          runParser property ("-" <> v <> "-transition")
            `shouldEqual`
              Right (Property $ "-" <> v <> "-transition")
    it "rejects numbers" do
      runParser property "abc-123" `shouldEqual` Right (Property "abc")
      runParser property "123-abc" `shouldSatisfy` isLeft
    it "rejects underscores" do
      runParser property "abc_123" `shouldEqual` Right (Property "abc")
      runParser property "_abc123" `shouldSatisfy` isLeft
  describe "declaration" do
    it "accepts a property followed by a colon followed by a value"
      $ runParser declaration "background:red"
          `shouldEqual`
            Right (Declaration (Tuple (Property "background") (Value (Right [ Tuple Nothing [ Lit "red" ] ]))))
    it "rejects incomplete declarations" do
      runParser declaration "background" `shouldSatisfy` isLeft
      runParser declaration "background:" `shouldSatisfy` isLeft
  describe "cls" do
    it "accepts a basic named class"
      $ runParser cls ".foo" `shouldEqual` Right (NamedClass "foo")
    it "accepts hyphens and lowercase letters within a named class"
      $ runParser cls ".foo-bar" `shouldEqual` Right (NamedClass "foo-bar")
    it "accepts numbers within a named class" do
      runParser cls ".3-layer" `shouldEqual` Right (NamedClass "3-layer")
      runParser cls ".red-500" `shouldEqual` Right (NamedClass "red-500")
    it "rejects consecutive hyphens within a named class"
      $ runParser cls ".foo--bar" `shouldEqual` Right (NamedClass "foo")
    it "rejects leading hyphen within a named class"
      $ runParser cls ".-foo" `shouldSatisfy` isLeft
    it "rejects trailing hyphen within a named class"
      $ runParser cls ".foo-" `shouldEqual` Right (NamedClass "foo")
    it "rejects underscores within a named class" do
      runParser cls ".foo_bar" `shouldEqual` Right (NamedClass "foo")
      runParser cls "._foo" `shouldSatisfy` isLeft
    it "rejects capital letters within a named class" do
      runParser cls ".fooBar" `shouldEqual` Right (NamedClass "foo")
      runParser cls ".Barfoo" `shouldSatisfy` isLeft
    it "accepts a basic pseudo-class"
      $ runParser cls ":foo" `shouldEqual` Right (PseudoClass "foo")
    it "accepts hyphens within a basic pseudo-class"
      $ runParser cls ":foo-bar" `shouldEqual` Right (PseudoClass "foo-bar")
    it "rejects consecutive hyphens within a pseudo-class"
      $ runParser cls ":foo--bar" `shouldEqual` Right (PseudoClass "foo")
    it "rejects leading hyphen within a pseudo-class"
      $ runParser cls ":-foo" `shouldSatisfy` isLeft
    it "rejects trailing hyphen within a named class"
      $ runParser cls ":foo-" `shouldEqual` Right (PseudoClass "foo")
    it "rejects capital letters within a pseudo-class" do
      runParser cls ":fooBar" `shouldEqual` Right (PseudoClass "foo")
      runParser cls ":Barfoo" `shouldSatisfy` isLeft
    it "rejects numbers within a basic pseudo-class" do
      runParser cls ":hover3" `shouldEqual` Right (PseudoClass "hover")
      runParser cls ":3hover" `shouldSatisfy` isLeft
    it "rejects underscores within a basic pseudo-class" do
      runParser cls ":focus_within" `shouldEqual` Right (PseudoClass "focus")
      runParser cls ":_hover" `shouldSatisfy` isLeft
    it "accepts leading hyphen within a vendor-prefixed pseudo-class"
      $ for_ [ "moz", "ms", "o", "webkit" ] \v ->
          runParser cls (":-" <> v <> "-foo") `shouldEqual` Right (PseudoClass $ "-" <> v <> "-foo")
    it "accepts :not() pseudo-selector containing a class list"
      $ runParser cls ":not(.err:disabled)" `shouldEqual` Right (NotPseudoClass [ NamedClass "err", PseudoClass "disabled" ])
    it "accepts nested :not() pseudo-selectors"
      $ runParser cls ":not(:not(.err))" `shouldEqual` Right (NotPseudoClass [ NotPseudoClass [ NamedClass "err" ] ])
    for_ [ "child", "last-child", "last-of-type", "of-type" ] \a ->
      it ("accepts :nth-" <> a <> "() pseudo-selectors")
        $ for_ [ "even", "odd", "2n+1", "-5n-5", "5" ] \b -> do
            runParser cls (":nth-" <> a <> "(" <> b <> ")") `shouldEqual` Right (PseudoClass $ "nth-" <> a <> "(" <> b <> ")")
            runParser cls (":not(:nth-" <> a <> "(" <> b <> "))")
              `shouldEqual`
                Right (NotPseudoClass [ PseudoClass $ "nth-" <> a <> "(" <> b <> ")" ])
    it "accepts :lang() pseudo-selectors" do
      runParser cls ":lang(en-US)" `shouldEqual` Right (PseudoClass "lang(en-US)")
      runParser cls ":not(:lang(en-US))" `shouldEqual` Right (NotPseudoClass [ PseudoClass "lang(en-US)" ])
  describe "combinator" do
    it "accepts ancestor"
      $ runParser combinator "_" `shouldEqual` Right Ancestor
    it "accepts parent"
      $ runParser combinator ">" `shouldEqual` Right Parent
    it "accepts adjacent sibling"
      $ runParser combinator "+" `shouldEqual` Right AdjSib
    it "accepts general sibling"
      $ runParser combinator "~" `shouldEqual` Right GenSib
    it "rejects unknown combinator"
      $ runParser combinator "#" `shouldSatisfy` isLeft
  describe "pseudoElement" do
    it "accepts lowercase letters and hyphens within a pseudo-element"
      $ runParser pseudoElement "::first-line" `shouldEqual` Right (PseudoElement "first-line")
    it "rejects a leading hyphen within a pseudo-element"
      $ runParser pseudoElement "::-first-line" `shouldSatisfy` isLeft
    it "rejects consecutive hyphens within a pseudo-element"
      $ runParser pseudoElement "::first--line" `shouldEqual` Right (PseudoElement "first")
    it "rejects a trailing hyphen within a pseudo-element"
      $ runParser pseudoElement "::first-line-" `shouldEqual` Right (PseudoElement "first-line")
    it "rejects a capital letter within a pseudo-element" do
      runParser pseudoElement "::FirstLine" `shouldSatisfy` isLeft
      runParser pseudoElement "::firstLine" `shouldEqual` Right (PseudoElement "first")
    it "rejects a number within a pseudo-element" do
      runParser pseudoElement "::1-first" `shouldSatisfy` isLeft
      runParser pseudoElement "::first1" `shouldEqual` Right (PseudoElement "first")
    it "rejects an underscore within a pseudo-element" do
      runParser pseudoElement "::_first" `shouldSatisfy` isLeft
      runParser pseudoElement "::first_line" `shouldEqual` Right (PseudoElement "first")
    it "accepts a vendor prefix within a pseudo-element"
      $ for_ [ "moz", "ms", "o", "webkit" ] \v ->
          runParser pseudoElement ("::-" <> v <> "-foo") `shouldEqual` Right (PseudoElement $ "-" <> v <> "-foo")
  describe "context" do
    it "accepts a class list and combinator"
      $ runParser context ".foo:hover+" `shouldEqual` Right (Context $ Tuple [ NamedClass "foo", PseudoClass "hover" ] AdjSib)
    it "rejects a class without a combinator"
      $ runParser context ".foo" `shouldSatisfy` isLeft
    it "rejects a combinator without a class"
      $ runParser context "_" `shouldSatisfy` isLeft
  describe "selector" do
    it "accepts a context-only selector"
      $ runParser selector ".parent>"
          `shouldEqual`
            Right ((selectorContext .~ Just (Context (Tuple [ NamedClass "parent" ] Parent))) emptySelector)
    it "accepts a class-only selector"
      $ runParser selector ".parent"
          `shouldEqual`
            Right ((selectorClasses .~ [ NamedClass "parent" ]) emptySelector)
    it "accepts a pseudo-element-only selector"
      $ runParser selector "::foo"
          `shouldEqual`
            Right ((selectorPseudoElement .~ Just (PseudoElement "foo")) emptySelector)
    it "accepts a selector with all elements"
      $ runParser selector ".err_:focus::after"
          `shouldEqual`
            Right
              ( emptySelector # selectorContext .~ Just (Context $ Tuple [ NamedClass "err" ] Ancestor)
                  # selectorClasses
                  .~ [ PseudoClass "focus" ]
                  # selectorPseudoElement
                  .~ Just (PseudoElement "after")
              )
    it "rejects a non-selector"
      $ runParser selector "foo" `shouldSatisfy` isLeft
  describe "at-scope" do
    it "accepts an at-scope consisting of lowercase letters, numbers, and hypens"
      $ runParser atScope "@foo-bar3" `shouldEqual` Right (AtScope "foo-bar3")
    it "rejects capital letters within an at-scope" do
      runParser atScope "@fooBar" `shouldEqual` Right (AtScope "foo")
      runParser atScope "@Foobar" `shouldSatisfy` isLeft
    it "rejects underscores within an at-scope" do
      runParser atScope "@foo_bar" `shouldEqual` Right (AtScope "foo")
      runParser atScope "@_foobar" `shouldSatisfy` isLeft
    it "rejects consecutive hyphens within an at-scope"
      $ runParser atScope "@foo--bar" `shouldEqual` Right (AtScope "foo")
    it "rejects leading hyphens within an at-scope"
      $ runParser atScope "@-foo" `shouldSatisfy` isLeft
    it "rejects trailing hyphens within an at-scope"
      $ runParser atScope "@foo-" `shouldEqual` Right (AtScope "foo")
  describe "priority"
    $ it "parses a priority"
    $ runParser priority "!!!" `shouldEqual` Right (Priority 3)
  describe "rule" do
    it "parses a rule with all elements"
      $ runParser rule "@sm{.cell{padding:0;margin:0}}"
          `shouldEqual`
            Right
              ( emptyRule # ruleAtScope .~ Just (AtScope "sm")
                  # ruleSelector
                  .~ Just (emptySelector # selectorClasses .~ [ NamedClass "cell" ])
                  # ruleDeclarations
                  .~ ( Declaration
                        <$> [ Tuple
                              (Property "padding")
                              (Value $ Right [ Tuple Nothing [ Lit "0" ] ])
                          , Tuple
                              (Property "margin")
                              (Value $ Right [ Tuple Nothing [ Lit "0" ] ])
                          ]
                    )
              )
    it "parses a rule with an at-scope and no selector"
      $ runParser rule "@sm{padding:0;margin:0}"
          `shouldEqual`
            Right
              ( emptyRule # ruleAtScope .~ Just (AtScope "sm")
                  # ruleDeclarations
                  .~ ( Declaration
                        <$> [ Tuple
                              (Property "padding")
                              (Value $ Right [ Tuple Nothing [ Lit "0" ] ])
                          , Tuple
                              (Property "margin")
                              (Value $ Right [ Tuple Nothing [ Lit "0" ] ])
                          ]
                    )
              )
    it "parses a rule with a selector and no at-scope"
      $ runParser rule ".cell{padding:0;margin:0}"
          `shouldEqual`
            Right
              ( emptyRule # ruleSelector .~ Just (emptySelector # selectorClasses .~ [ NamedClass "cell" ])
                  # ruleDeclarations
                  .~ ( Declaration
                        <$> [ Tuple
                              (Property "padding")
                              (Value $ Right [ Tuple Nothing [ Lit "0" ] ])
                          , Tuple
                              (Property "margin")
                              (Value $ Right [ Tuple Nothing [ Lit "0" ] ])
                          ]
                    )
              )
    it "parses a rule with declarations only"
      $ runParser rule "padding:0;margin:0;"
          `shouldEqual`
            Right
              ( emptyRule # ruleDeclarations
                  .~ ( Declaration
                        <$> [ Tuple
                              (Property "padding")
                              (Value $ Right [ Tuple Nothing [ Lit "0" ] ])
                          , Tuple
                              (Property "margin")
                              (Value $ Right [ Tuple Nothing [ Lit "0" ] ])
                          ]
                    )
              )
    it "parses a rule with all elements and priority"
      $ runParser rule "@sm{.cell{padding:0;margin:0}}!!"
          `shouldEqual`
            Right
              ( emptyRule # ruleAtScope .~ Just (AtScope "sm")
                  # ruleSelector
                  .~ Just (emptySelector # selectorClasses .~ [ NamedClass "cell" ])
                  # ruleDeclarations
                  .~ ( Declaration
                        <$> [ Tuple
                              (Property "padding")
                              (Value $ Right [ Tuple Nothing [ Lit "0" ] ])
                          , Tuple
                              (Property "margin")
                              (Value $ Right [ Tuple Nothing [ Lit "0" ] ])
                          ]
                    )
                  # rulePriority
                  .~ Priority 2
              )
    it "parses a rule with an at-scope and no selector and priority"
      $ runParser rule "@sm{padding:0;margin:0}!!"
          `shouldEqual`
            Right
              ( emptyRule # ruleAtScope .~ Just (AtScope "sm")
                  # ruleDeclarations
                  .~ ( Declaration
                        <$> [ Tuple
                              (Property "padding")
                              (Value $ Right [ Tuple Nothing [ Lit "0" ] ])
                          , Tuple
                              (Property "margin")
                              (Value $ Right [ Tuple Nothing [ Lit "0" ] ])
                          ]
                    )
                  # rulePriority
                  .~ Priority 2
              )
    it "parses a rule with a selector and no at-scope and priority"
      $ runParser rule ".cell{padding:0;margin:0}!!!!"
          `shouldEqual`
            Right
              ( emptyRule # ruleSelector .~ Just (emptySelector # selectorClasses .~ [ NamedClass "cell" ])
                  # ruleDeclarations
                  .~ ( Declaration
                        <$> [ Tuple
                              (Property "padding")
                              (Value $ Right [ Tuple Nothing [ Lit "0" ] ])
                          , Tuple
                              (Property "margin")
                              (Value $ Right [ Tuple Nothing [ Lit "0" ] ])
                          ]
                    )
                  # rulePriority
                  .~ Priority 4
              )
    it "parses a rule with declarations and priority"
      $ runParser rule "padding:0;margin:0;!!"
          `shouldEqual`
            Right
              ( emptyRule # ruleDeclarations
                  .~ ( Declaration
                        <$> [ Tuple
                              (Property "padding")
                              (Value $ Right [ Tuple Nothing [ Lit "0" ] ])
                          , Tuple
                              (Property "margin")
                              (Value $ Right [ Tuple Nothing [ Lit "0" ] ])
                          ]
                    )
                  # rulePriority
                  .~ Priority 2
              )
