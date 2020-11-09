module Test.Hacss.Parser where

import Prelude
import Data.Either (Either(Right), isLeft)
import Data.Maybe (Maybe(..))
import Data.Traversable (for_)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Test.Spec (SpecT, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Hacss.Data (AtScope(..), ClassList(..), ClassName(..), Combinator(..), Context(..), Property(..), PseudoClass(..), PseudoElement(..), Selector(..), ValContext(..), ValExpr(..), Value(..), Variable(..))
import Hacss.Parser (runParseM, atScope, className, combinator, context, declaration, priority, property, pseudoClass, pseudoElement, rule, selector, value)

tests :: forall m. Monad m => SpecT Aff Unit m Unit
tests = do
  describe "value" do
    it "accepts a simple value"
      $ runParseM value { knownVariables: [] } "red"
          `shouldEqual`
            Right (Value [ Tuple Nothing [ Lit "red" ] ])
    it "accepts a variable value"
      $ runParseM value { knownVariables: [ Variable "red500" ] } "$red500"
          `shouldEqual`
            Right (Value [ Tuple Nothing [ Var (Variable "red500") ] ])
    it "accepts a simple value containing variables"
      $ runParseM value { knownVariables: [ Variable "red500" ] } "as$red500df"
          `shouldEqual`
            Right (Value [ Tuple Nothing [ Lit "as", Var (Variable "red500"), Lit "df" ] ])
    it "excludes semicolons in simple values"
      $ runParseM value { knownVariables: [] } "asdf;fdsa"
          `shouldEqual`
            Right (Value [ Tuple Nothing [ Lit "asdf" ] ])
    it "excludes closing curly braces in simple values"
      $ runParseM value { knownVariables: [] } "asdf}fdsa"
          `shouldEqual`
            Right (Value [ Tuple Nothing [ Lit "asdf" ] ])
    it "excludes white space" do
      runParseM value { knownVariables: [] } "asdf fdsa"
        `shouldEqual`
          Right (Value [ Tuple Nothing [ Lit "asdf" ] ])
      runParseM value { knownVariables: [] } "'asdf fdsa'"
        `shouldEqual`
          Right (Value [ Tuple Nothing [ Lit "'asdf" ] ])
      runParseM value { knownVariables: [] } "url('asdf fdsa')"
        `shouldEqual`
          Right (Value [ Tuple Nothing [ Lit "url('asdf" ] ])
      runParseM value { knownVariables: [] } "calc(20% + 5px)"
        `shouldEqual`
          Right (Value [ Tuple Nothing [ Lit "calc(20%" ] ])
    it "accepts semicolons between quotes"
      $ runParseM value { knownVariables: [] } "'red;blue'"
          `shouldEqual`
            Right (Value [ Tuple (Just Quoted) [ Lit "red;blue" ] ])
    it "accepts closing curly braces between quotes"
      $ runParseM value { knownVariables: [] } "'red}blue'"
          `shouldEqual`
            Right (Value [ Tuple (Just Quoted) [ Lit "red}blue" ] ])
    it "accepts empty quotes"
      $ runParseM value { knownVariables: [] } "''"
          `shouldEqual`
            Right (Value [ Tuple Nothing [ Lit "''" ] ])
    it "accepts a combination of quoted and unquoted segments" do
      runParseM value { knownVariables: [] } "'foo'asdf'bar'"
        `shouldEqual`
          Right (Value [ Tuple (Just Quoted) [ Lit "foo" ], Tuple Nothing [ Lit "asdf" ], Tuple (Just Quoted) [ Lit "bar" ] ])
      runParseM value { knownVariables: [] } "asdf'foo'bar"
        `shouldEqual`
          Right (Value [ Tuple Nothing [ Lit "asdf" ], Tuple (Just Quoted) [ Lit "foo" ], Tuple Nothing [ Lit "bar" ] ])
    it "accepts a URL value"
      $ runParseM value { knownVariables: [] } "url('https://abc.xyz/foo.jpg')"
          `shouldEqual`
            Right (Value [ Tuple (Just URL) [ Lit "https://abc.xyz/foo.jpg" ] ])
    it "accepts a value that includes a URL"
      $ runParseM value { knownVariables: [] } "#000__url('http://bomb.com/xyz.gif')__no-repeat"
          `shouldEqual`
            Right
              ( Value
                  [ Tuple Nothing [ Lit "#000 " ]
                  , Tuple (Just URL) [ Lit "http://bomb.com/xyz.gif" ]
                  , Tuple Nothing [ Lit " no-repeat" ]
                  ]
              )
    it "parses variables within URL values"
      $ runParseM value { knownVariables: [ Variable "foo" ] } "url('http://xy.z/$foo.gif')"
          `shouldEqual`
            Right (Value [ Tuple (Just URL) [ Lit "http://xy.z/", Var (Variable "foo"), Lit ".gif" ] ])
    it "accepts a calc value"
      $ runParseM value { knownVariables: [] } "calc((100%/3)+16px)"
          `shouldEqual`
            Right (Value [ Tuple (Just Calc) [ Lit "(100% / 3) + 16px" ] ])
    it "accepts a value that includes a calc expression"
      $ runParseM value { knownVariables: [] } "1rem__calc(1rem+2px)__1rem"
          `shouldEqual`
            Right
              ( Value
                  [ Tuple Nothing [ Lit "1rem " ]
                  , Tuple (Just Calc) [ Lit "1rem + 2px" ]
                  , Tuple Nothing [ Lit " 1rem" ]
                  ]
              )
    it "parses variables within calc expressions"
      $ runParseM value { knownVariables: [ Variable "foo" ] } "calc(1rem+$foo+1px)"
          `shouldEqual`
            Right (Value [ Tuple (Just Calc) [ Lit "1rem + ", Var (Variable "foo"), Lit " + 1px" ] ])
    it "parses double underscores as white space within simple/quoted values"
      $ runParseM value { knownVariables: [] } "a__b__'c__d'__e"
          `shouldEqual`
            Right
              ( Value
                  [ Tuple Nothing [ Lit "a b " ]
                  , Tuple (Just Quoted) [ Lit "c d" ]
                  , Tuple Nothing [ Lit " e" ]
                  ]
              )
    it "parses double underscores as white space within URL values"
      $ runParseM value { knownVariables: [] } "url('http://xy.z/logo__1.gif')"
          `shouldEqual`
            Right (Value [ Tuple (Just URL) [ Lit "http://xy.z/logo 1.gif" ] ])
  describe "property" do
    it "parses a known property"
      $ runParseM property { knownProperties: [ Property "background" ] } "background"
          `shouldEqual`
            Right (Property "background")
    it "parses the most specific known property"
      $ runParseM property { knownProperties: Property <$> [ "background", "background-color" ] } "background-color"
          `shouldEqual`
            Right (Property "background-color")
    it "rejects an unknown property"
      $ runParseM property { knownProperties: [] } "foo"
          `shouldSatisfy`
            isLeft
  describe "className" do
    it "accepts lowercase letters and hyphens following a '.'"
      $ runParseM className unit ".abc-def"
          `shouldEqual`
            Right (ClassName "abc-def")
    it "accepts a single lowercase letter following a '.'"
      $ runParseM className unit ".a" `shouldEqual` Right (ClassName "a")
    it "rejects a value that does not begin with '.'"
      $ runParseM className unit "asdf" `shouldSatisfy` isLeft
    it "rejects leading hyphen"
      $ runParseM className unit ".-asdf" `shouldSatisfy` isLeft
    it "rejects trailing hyphen"
      $ runParseM className unit ".abc-" `shouldEqual` Right (ClassName "abc")
    it "rejects characters other than lowercase letters and hyphens" do
      runParseM className unit ".abc_123" `shouldEqual` Right (ClassName "abc")
  describe "pseudoClass" do
    it "accepts basic pseudo-classes"
      $ for_
          [ "active"
          , "checked"
          , "default"
          , "defined"
          , "disabled"
          , "empty"
          , "enabled"
          , "first"
          , "first-child"
          , "first-of-type"
          , "focus"
          , "focus-within"
          , "hover"
          , "indeterminate"
          , "in-range"
          , "invalid"
          , "last-child"
          , "last-of-type"
          , "left"
          , "link"
          , "only-child"
          , "only-of-type"
          , "optional"
          , "out-of-range"
          , "picture-in-picture"
          , "read-only"
          , "read-write"
          , "required"
          , "right"
          , "root"
          , "scope"
          , "target"
          , "valid"
          , "visited"
          ] \x ->
          runParseM pseudoClass unit (":" <> x)
            `shouldEqual`
              Right (BasicPseudoClass x)
    it "accepts :lang() pseudo-class" do
      runParseM pseudoClass unit ":lang(en)"
        `shouldEqual`
          Right (BasicPseudoClass "lang(en)")
    it "accepts uppercase letters and hyphens in :lang() pseudo-class"
      $ runParseM pseudoClass unit ":lang(en-US)"
          `shouldEqual`
            Right (BasicPseudoClass "lang(en-US)")
    it "rejects a leading hyphen in :lang() pseudo-class"
      $ runParseM pseudoClass unit ":lang(-US)" `shouldSatisfy` isLeft
    it "rejects a trailing hyphen in :lang() pseudo-class"
      $ runParseM pseudoClass unit ":lang(en-)" `shouldSatisfy` isLeft
    for_ [ "child", "last-child", "last-of-type", "of-type" ] \x -> do
      for_ [ "1", "-n+2", "2n+3", "even", "odd" ] \y ->
        it ("accepts :nth-" <> x <> "(" <> y <> ") pseudo-class")
          $ runParseM pseudoClass unit (":nth-" <> x <> "(" <> y <> ")")
              `shouldEqual`
                Right (BasicPseudoClass $ "nth-" <> x <> "(" <> y <> ")")
      for_ [ "foo", "-", "+", "2n+" ] \y ->
        it ("rejects :nth-" <> x <> "(" <> y <> ") pseudo-class")
          $ runParseM pseudoClass unit (":nth-" <> x <> "(" <> y <> ")")
              `shouldSatisfy`
                isLeft
    it "accepts :not() pseudo-class"
      $ runParseM pseudoClass unit ":not(.a.b:not(:focus):hover)"
          `shouldEqual`
            Right (NotPseudoClass (ClassList $ Tuple (ClassName <$> [ "a", "b" ]) [ NotPseudoClass (ClassList $ Tuple [] [ BasicPseudoClass "focus" ]), BasicPseudoClass "hover" ]))
    it "rejects unknown pseudo-classes"
      $ runParseM pseudoClass unit "foo" `shouldSatisfy` isLeft
  describe "pseudoElement" do
    it "accepts standard pseudo-element"
      $ for_
          [ "after"
          , "before"
          , "first-letter"
          , "first-line"
          , "placeholder"
          , "selection"
          ] \x ->
          runParseM pseudoElement unit ("::" <> x)
            `shouldEqual`
              Right (PseudoElement x)
    it "accepts arbitrary browser-prefixed pseudo-element"
      $ for_ [ "moz", "ms", "o", "webkit" ] \p ->
          runParseM pseudoElement unit ("::-" <> p <> "-foo-bar")
            `shouldEqual`
              Right (PseudoElement $ "-" <> p <> "-foo-bar")
    it "rejects adjacent hyphens"
      $ for_ [ "moz", "ms", "o", "webkit" ] \p ->
          runParseM pseudoElement unit ("::-" <> p <> "-foo--bar")
            `shouldSatisfy`
              isLeft
    it "rejects leading hyphens"
      $ for_ [ "moz", "ms", "o", "webkit" ] \p ->
          runParseM pseudoElement unit ("::-" <> p <> "--foo-bar")
            `shouldSatisfy`
              isLeft
    it "rejects trailing hyphens"
      $ for_ [ "moz", "ms", "o", "webkit" ] \p ->
          runParseM pseudoElement unit ("::-" <> p <> "-foo-bar-")
            `shouldSatisfy`
              isLeft
    it "rejects arbitrary pseudo-elements"
      $ runParseM pseudoElement unit "::foo-bar" `shouldSatisfy` isLeft
  describe "combinator" do
    it "parses '~' as a general sibling"
      $ runParseM combinator unit "~" `shouldEqual` Right GeneralSibling
    it "parses '+' as an adjacent sibling"
      $ runParseM combinator unit "+" `shouldEqual` Right AdjacentSibling
    it "parses '>' as a parent"
      $ runParseM combinator unit ">" `shouldEqual` Right Parent
    it "parses '_' as an ancestor"
      $ runParseM combinator unit "_" `shouldEqual` Right Ancestor
  describe "context" do
    it "parses a context including class list and combinator"
      $ runParseM context unit ".foo:focus~"
          `shouldEqual`
            Right (Context $ Tuple (ClassList $ Tuple [ ClassName "foo" ] [ BasicPseudoClass "focus" ]) GeneralSibling)
    it "rejects something that is not a context"
      $ runParseM context unit "foo::focus" `shouldSatisfy` isLeft
  describe "selector" do
    it "parses a context"
      $ runParseM selector unit ":checked+"
          `shouldEqual`
            (Right $ Selector { context: Just (Context $ Tuple (ClassList $ Tuple [] [ BasicPseudoClass "checked" ]) AdjacentSibling), classes: Nothing, pseudoElement: Nothing })
    it "parses a class list"
      $ runParseM selector unit ".foo.bar"
          `shouldEqual`
            (Right $ Selector { context: Nothing, classes: Just $ ClassList $ Tuple (ClassName <$> [ "foo", "bar" ]) [], pseudoElement: Nothing })
    it "parses a pseudo-element"
      $ runParseM selector unit "::after"
          `shouldEqual`
            (Right $ Selector { context: Nothing, classes: Nothing, pseudoElement: Just $ PseudoElement "after" })
    it "parses a context with a class list"
      $ runParseM selector unit ".foo+:hover"
          `shouldEqual`
            (Right $ Selector { context: Just (Context $ Tuple (ClassList $ Tuple [ ClassName "foo" ] []) AdjacentSibling), classes: Just $ ClassList $ Tuple [] [ BasicPseudoClass "hover" ], pseudoElement: Nothing })
    it "parses a class list with a pseudo-element"
      $ runParseM selector unit ":hover:disabled::after"
          `shouldEqual`
            (Right $ Selector { context: Nothing, classes: Just $ ClassList $ Tuple [] $ BasicPseudoClass <$> [ "hover", "disabled" ], pseudoElement: Just $ PseudoElement "after" })
    it "parses a context with a pseudo-element"
      $ runParseM selector unit ".err>::before"
          `shouldEqual`
            (Right $ Selector { context: Just (Context $ Tuple (ClassList $ Tuple [ ClassName "err" ] []) Parent), classes: Nothing, pseudoElement: Just $ PseudoElement "before" })
    it "parses a context with all components"
      $ runParseM selector unit ":disabled_.err::placeholder"
          `shouldEqual`
            (Right $ Selector { context: Just (Context $ Tuple (ClassList $ Tuple [] [ BasicPseudoClass "disabled" ]) Ancestor), classes: Just $ ClassList $ Tuple [ ClassName "err" ] [], pseudoElement: Just $ PseudoElement "placeholder" })
  describe "atScope" do
    it "accepts a known at-scope"
      $ runParseM atScope { knownAtScopes: AtScope <$> [ "lg", "sm" ] } "@sm"
          `shouldEqual`
            Right (AtScope "sm")
    it "rejects an unknown at-scope"
      $ runParseM atScope { knownAtScopes: [] } "@sm" `shouldSatisfy` isLeft
  describe "priority"
    $ it "increases by one for each '!' instance" do
        runParseM priority unit "!" `shouldEqual` Right 1
        runParseM priority unit "!!!!" `shouldEqual` Right 4
  describe "declaration" do
    it "accepts a declaration in the form of property:value"
      $ runParseM declaration { knownProperties: Property <$> [ "color" ], knownVariables: [] } "color:red"
          `shouldEqual`
            Right (Tuple (Property "color") (Value [ Tuple Nothing [ Lit "red" ] ]))
    it "rejects an incomplete declaration missing property"
      $ runParseM declaration { knownProperties: Property <$> [ "color" ], knownVariables: [] } ":red"
          `shouldSatisfy`
            isLeft
    it "rejects an incomplete declaration missing value"
      $ runParseM declaration { knownProperties: Property <$> [ "color" ], knownVariables: [] } "color:"
          `shouldSatisfy`
            isLeft
  describe "rule" do
    it "accepts a declarations-only rule"
      $ runParseM rule { knownAtScopes: [], knownProperties: Property <$> [ "background", "color" ], knownVariables: [] } "background:blue;color:white;"
          `shouldEqual`
            Right { atScope: Nothing, selector: Nothing, declarations: [ Tuple (Property "background") (Value [ Tuple Nothing [ Lit "blue" ] ]), Tuple (Property "color") (Value [ Tuple Nothing [ Lit "white" ] ]) ], priority: 0 }
    it "accepts a declarations-only rule with priority"
      $ runParseM rule { knownAtScopes: [], knownProperties: Property <$> [ "background", "color" ], knownVariables: [] } "background:blue;!!!"
          `shouldEqual`
            Right { atScope: Nothing, selector: Nothing, declarations: [ Tuple (Property "background") (Value [ Tuple Nothing [ Lit "blue" ] ]) ], priority: 3 }
    it "accepts a rule with a selector"
      $ runParseM rule { knownAtScopes: [], knownProperties: Property <$> [ "color" ], knownVariables: [] } ".err{color:red}"
          `shouldEqual`
            Right { atScope: Nothing, selector: Just $ Selector { context: Nothing, classes: Just $ ClassList $ Tuple [ ClassName "err" ] [], pseudoElement: Nothing }, declarations: [ Tuple (Property "color") (Value [ Tuple Nothing [ Lit "red" ] ]) ], priority: 0 }
    it "accepts a rule with a selector and priority"
      $ runParseM rule { knownAtScopes: [], knownProperties: Property <$> [ "color" ], knownVariables: [] } ".err{color:red}!"
          `shouldEqual`
            Right { atScope: Nothing, selector: Just $ Selector { context: Nothing, classes: Just $ ClassList $ Tuple [ ClassName "err" ] [], pseudoElement: Nothing }, declarations: [ Tuple (Property "color") (Value [ Tuple Nothing [ Lit "red" ] ]) ], priority: 1 }
    it "accepts a rule with an at-scope"
      $ runParseM rule { knownAtScopes: [ AtScope "sm" ], knownProperties: Property <$> [ "color" ], knownVariables: [] } "@sm{color:red}"
          `shouldEqual`
            Right { atScope: Just $ AtScope "sm", selector: Nothing, declarations: [ Tuple (Property "color") (Value [ Tuple Nothing [ Lit "red" ] ]) ], priority: 0 }
    it "accepts a rule with an at-scope and priority"
      $ runParseM rule { knownAtScopes: [ AtScope "sm" ], knownProperties: Property <$> [ "color" ], knownVariables: [] } "@sm{color:red}!!!"
          `shouldEqual`
            Right { atScope: Just $ AtScope "sm", selector: Nothing, declarations: [ Tuple (Property "color") (Value [ Tuple Nothing [ Lit "red" ] ]) ], priority: 3 }
    it "accepts a rule with an at-scope and a selector"
      $ runParseM rule { knownAtScopes: [ AtScope "sm" ], knownProperties: [ Property "color" ], knownVariables: [] } "@sm{.err{color:red}}"
          `shouldEqual`
            Right { atScope: Just $ AtScope "sm", selector: Just $ Selector { context: Nothing, classes: Just $ ClassList $ Tuple [ ClassName "err" ] [], pseudoElement: Nothing }, declarations: [ Tuple (Property "color") (Value [ Tuple Nothing [ Lit "red" ] ]) ], priority: 0 }
    it "accepts a rule with an at-scope, a selector, and a priority"
      $ runParseM rule { knownAtScopes: [ AtScope "sm" ], knownProperties: [ Property "color" ], knownVariables: [] } "@sm{.err{color:red}}!"
          `shouldEqual`
            Right { atScope: Just $ AtScope "sm", selector: Just $ Selector { context: Nothing, classes: Just $ ClassList $ Tuple [ ClassName "err" ] [], pseudoElement: Nothing }, declarations: [ Tuple (Property "color") (Value [ Tuple Nothing [ Lit "red" ] ]) ], priority: 1 }
