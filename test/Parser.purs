module Test.Hacss.Parser where

import Prelude
import Control.Monad.Reader (runReader)
import Data.Array ((..))
import Data.Array (filter) as A
import Data.Char.Unicode (isLetter, isLower)
import Data.Either (Either(..), isLeft)
import Data.Enum (toEnumWithDefaults)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (length, singleton, toCharArray) as S
import Data.Traversable (for_, traverse_)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Test.Spec (SpecT, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Text.Parsing.Parser (runParser, runParserT)
import Hacss.Data
  ( AtScope(..)
  , ClassName(..)
  , Combinator(..)
  , Declaration(..)
  , Property(..)
  , PseudoClass(..)
  , PseudoElement(..)
  , QualifiedVal(..)
  , ValExpr(..)
  , ValSeg(..)
  , Value(..)
  , Variable(..)
  )
import Hacss.Parser
  ( atScope
  , className
  , combinator
  , context
  , declaration
  , property
  , pseudoClass
  , pseudoElement
  , rule
  , rules
  , selector
  , value
  )

tests :: forall m. Monad m => SpecT Aff Unit m Unit
tests = do
  describe "className" do
    it "should allow a '.' followed by series of lowercase or '-'"
      $ runParser ".primary-button" className
          `shouldEqual`
            Right (ClassName "primary-button")
    it "should allow a single lowercase letter"
      $ runParser ".p" className `shouldEqual` Right (ClassName "p")
    it "should reject without a leading '.'"
      $ runParser "a" className `shouldSatisfy` isLeft
    it "should reject a leading '-'"
      $ runParser ".-primary" className `shouldSatisfy` isLeft
    it "should reject a trailing '-'"
      $ runParser ".primary-" className `shouldSatisfy` isLeft
    it "should reject characters other than lowercase or '-'" do
      let
        chars = toEnumWithDefaults bottom top <$> 33 .. 126
      let
        invalidChars = A.filter (\c -> c /= '-' && not (isLower c)) chars
      let
        xs = S.singleton <$> invalidChars
      for_ ((\x -> [ ".a" <> x, "." <> x <> "a", ".a" <> x <> "b" ]) =<< xs) \x ->
        runParser x className
          `shouldSatisfy`
            case _ of
              Left _ -> true
              Right (ClassName y) -> S.length y + 1 < S.length x
  describe "pseudoClass" do
    it "should allow basic pseudo-classes"
      $ for_
          [ "active"
          , "checked"
          , "disabled"
          , "empty"
          , "enabled"
          , "first-child"
          , "first-of-type"
          , "focus"
          , "focus-within"
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
          ] \x ->
          runParser (":" <> x) pseudoClass
            `shouldEqual`
              Right (PseudoClass x)
  it "should allow :lang(...) selectors"
    $ for_ [ "lang(en)", "lang(zh-Hans)" ] \x ->
        runParser (":" <> x) pseudoClass `shouldEqual` Right (PseudoClass x)
  for_ [ "child", "last-child", "last-of-type", "of-type" ] \x -> do
    for_ [ "even", "odd" ] \eo ->
      it ("should allow :nth-" <> x <> "(" <> eo <> ") selector")
        $ runParser (":nth-" <> x <> "(" <> eo <> ")") pseudoClass
            `shouldEqual`
              Right (PseudoClass $ "nth-" <> x <> "(" <> eo <> ")")
    it ("should allow formulas in :nth-" <> x <> "(...) selectors")
      $ for_
          [ "1", "-1", "n+1", "n-1", "-n+1", "-n-1", "-2n", "-2n+1", "2n", "-2n" ] \formula ->
          runParser (":nth-" <> x <> "(" <> formula <> ")") pseudoClass
            `shouldEqual`
              Right (PseudoClass $ "nth-" <> x <> "(" <> formula <> ")")
  it "should allow basic :not(...) selectors" do
    runParser ":not(:focus)" pseudoClass
      `shouldEqual`
        Right (PseudoClassNot ({ classNames: [], pseudoClasses: [ PseudoClass "focus" ] }))
    runParser ":not(.foo:nth-child(2n+1))" pseudoClass
      `shouldEqual`
        Right
          ( PseudoClassNot
              ( { classNames: [ ClassName "foo" ]
                , pseudoClasses: [ PseudoClass "nth-child(2n+1)" ]
                }
              )
          )
  it "should allow nested :not(...) selectors"
    $ runParser ":not(:not(:focus))" pseudoClass
        `shouldEqual`
          Right
            ( PseudoClassNot
                ( { classNames: []
                  , pseudoClasses:
                      [ PseudoClassNot
                          ( { classNames: []
                            , pseudoClasses: [ PseudoClass "focus" ]
                            }
                          )
                      ]
                  }
                )
            )
  it "should reject :lang(...) selectors with invalid characters"
    $ for_
        ( let
            chars = toEnumWithDefaults bottom top <$> 33 .. 126

            nonLetters = A.filter (\c -> not (isLetter c) && c /= ')' && c /= '-') chars
          in
            do
              x <- S.singleton <$> nonLetters
              (\s -> ":lang(" <> s <> ")") <$> [ "a" <> x, x <> "a", "a" <> x <> "b" ]
        ) \c -> runParser c pseudoClass `shouldSatisfy` isLeft
  it "should reject :lang(...) selectors with adjacent hyphens"
    $ runParser ":lang(a--b)" pseudoClass `shouldSatisfy` isLeft
  it "should reject unrecognized pseudo-classes"
    $ runParser ":hello-world" pseudoClass `shouldSatisfy` isLeft
  describe "pseudoElement" do
    it "should allow standard pseudo-elements"
      $ for_
          [ "after"
          , "before"
          , "first-letter"
          , "first-line"
          , "marker"
          , "placeholder"
          , "selection"
          ] \x ->
          runParser ("::" <> x) pseudoElement
            `shouldEqual`
              Right (PseudoElement x)
    it "should allow arbitrary vendor-prefixed pseudo-elements"
      $ traverse_
          (\x -> runParser ("::-" <> x) pseudoElement `shouldEqual` Right (PseudoElement $ "-" <> x))
          ((\v e -> v <> "-" <> e) <$> [ "moz", "ms", "o", "webkit" ] <*> [ "foo", "foo-bar" ])
    it "should reject non-standard unprefixed pseudo-elements"
      $ runParser "::asdf" pseudoElement `shouldSatisfy` isLeft
    it "should reject unknown vendor prefixes"
      $ runParser "::-a-foo" pseudoElement `shouldSatisfy` isLeft
  describe "combinator" do
    it "should allow ancestor combinators"
      $ runParser "_" combinator `shouldEqual` Right Ancestor
    it "should allow parent combinators"
      $ runParser ">" combinator `shouldEqual` Right Parent
    it "should allow adjacent-sibling combinators"
      $ runParser "+" combinator `shouldEqual` Right AdjacentSibling
    it "should allow general-sibling combinators"
      $ runParser "~" combinator `shouldEqual` Right GeneralSibling
    it "should reject characters that are not combinators"
      $ for_
          (S.toCharArray "!@#$%^&*(){}\\|.,`") \x -> runParser (S.singleton x) combinator `shouldSatisfy` isLeft
  describe "atScope" do
    it "should allow known at-scopes"
      $ for_ [ "foo", "a-b" ] \x ->
          runReader
            (runParserT ("@" <> x) atScope)
            { knownAtScopes: AtScope <$> [ "x", x ] }
            `shouldEqual`
              Right (AtScope x)
    it "should reject unknown at-scopes"
      $ runReader (runParserT "sm" atScope) { knownAtScopes: AtScope <$> [ "lg" ] }
          `shouldSatisfy`
            isLeft
  describe "property" do
    it "should allow known properties"
      $ for_ [ "foo", "a-b" ] \x ->
          runReader (runParserT x property) { knownProperties: Property <$> [ "x", x ] }
            `shouldEqual`
              Right (Property x)
    it "should handle properties that begin with the same substring"
      $ runReader
          (runParserT "background-image" property)
          { knownProperties: Property <$> [ "background", "background-image" ] }
          `shouldEqual`
            Right (Property "background-image")
    it "should reject unknown properties"
      $ runReader (runParserT "x" property) { knownProperties: Property <$> [ "y" ] }
          `shouldSatisfy`
            isLeft
  describe "value" do
    it "should allow a calc expression"
      $ runReader
          (runParserT "calc((1px+$width)/2-1rem*1.5)" value)
          { knownVariables: [ Variable "width" ] }
          `shouldEqual`
            Right (Value [ Calc [ Lit "(1px + ", Var $ Variable "width", Lit ") / 2 - 1rem * 1.5" ] ])
    it "should parse unknown variables as literals within a calc expression"
      $ runReader
          (runParserT "calc((1px+$width)-1rem)" value)
          { knownVariables: [] }
          `shouldEqual`
            Right (Value [ Calc [ Lit "(1px + $width) - 1rem" ] ])
    it "should allow an unquoted URL"
      $ runReader
          (runParserT "url(https://acme.co/$logo.gif)" value)
          { knownVariables: [ Variable "logo" ] }
          `shouldEqual`
            Right
              ( Value
                  ( URL
                      <$> [ Unquoted
                            [ Lit "https://acme.co/"
                            , Var $ Variable "logo"
                            , Lit ".gif"
                            ]
                        ]
                  )
              )
    it "should allow a quoted URL"
      $ runReader
          (runParserT "url('https://acme.co/$logo.gif')" value)
          { knownVariables: [ Variable "logo" ] }
          `shouldEqual`
            Right
              ( Value
                  ( URL
                      <$> [ Quoted
                            [ Lit "https://acme.co/"
                            , Var $ Variable "logo"
                            , Lit ".gif"
                            ]
                        ]
                  )
              )
    it "should allow additional characters within a quoted URL"
      $ runReader (runParserT "url('a;b}c)d')" value) { knownVariables: [] }
          `shouldEqual`
            Right (Value (URL <$> [ Quoted [ Lit "a;b}c)d" ] ]))
    it "should allow quoted and unquoted value segments"
      $ runReader (runParserT "red'yellow'green'blue'" value) { knownVariables: [] }
          `shouldEqual`
            Right
              ( Value
                  ( Simple
                      <$> [ Unquoted [ Lit "red" ]
                        , Quoted [ Lit "yellow" ]
                        , Unquoted [ Lit "green" ]
                        , Quoted [ Lit "blue" ]
                        ]
                  )
              )
    it "should allow additional characters within a quoted value"
      $ runReader (runParserT "'a;b}c)d'" value) { knownVariables: [] }
          `shouldEqual`
            Right (Value (Simple <$> [ Quoted [ Lit "a;b}c)d" ] ]))
    it "should stop before an unquoted semicolon or curly brace"
      $ for_ ((\x -> "re" <> x <> "d") <$> [ ";", "}" ]) \x ->
          runReader
            (runParserT x value)
            { knownVariables: [] }
            `shouldEqual`
              Right (Value [ Simple $ Unquoted [ Lit "re" ] ])
    it "should convert double-underscore to space in a quoted URL"
      $ runReader (runParserT "url('a__b')" value) { knownVariables: [] }
          `shouldEqual`
            Right (Value [ URL (Quoted [ Lit "a b" ]) ])
    it "should convert double-underscore to space in an unquoted URL"
      $ runReader (runParserT "url(a__b)" value) { knownVariables: [] }
          `shouldEqual`
            Right (Value [ URL (Unquoted [ Lit "a b" ]) ])
    it "should convert double-underscore to space in a quoted value"
      $ runReader (runParserT "'a__b'" value) { knownVariables: [] }
          `shouldEqual`
            Right (Value [ Simple (Quoted [ Lit "a b" ]) ])
    it "should convert double-underscore to space in an unquoted value"
      $ runReader (runParserT "a__b" value) { knownVariables: [] }
          `shouldEqual`
            Right (Value [ Simple (Unquoted [ Lit "a b" ]) ])
    it "should allow a complex shorthand value involving a URL"
      $ runReader
          (runParserT "#000__url('https://hacss.io/$logo.gif')" value)
          { knownVariables: [ Variable "logo" ] }
          `shouldEqual`
            Right
              ( Value
                  [ Simple $ Unquoted [ Lit "#000 " ]
                  , URL $ Quoted [ Lit "https://hacss.io/", Var (Variable "logo"), Lit ".gif" ]
                  ]
              )
    it "should properly handle 'u' in unquoted value"
      $ runReader
          (runParserT "no-underline" value)
          { knownVariables: [] }
          `shouldEqual`
            Right (Value [ Simple $ Unquoted [ Lit "no-underline" ] ])
    it "should allow a complex shorthand value involving a calc expression"
      $ runReader
          (runParserT "1rem__calc(2rem-8px)" value)
          { knownVariables: [] }
          `shouldEqual`
            Right
              ( Value
                  [ Simple $ Unquoted [ Lit "1rem " ]
                  , Calc [ Lit "2rem - 8px" ]
                  ]
              )
    it "should properly handle 'c' in unquoted value"
      $ runReader
          (runParserT "#ccc" value)
          { knownVariables: [] }
          `shouldEqual`
            Right (Value [ Simple $ Unquoted [ Lit "#ccc" ] ])
  describe "declaration" do
    it "should parse a declaration"
      $ runReader
          (runParserT "font-size:16px" declaration)
          { knownProperties: [ Property "font-size" ], knownVariables: [] }
          `shouldEqual`
            Right
              ( Declaration
                  $ Tuple (Property "font-size")
                      (Value [ Simple $ Unquoted [ Lit "16px" ] ])
              )
  describe "context" do
    it "should parse a context"
      $ runParser ".field.touched:focus:hover+" context
          `shouldEqual`
            Right
              { classNames: ClassName <$> [ "field", "touched" ]
              , pseudoClasses: PseudoClass <$> [ "focus", "hover" ]
              , combinator: AdjacentSibling
              }
    it "should require at least one class name or pseudo-class"
      $ runParser "+" context `shouldSatisfy` isLeft
  describe "selector" do
    it "should parse a selector"
      $ runParser ".foo:hover>.bar:disabled::after" selector
          `shouldEqual`
            Right
              { context:
                  Just
                    { classNames: [ ClassName "foo" ]
                    , pseudoClasses: [ PseudoClass "hover" ]
                    , combinator: Parent
                    }
              , classNames: [ ClassName "bar" ]
              , pseudoClasses: [ PseudoClass "disabled" ]
              , pseudoElement: Just (PseudoElement "after")
              }
  describe "rule" do
    it "should allow a rule with declarations only"
      $ runReader
          (runParserT "background:red;color:white;" rule)
          { knownAtScopes: []
          , knownProperties: Property <$> [ "background", "color" ]
          , knownVariables: []
          }
          `shouldEqual`
            Right
              { atScope: Nothing
              , selector: Nothing
              , declarations:
                  Declaration
                    <$> [ Tuple (Property "background") (Value [ Simple (Unquoted [ Lit "red" ]) ])
                      , Tuple (Property "color") (Value [ Simple (Unquoted [ Lit "white" ]) ])
                      ]
              , importance: 0
              }
    it "should allow a rule with a selector"
      $ runReader
          (runParserT ".err{background:red}" rule)
          { knownAtScopes: []
          , knownProperties: [ Property "background" ]
          , knownVariables: []
          }
          `shouldEqual`
            Right
              { atScope: Nothing
              , selector:
                  Just
                    { context: Nothing
                    , classNames: [ ClassName "err" ]
                    , pseudoClasses: []
                    , pseudoElement: Nothing
                    }
              , declarations:
                  [ Declaration
                      $ Tuple
                          (Property "background")
                          (Value [ Simple (Unquoted [ Lit "red" ]) ])
                  ]
              , importance: 0
              }
    it "should allow a rule with an at-scope"
      $ runReader
          (runParserT "@sm{background:red}" rule)
          { knownAtScopes: [ AtScope "sm" ]
          , knownProperties: [ Property "background" ]
          , knownVariables: []
          }
          `shouldEqual`
            Right
              { atScope: Just $ AtScope "sm"
              , selector: Nothing
              , declarations:
                  [ Declaration
                      $ Tuple
                          (Property "background")
                          (Value [ Simple (Unquoted [ Lit "red" ]) ])
                  ]
              , importance: 0
              }
    it "should allow a rule with an at-scope and a selector"
      $ runReader
          (runParserT "@sm{.err{background:red}}" rule)
          { knownAtScopes: [ AtScope "sm" ]
          , knownProperties: [ Property "background" ]
          , knownVariables: []
          }
          `shouldEqual`
            Right
              { atScope: Just $ AtScope "sm"
              , selector:
                  Just
                    { context: Nothing
                    , classNames: [ ClassName "err" ]
                    , pseudoClasses: []
                    , pseudoElement: Nothing
                    }
              , declarations:
                  [ Declaration
                      $ Tuple
                          (Property "background")
                          (Value [ Simple (Unquoted [ Lit "red" ]) ])
                  ]
              , importance: 0
              }
    it "should allow a declarations-only rule with importance"
      $ runReader
          (runParserT "background:red;color:white;!!!" rule)
          { knownAtScopes: []
          , knownProperties: Property <$> [ "background", "color" ]
          , knownVariables: []
          }
          `shouldEqual`
            Right
              { atScope: Nothing
              , selector: Nothing
              , declarations:
                  Declaration
                    <$> [ Tuple (Property "background") (Value [ Simple (Unquoted [ Lit "red" ]) ])
                      , Tuple (Property "color") (Value [ Simple (Unquoted [ Lit "white" ]) ])
                      ]
              , importance: 3
              }
    it "should allow an advanced rule with importance"
      $ runReader
          (runParserT "@sm{background:red}!!" rule)
          { knownAtScopes: [ AtScope "sm" ]
          , knownProperties: [ Property "background" ]
          , knownVariables: []
          }
          `shouldEqual`
            Right
              { atScope: Just $ AtScope "sm"
              , selector: Nothing
              , declarations:
                  [ Declaration
                      $ Tuple
                          (Property "background")
                          (Value [ Simple (Unquoted [ Lit "red" ]) ])
                  ]
              , importance: 2
              }
  describe "rules" do
    it "should parse rules from a large block of code"
      $ runReader
          ( runParserT
              """
              <!DOCTYPE html>
              <html>
                <head>
                  <title>Foo</title>
                </head>
                <body class="@sm{padding:0}">
                  <button class="@sm{padding:0} background:#eee; :hover{color:red}">
                    Testing
                  </button>
                </body>
              </html>
            """
              rules
          )
          { knownAtScopes: [ AtScope "sm" ]
          , knownProperties: Property <$> [ "background", "color", "padding" ]
          , knownVariables: [ Variable "foo" ]
          }
          `shouldEqual`
            Right
              [ { atScope: Just $ AtScope "sm"
                , selector: Nothing
                , declarations:
                    [ Declaration
                        $ Tuple
                            (Property "padding")
                            (Value [ Simple $ Unquoted [ Lit "0" ] ])
                    ]
                , importance: 0
                }
              , { atScope: Nothing
                , selector: Nothing
                , declarations:
                    [ Declaration
                        $ Tuple
                            (Property "background")
                            (Value [ Simple $ Unquoted [ Lit "#eee" ] ])
                    ]
                , importance: 0
                }
              , { atScope: Nothing
                , selector:
                    Just
                      { context: Nothing
                      , classNames: []
                      , pseudoClasses: [ PseudoClass "hover" ]
                      , pseudoElement: Nothing
                      }
                , declarations:
                    [ Declaration
                        $ Tuple
                            (Property "color")
                            (Value [ Simple $ Unquoted [ Lit "red" ] ])
                    ]
                , importance: 0
                }
              ]
