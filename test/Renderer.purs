module Test.Hacss.Internal.Renderer (tests) where

import Prelude
import Data.Either (Either(..))
import Data.Lens ((.~))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Test.Spec (SpecT, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Hacss.Internal.Data (AtScope(..), Class(..), Combinator(..), Context(..), Declaration(..), Priority(..), Property(..), PseudoElement(..), ValCtx(..), ValExpr(..), Value(..), Variable(..), emptyRule, emptySelector, ruleAtScope, ruleDeclarations, rulePriority, ruleSelector, selectorClasses, selectorContext, selectorPseudoElement)
import Hacss.Internal.Renderer (RenderError(..), render)

tests :: forall m. Monad m => SpecT Aff Unit m Unit
tests =
  describe "renderer"
    $ let
        resolveAtScope (AtScope "sm") = Just "media only screen and (max-width:400px)"

        resolveAtScope _ = Nothing

        resolveVariable (Property "border") (Variable "thin") = Just "1px"

        resolveVariable (Property "font-weight") (Variable "thin") = Just "300"

        resolveVariable _ (Variable "red500") = Just "#900"

        resolveVariable _ _ = Nothing

        render' = render resolveVariable resolveAtScope
      in
        do
          it "renders a simple declarations-only rule"
            $ render'
                ( emptyRule
                    # ruleDeclarations
                    .~ ( Declaration
                          <$> [ Tuple
                                (Property "background")
                                (Value $ Right [ Tuple Nothing [ Lit "red" ] ])
                            , Tuple
                                (Property "color")
                                (Value $ Right [ Tuple Nothing [ Lit "white" ] ])
                            ]
                      )
                )
                `shouldEqual`
                  Right """.background\:red\;color\:white\;{background:red;color:white;}"""
          it "renders a rule with a variable-only declaration"
            $ render'
                ( emptyRule
                    # ruleDeclarations
                    .~ [ Declaration
                          $ Tuple
                              (Property "background")
                              (Value $ Left $ Variable "red500")
                      ]
                )
                `shouldEqual`
                  Right """.background\:\$red500\;{background:#900;}"""
          it "renders a rule with an interpolated variable"
            $ render'
                ( emptyRule
                    # ruleDeclarations
                    .~ [ Declaration
                          $ Tuple
                              (Property "background")
                              (Value $ Right [ Tuple Nothing [ Var $ Variable "red500", Lit "asdf" ] ])
                      ]
                )
                `shouldEqual`
                  Right """.background\:\#\{\$red500\}asdf\;{background:#900asdf;}"""
          it "resolves a non-interpolated variable using both property and variable name"
            $ render'
                ( emptyRule
                    # ruleDeclarations
                    .~ [ Declaration
                          $ Tuple
                              (Property "font-weight")
                              (Value $ Left $ Variable "thin")
                      ]
                )
                `shouldEqual`
                  Right """.font-weight\:\$thin\;{font-weight:300;}"""
          it "fails when a non-interpolated variable cannot be resolved"
            $ render'
                ( emptyRule
                    # ruleDeclarations
                    .~ [ Declaration
                          $ Tuple
                              (Property "background")
                              (Value $ Left $ Variable "foo-asdf")
                      ]
                )
                `shouldEqual`
                  Left (UnresolvedVariable (Property "background") (Variable "foo-asdf"))
          it "resolves an interpolated variable using both property and variable name"
            $ render'
                ( emptyRule
                    # ruleDeclarations
                    .~ [ Declaration
                          $ Tuple
                              (Property "border")
                              (Value $ Right [ Tuple Nothing [ Var (Variable "thin"), Lit " solid black" ] ])
                      ]
                )
                `shouldEqual`
                  Right """.border\:\#\{\$thin\}__solid__black\;{border:1px solid black;}"""
          it "fails when an interpolated variable cannot be resolved"
            $ render'
                ( emptyRule
                    # ruleDeclarations
                    .~ [ Declaration
                          $ Tuple
                              (Property "background")
                              (Value $ Right [ Tuple Nothing [ Lit "a", Var $ Variable "foo-asdf", Lit "b" ] ])
                      ]
                )
                `shouldEqual`
                  Left (UnresolvedVariable (Property "background") (Variable "foo-asdf"))
          it "renders a rule with a URL"
            $ render'
                ( emptyRule
                    # ruleDeclarations
                    .~ [ Declaration
                          $ Tuple
                              (Property "background-image")
                              (Value $ Right [ Tuple (Just URL) [ Lit "https://bomb.com/foo.gif" ] ])
                      ]
                )
                `shouldEqual`
                  Right """.background-image\:url\(\'https\:\/\/bomb\.com\/foo\.gif\'\)\;{background-image:url('https://bomb.com/foo.gif');}"""
          it "renders a rule with a variable within a URL"
            $ render'
                ( emptyRule
                    # ruleDeclarations
                    .~ [ Declaration
                          $ Tuple
                              (Property "background-image")
                              (Value $ Right [ Tuple (Just URL) [ Lit "https://abc.xyz/logo.svg?c=", Var $ Variable "red500" ] ])
                      ]
                )
                `shouldEqual`
                  Right """.background-image\:url\(\'https\:\/\/abc\.xyz\/logo\.svg\?c\=\#\{\$red500\}\'\)\;{background-image:url('https://abc.xyz/logo.svg?c=%23900');}"""
          it "renders a rule with a calc expression"
            $ render'
                ( emptyRule
                    # ruleDeclarations
                    .~ [ Declaration
                          $ Tuple
                              (Property "width")
                              (Value $ Right [ Tuple (Just Calc) [ Lit "5px + 20%" ] ])
                      ]
                )
                `shouldEqual`
                  Right """.width\:calc\(5px\+20\%\)\;{width:calc(5px + 20%);}"""
          it "renders a rule with a class selector"
            $ render'
                ( emptyRule
                    # ruleSelector
                    .~ Just (emptySelector # selectorClasses .~ [ NamedClass "err" ])
                    # ruleDeclarations
                    .~ [ Declaration
                          $ Tuple
                              (Property "color")
                              (Value $ Right [ Tuple Nothing [ Lit "red" ] ])
                      ]
                )
                `shouldEqual`
                  Right """.\.err\{color\:red\}.err{color:red;}"""
          it "renders a rule with a context selector (adjacent sibling)"
            $ render'
                ( emptyRule
                    # ruleSelector
                    .~ Just
                        (emptySelector # selectorContext .~ Just (Context $ Tuple [ PseudoClass "checked" ] AdjSib))
                    # ruleDeclarations
                    .~ [ Declaration
                          $ Tuple
                              (Property "text-decoration")
                              (Value $ Right [ Tuple Nothing [ Lit "line-through" ] ])
                      ]
                )
                `shouldEqual`
                  Right """:checked+.\:checked\+\{text-decoration\:line-through\}{text-decoration:line-through;}"""
          it "renders a rule with a context selector (ancestor)"
            $ render'
                ( emptyRule
                    # ruleSelector
                    .~ Just
                        (emptySelector # selectorContext .~ Just (Context $ Tuple [ PseudoClass "active" ] Ancestor))
                    # ruleDeclarations
                    .~ [ Declaration
                          $ Tuple
                              (Property "color")
                              (Value $ Right [ Tuple Nothing [ Lit "red" ] ])
                      ]
                )
                `shouldEqual`
                  Right """:active .\:active_\{color\:red\}{color:red;}"""
          it "renders a rule with a pseudo-element"
            $ render'
                ( emptyRule
                    # ruleSelector
                    .~ Just (emptySelector # selectorPseudoElement .~ Just (PseudoElement "after"))
                    # ruleDeclarations
                    .~ [ Declaration
                          $ Tuple
                              (Property "content")
                              (Value $ Right [ Tuple Nothing [ Lit "''" ] ])
                      ]
                )
                `shouldEqual`
                  Right """.\:\:after\{content\:\'\'\}::after{content:'';}"""
          it "renders a rule with an at-scope"
            $ render'
                ( emptyRule
                    # ruleAtScope
                    .~ Just (AtScope "sm")
                    # ruleDeclarations
                    .~ [ Declaration
                          $ Tuple
                              (Property "padding")
                              (Value $ Right [ Tuple Nothing [ Lit "0" ] ])
                      ]
                )
                `shouldEqual`
                  Right """@media only screen and (max-width:400px){.\@sm\{padding\:0\}{padding:0;}}"""
          it "rejects a rule with an at-scope that cannot be resolved"
            $ render'
                ( emptyRule
                    # ruleAtScope
                    .~ Just (AtScope "foo")
                    # ruleDeclarations
                    .~ [ Declaration
                          $ Tuple
                              (Property "padding")
                              (Value $ Right [ Tuple Nothing [ Lit "0" ] ])
                      ]
                )
                `shouldEqual`
                  Left (UnresolvedAtScope $ AtScope "foo")
          it "renders a rule with an at-scope and a selector"
            $ render'
                ( emptyRule
                    # ruleAtScope
                    .~ Just (AtScope "sm")
                    # ruleSelector
                    .~ Just (emptySelector # selectorPseudoElement .~ Just (PseudoElement "before"))
                    # ruleDeclarations
                    .~ [ Declaration
                          $ Tuple
                              (Property "padding")
                              (Value $ Right [ Tuple Nothing [ Lit "0" ] ])
                      ]
                )
                `shouldEqual`
                  Right """@media only screen and (max-width:400px){.\@sm\{\:\:before\{padding\:0\}\}::before{padding:0;}}"""
          it "renders a rule with priority"
            $ render'
                ( emptyRule
                    # ruleDeclarations
                    .~ [ Declaration
                          $ Tuple
                              (Property "background")
                              (Value $ Right [ Tuple Nothing [ Lit "red" ] ])
                      ]
                    # rulePriority
                    .~ Priority 2
                )
                `shouldEqual`
                  Right """.background\:red\;\!\!.background\:red\;\!\!.background\:red\;\!\!{background:red;}"""
