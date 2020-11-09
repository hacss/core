module Test.Hacss.Renderer where

import Prelude
import Data.Map (lookup)
import Data.Map (singleton) as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Test.Spec (SpecT, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Hacss.Data (AtScope(..), ClassList(..), ClassName(..), Combinator(..), Context(..), Property(..), PseudoClass(..), PseudoElement(..), Selector(..), ValContext(..), ValExpr(..), Value(..), Variable(..))
import Hacss.Renderer (render)

tests :: forall m. Monad m => SpecT Aff Unit m Unit
tests =
  describe "renderer"
    $ let
        emptyRule = { atScope: Nothing, selector: Nothing, declarations: [], priority: 0 }

        emptySelector f = Selector $ f { context: Nothing, classes: Nothing, pseudoElement: Nothing }

        resolveAtScope = flip lookup $ Map.singleton (AtScope "sm") ("media only screen and (max-width:400px)")

        resolveVariable = flip lookup $ Map.singleton (Variable "red500") "#900"

        render' = render resolveAtScope resolveVariable
      in
        do
          it "renders a simple declarations-only rule"
            $ render'
                emptyRule
                  { declarations =
                    [ Tuple
                        (Property "background")
                        (Value [ Tuple Nothing [ Lit "red" ] ])
                    , Tuple
                        (Property "color")
                        (Value [ Tuple Nothing [ Lit "white" ] ])
                    ]
                  }
                `shouldEqual`
                  """.background\:red\;color\:white\;{background:red;color:white}"""
          it "renders a rule with a variable"
            $ render'
                emptyRule
                  { declarations =
                    [ Tuple
                        (Property "background")
                        (Value [ Tuple Nothing [ Var $ Variable "red500" ] ])
                    ]
                  }
                `shouldEqual`
                  """.background\:\$red500\;{background:#900}"""
          it "renders a rule with an unresolved variable"
            $ render'
                emptyRule
                  { declarations =
                    [ Tuple
                        (Property "background")
                        (Value [ Tuple Nothing [ Var $ Variable "foo-asdf" ] ])
                    ]
                  }
                `shouldEqual`
                  """.background\:\$foo-asdf\;{background:$foo-asdf}"""
          it "renders a rule with a URL"
            $ render'
                emptyRule
                  { declarations =
                    [ Tuple
                        (Property "background-image")
                        (Value [ Tuple (Just URL) [ Lit "https://bomb.com/foo.gif" ] ])
                    ]
                  }
                `shouldEqual`
                  """.background-image\:url\(\'https\:\/\/bomb\.com\/foo\.gif\'\)\;{background-image:url('https://bomb.com/foo.gif')}"""
          it "renders a rule with a variable within a URL"
            $ render'
                emptyRule
                  { declarations =
                    [ Tuple
                        (Property "background-image")
                        (Value [ Tuple (Just URL) [ Lit "https://abc.xyz/logo.svg?c=", Var $ Variable "red500" ] ])
                    ]
                  }
                `shouldEqual`
                  """.background-image\:url\(\'https\:\/\/abc\.xyz\/logo\.svg\?c\=\$red500\'\)\;{background-image:url('https://abc.xyz/logo.svg?c=%23900')}"""
          it "renders a rule with a calc expression"
            $ render'
                emptyRule
                  { declarations =
                    [ Tuple
                        (Property "width")
                        (Value [ Tuple (Just Calc) [ Lit "5px + 20%" ] ])
                    ]
                  }
                `shouldEqual`
                  """.width\:calc\(5px\+20\%\)\;{width:calc(5px + 20%)}"""
          it "renders a rule with a class selector"
            $ render'
                emptyRule
                  { selector =
                    Just $ emptySelector _ { classes = Just $ ClassList $ Tuple [ ClassName "err" ] [] }
                  , declarations =
                    [ Tuple
                        (Property "color")
                        (Value [ Tuple Nothing [ Lit "red" ] ])
                    ]
                  }
                `shouldEqual`
                  """.\.err\{color\:red\}.err{color:red}"""
          it "renders a rule with a context selector"
            $ render'
                emptyRule
                  { selector =
                    Just
                      $ emptySelector
                          _
                            { context = Just $ Context $ Tuple (ClassList $ Tuple [] [ BasicPseudoClass "checked" ]) AdjacentSibling
                            }
                  , declarations =
                    [ Tuple
                        (Property "text-decoration")
                        (Value [ Tuple Nothing [ Lit "line-through" ] ])
                    ]
                  }
                `shouldEqual`
                  """:checked+.\:checked\+\{text-decoration\:line-through\}{text-decoration:line-through}"""
          it "renders a rule with a pseudo-element"
            $ render'
                emptyRule
                  { selector =
                    Just $ emptySelector _ { pseudoElement = Just $ PseudoElement "after" }
                  , declarations =
                    [ Tuple
                        (Property "content")
                        (Value [ Tuple Nothing [ Lit "''" ] ])
                    ]
                  }
                `shouldEqual`
                  """.\:\:after\{content\:\'\'\}::after{content:''}"""
