module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec (describe)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Test.Hacss.Internal.Parser (tests) as Parser
import Test.Hacss.Internal.Printer (tests) as Printer
import Test.Hacss.Internal.Renderer (tests) as Renderer

main :: Effect Unit
main =
  launchAff_
    $ runSpec [ consoleReporter ] do
        describe "parser" Parser.tests
        describe "printer" Printer.tests
        describe "renderer" Renderer.tests
