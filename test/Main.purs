module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec (describe)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Test.Hacss.ClassName (tests) as ClassName
import Test.Hacss.Combinator (tests) as Combinator
import Test.Hacss.AtScope (tests) as AtScope
import Test.Hacss.Parser (tests) as Parser
import Test.Hacss.Printer (tests) as Printer
import Test.Hacss.Property (tests) as Property
import Test.Hacss.PseudoClass (tests) as PseudoClass
import Test.Hacss.PseudoElement (tests) as PseudoElement

main :: Effect Unit
main =
  launchAff_
    $ runSpec [ consoleReporter ] do
        describe "Hacss" do
          ClassName.tests
          Combinator.tests
          PseudoClass.tests
          PseudoElement.tests
          AtScope.tests
          Property.tests
        describe "Parser" Parser.tests
        describe "Printer" Printer.tests
