module Test.Hacss.PseudoElement where

import Prelude
import Data.Either (Either(Right))
import Effect.Aff (Aff)
import Test.QuickCheck ((<?>))
import Test.Spec (SpecT, it)
import Test.Spec.QuickCheck (quickCheck)
import Text.Parsing.Parser (runParser)
import Hacss.Parser (pseudoElement) as P
import Hacss.Printer (pseudoElement) as R

tests :: forall m. Monad m => SpecT Aff Unit m Unit
tests =
  it "should parse and print pseudo-elements consistently"
    $ quickCheck \x ->
        let
          e = Right x

          a = runParser (R.pseudoElement x) P.pseudoElement
        in
          a == e <?> "Failed given " <> show x <> ": " <> show a <> " /= " <> show e
