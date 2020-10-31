module Test.Hacss.Property where

import Prelude
import Control.Monad.Reader (runReader)
import Data.Either (Either(Right))
import Effect.Aff (Aff)
import Test.QuickCheck ((<?>))
import Test.Spec (SpecT, it)
import Test.Spec.QuickCheck (quickCheck)
import Text.Parsing.Parser (runParserT)
import Hacss.Parser (property) as P
import Hacss.Printer (property) as R

tests :: forall m. Monad m => SpecT Aff Unit m Unit
tests =
  it "should parse and print properties consistently"
    $ quickCheck \x ->
        let
          e = Right x

          a = runReader (runParserT (R.property x) P.property) { knownProperties: [ x ] }
        in
          a == e <?> "Failed given " <> show x <> ": " <> show a <> " /= " <> show e
