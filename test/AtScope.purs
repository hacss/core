module Test.Hacss.AtScope where

import Prelude
import Control.Monad.Reader (runReader)
import Data.Either (Either(Right))
import Effect.Aff (Aff)
import Test.QuickCheck ((<?>))
import Test.Spec (SpecT, it)
import Test.Spec.QuickCheck (quickCheck)
import Text.Parsing.Parser (runParserT)
import Hacss.Parser (atScope) as P
import Hacss.Printer (atScope) as R

tests :: forall m. Monad m => SpecT Aff Unit m Unit
tests =
  it "should parse and print at-scopes consistently"
    $ quickCheck \x ->
        let
          e = Right x

          a = runReader (runParserT (R.atScope x) P.atScope) { knownAtScopes: [ x ] }
        in
          a == e <?> "Failed given " <> show x <> ": " <> show a <> " /= " <> show e
