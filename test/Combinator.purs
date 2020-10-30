module Test.Hacss.Combinator where

import Prelude
import Data.Either (Either(Right))
import Effect.Aff (Aff)
import Test.QuickCheck ((<?>))
import Test.Spec (SpecT, it)
import Test.Spec.QuickCheck (quickCheck)
import Text.Parsing.Parser (runParser)

import Hacss.Parser (combinator) as P
import Hacss.Printer (combinator) as R

tests :: forall m. Monad m => SpecT Aff Unit m Unit
tests = it "should parse and print combinators consistently" $ quickCheck \x ->
  let
    e = Right x
    a = runParser (R.combinator x) P.combinator
  in
    a == e <?> "Failed given " <> show x <> ": " <> show a <> " /= " <> show e
