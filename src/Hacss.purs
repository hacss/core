module Hacss
  ( hacss
  , HacssError(..)
  , printHacssError
  , module PublicData
  , module PublicRenderer
  ) where

import Prelude
import Control.Alt ((<|>))
import Control.Monad.Except (runExcept)
import Data.Bifunctor (lmap)
import Data.Either (Either, either, hush)
import Data.Function.Uncurried (Fn2, mkFn2)
import Data.Maybe (Maybe)
import Data.String.Common (joinWith)
import Data.Traversable (traverse)
import Effect.Exception (throw)
import Effect.Unsafe (unsafePerformEffect)
import Foreign (Foreign, readString)
import Foreign.Index (readProp)
import Text.Parsing.StringParser (ParseError, runParser)
import Hacss.Internal.Data (AtScope(..), Property(..), Variable(..))
import Hacss.Internal.Parser (rules) as Parse
import Hacss.Internal.Renderer (CSS, RenderError, printRenderError, render)
import Hacss.Internal.Data (AtScope(..), Property(..), Variable(..)) as PublicData
import Hacss.Internal.Renderer (CSS, RenderError(..), printRenderError) as PublicRenderer

type Code
  = String

type Config
  = Foreign

data HacssError
  = ParseFailure ParseError
  | RenderFailure RenderError

printHacssError :: HacssError -> String
printHacssError = case _ of
  ParseFailure e -> show e
  RenderFailure e -> printRenderError e

hacss :: (Property -> Variable -> Maybe CSS) -> (AtScope -> Maybe CSS) -> Code -> Either HacssError CSS
hacss resolveVariable resolveAtScope code =
  joinWith ""
    <$> ( (lmap ParseFailure $ runParser Parse.rules code)
          >>= (lmap RenderFailure <<< traverse (render resolveVariable resolveAtScope))
      )

unsafeForeignHacss :: Fn2 Code Config CSS
unsafeForeignHacss =
  mkFn2 \code config ->
    let
      resolveVariable (Property p) (Variable v) =
        let
          variables = config # readProp "variables"
        in
          hush $ runExcept
            $ (variables >>= readProp p >>= readProp v >>= readString)
            <|> (variables >>= readProp v >>= readString)

      resolveAtScope (AtScope a) = hush $ runExcept $ readProp "atScopes" config >>= readProp a >>= readString
    in
      either
        (printHacssError >>> throw >>> unsafePerformEffect)
        identity
        $ hacss resolveVariable resolveAtScope code
