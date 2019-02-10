module PurelyScriptable.Toggl.Authentication
  ( tokenAuth
  , passwordAuth
  , asyncTokenAuth
  , asyncPasswordAuth
  ) where

import Control.Applicative (pure)
import Control.Bind (bind)
import Control.Semigroupoid ((>>>))
import Data.Either (Either, either)
import Data.Function (flip, (#), ($))
import Data.Semigroup ((<>))
import Data.String.Base64 (btoa)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (Error, throwException)
import PurelyScriptable.Request (Header)

tokenAuth :: String -> Either Error Header
tokenAuth = (flip authHelper) "api_token"

passwordAuth :: String -> String -> Either Error Header
passwordAuth = authHelper

authHelper :: String -> String -> Either Error Header
authHelper a b = do
  key <- (a <> ":" <> b) # btoa
  pure $ Tuple "Authorization" ("Basic " <> key)

collapseEither :: forall a . Either Error a -> Aff a
collapseEither = either (throwException >>> liftEffect) pure

asyncTokenAuth :: String -> Aff Header
asyncTokenAuth = tokenAuth >>> collapseEither

asyncPasswordAuth :: String -> String -> Aff Header
asyncPasswordAuth user = passwordAuth user >>> collapseEither
