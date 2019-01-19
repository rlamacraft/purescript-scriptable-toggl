module PurelyScriptable.Toggl.Authentication
  ( tokenAuth
  , passwordAuth
  ) where

import Control.Applicative (pure)
import Control.Bind (bind)
import Control.Semigroupoid ((>>>))
import Data.Either (hush)
import Data.Function (flip, (#), ($))
import Data.Maybe (Maybe)
import Data.Semigroup ((<>))
import Data.String.Base64 (atob, encode)
import Data.Tuple (Tuple(..))
import PurelyScriptable.Request (Header)

tokenAuth :: String -> Maybe Header
tokenAuth = (flip authHelper) ":api_token"

passwordAuth :: String -> String -> Maybe Header
passwordAuth = authHelper

authHelper :: String -> String -> Maybe Header
authHelper a b = do
  key <- a <> b # encode >>> pure
  pure $ Tuple "Authorization" ("Basic: " <> key) 
