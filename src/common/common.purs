module PurelyScriptable.Toggl.Common
  ( fetch
  ) where

import Data.Monoid ((<>))
import PurelyScriptable.Request (Header, Method(..), Request(..))

fetch :: String -> Header -> Request
fetch resource authHeader = Request [authHeader] GET ("https://www.toggl.com/api/v8/" <> resource)
