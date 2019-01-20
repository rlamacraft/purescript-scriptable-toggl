module PurelyScriptable.Toggl.Common
  ( request
  ) where

import Data.Monoid ((<>))
import PurelyScriptable.Request (Header, Method(..), Request(..))

request :: String -> Header -> Request
request resource authHeader = Request [authHeader] GET ("https://www.toggl.com/api/v8/" <> resource)
