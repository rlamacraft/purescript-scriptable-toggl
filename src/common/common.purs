module PurelyScriptable.Toggl.Common
  ( togglRequest
  ) where

import Control.Semigroupoid ((<<<))
import Data.Foldable (intercalate)
import Data.Function (($))
import Data.Functor (map)
import Data.Semigroup ((<>))
import Data.Tuple (Tuple(..))
import PurelyScriptable.Request (Header, Method(..), Request(..))

type Path = Array String
type Arg = Tuple String String
type Args = Array Arg

togglEndpoint :: Path
togglEndpoint = ["https://www.toggl.com", "api", "v8"]

togglRequest :: Header -> Path -> Args -> Request
togglRequest authHeader path args = Request [authHeader] GET $ pathToString path <> argsToString args where
  pathToString = intercalate "/" <<< ((<>) togglEndpoint)
  argsToString [] = ""
  argsToString as = "?" <> (intercalate "&" $ map argToString as)
  argToString (Tuple k v) = k <> "=" <> v

