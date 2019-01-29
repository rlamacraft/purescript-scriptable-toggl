module PurelyScriptable.Toggl.Common
  ( DataObject(..)
  , togglRequest
  , unwrap
  ) where

import Control.Applicative (pure)
import Control.Bind (bind)
import Control.Semigroupoid ((<<<))
import Data.Argonaut (class DecodeJson, class EncodeJson, encodeJson, jsonEmptyObject, (.:), (:=), (~>))
import Data.Argonaut.Decode.Class (decodeJObject)
import Data.Foldable (intercalate)
import Data.Function ((#), ($))
import Data.Functor (map)
import Data.Semigroup ((<>))
import Data.Tuple (Tuple(..))
import PurelyScriptable.Request (Header, Method, Request(..))

type Path = Array String
type Arg = Tuple String String
type Args = Array Arg

togglEndpoint :: Path
togglEndpoint = ["https://www.toggl.com", "api", "v8"]

togglRequest :: Header -> Method -> Path -> Args -> Request
togglRequest authHeader method path args = Request [authHeader] method $ pathToString path <> argsToString args where
  pathToString = intercalate "/" <<< ((<>) togglEndpoint)
  argsToString [] = ""
  argsToString as = "?" <> (intercalate "&" $ map argToString as)
  argToString (Tuple k v) = k <> "=" <> v

newtype DataObject a = DO a

instance decodeJsonDataObject :: DecodeJson a => DecodeJson (DataObject a) where
  decodeJson json = do
    obj <- decodeJObject json
    a <- obj .: "data"
    DO a # pure

instance encodeJsonDataObject :: EncodeJson a => EncodeJson (DataObject a) where
  encodeJson (DO a)
    = "data" := (encodeJson a)
    ~> jsonEmptyObject

unwrap :: forall a . DataObject a -> a
unwrap (DO a) = a
