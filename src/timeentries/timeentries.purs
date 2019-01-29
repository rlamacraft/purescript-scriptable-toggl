module PurelyScriptable.Toggl.TimeEntries where

import Control.Applicative (pure)
import Control.Bind (bind)
import Control.Semigroupoid ((>>>))
import Data.Argonaut (class DecodeJson, class EncodeJson, encodeJson, jsonEmptyObject, stringify, (.:), (.:?), (:=), (:=?), (~>), (~>?))
import Data.Argonaut.Decode.Class (decodeJObject)
import Data.Either (Either)
import Data.Function ((#))
import Data.Functor (map)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import PurelyScriptable.Request (Header, Method(..), loadDecodable)
import PurelyScriptable.Toggl.Common (togglRequest, unwrap)
import PurelyScriptable.Toggl.Projects (Project(..), ProjectId)
import PurelyScriptable.Toggl.Workspaces (WorkspaceId)

type TaskId = String
type Description = String

newtype TimeEntry = TimeEntry
  { description :: Description
  , wid :: Maybe WorkspaceId
  , pid :: Maybe ProjectId
  , tid :: Maybe TaskId
  , billable :: Maybe Boolean
  , start :: String
  , stop :: Maybe String
  , duration :: Int
  , created_with :: String
  , tags :: Array String
  , duronly :: Maybe Boolean
  , at :: String
  }

instance decodeJsonTimeEntry :: DecodeJson TimeEntry where
  decodeJson json = do
    obj          <- decodeJObject json
    description  <- obj .:  "description"
    wid          <- obj .:? "wid"
    pid          <- obj .:? "pid"
    tid          <- obj .:? "tid"
    billable     <- obj .:? "billable"
    start        <- obj .:  "start"
    stop         <- obj .:? "stop"
    duration     <- obj .:  "duration"
    created_with <- obj .:  "created_with"
    tags         <- obj .:  "tags"
    duronly      <- obj .:? "duronly"
    at           <- obj .:  "at"
    TimeEntry
      { description
      , wid
      , pid
      , tid
      , billable
      , start
      , stop
      , duration
      , created_with
      , tags
      , duronly
      , at
      } # pure

instance encodeJsonTimeEntry :: EncodeJson TimeEntry where
  encodeJson (TimeEntry te)
    =   "description"  :=  te.description
    ~>  "wid"          :=? te.wid
    ~>? "pid"          :=? te.pid
    ~>? "tid"          :=? te.tid
    ~>? "billable"     :=? te.billable
    ~>? "start"        :=  te.start
    ~>  "stop"         :=? te.stop
    ~>? "duration"     :=  te.duration
    ~>  "created_with" :=  te.created_with
    ~>  "tags"         :=  te.tags
    ~>  "duronly"      :=? te.duronly
    ~>? "at"           :=  te.at
    ~> jsonEmptyObject

newtype TimeEntryRequest = TimeEntryRequest TimeEntry

instance encodeJsonTimeEntryRequest :: EncodeJson TimeEntryRequest where
  encodeJson (TimeEntryRequest timeEntry)
    = "time_entry" := (encodeJson timeEntry)
    ~> jsonEmptyObject

startTimeEntry :: Header -> Project -> Description -> Aff (Either String TimeEntry)
startTimeEntry header (Project p) desc = togglRequest header (POST entry) ["time_entries", "start"] [] # loadDecodable >>> map (map unwrap) where
  entry = TimeEntry
          { description : desc
          , wid : Nothing
          , pid : Just p.id
          , tid : Nothing
          , billable : Nothing
          , start : ""
          , stop : Nothing
          , duration : 0
          , created_with : "PurelyScriptable"
          , tags : []
          , duronly : Nothing
          , at : ""
          } # TimeEntryRequest >>> encodeJson >>> stringify
