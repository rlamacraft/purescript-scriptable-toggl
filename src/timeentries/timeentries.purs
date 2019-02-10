module PurelyScriptable.Toggl.TimeEntries
  ( TimeEntry(..)
  , Description
  , TaskId
  , startTimeEntry
  ) where

import Control.Applicative (pure)
import Control.Bind (bind)
import Control.Plus (empty)
import Control.Semigroupoid ((>>>))
import Data.Argonaut (class DecodeJson, class EncodeJson, encodeJson, jsonEmptyObject, stringify, (.:), (.:?), (:=), (:=?), (~>), (~>?))
import Data.Argonaut.Decode.Class (decodeJObject)
import Data.Either (Either(..))
import Data.Function ((#))
import Data.Functor (map)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (un)
import Data.Show (show)
import Effect.Aff (Aff)
import PurelyScriptable.Request (Header, Method(..), loadDecodable)
import PurelyScriptable.Toggl.Common (DataObject(..), togglRequest)
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
  , created_with :: Maybe String
  , tags :: Array String
  , duronly :: Maybe Boolean
  , at :: String
  }

instance decodeJsonTimeEntry :: DecodeJson TimeEntry where
  decodeJson json = do
    obj            <- decodeJObject json
    description    <- obj .:  "description"
    wid_asMaybeInt <- obj .:? "wid"
    wid            <- (wid_asMaybeInt :: Maybe Int) # maybe (Right Nothing) (show >>> Just >>> Right)
    pid_asMaybeInt <- obj .:? "pid"
    pid            <- (pid_asMaybeInt :: Maybe Int) # maybe (Right Nothing) (show >>> Just >>> Right)
    tid_asMaybeInt <- obj .:? "tid"
    tid            <- (tid_asMaybeInt :: Maybe Int) # maybe (Right Nothing) (show >>> Just >>> Right)
    billable       <- obj .:? "billable"
    start          <- obj .:  "start"
    stop           <- obj .:? "stop"
    duration       <- obj .:  "duration"
    created_with   <- obj .:? "created_with"
    tags_asMaybeA  <- obj .:  "tags"
    tags           <- (tags_asMaybeA :: Maybe (Array String)) # maybe (Right empty) Right
    duronly        <- obj .:? "duronly"
    at             <- obj .:  "at"
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
    ~>  "created_with" :=? te.created_with
    ~>? "tags"         :=  te.tags
    ~>  "duronly"      :=? te.duronly
    ~>? "at"           :=  te.at
    ~> jsonEmptyObject

newtype TimeEntryRequest = TimeEntryRequest TimeEntry

instance encodeJsonTimeEntryRequest :: EncodeJson TimeEntryRequest where
  encodeJson (TimeEntryRequest timeEntry)
    = "time_entry" := (encodeJson timeEntry)
    ~> jsonEmptyObject

startTimeEntry :: Header -> Project -> Description -> Aff TimeEntry
startTimeEntry header (Project p) desc = togglRequest header (POST entry) ["time_entries", "start"] [] # loadDecodable >>> map (un DO) where
  entry = TimeEntry
          { description : desc
          , wid : Nothing
          , pid : Just p.id
          , tid : Nothing
          , billable : Nothing
          , start : ""
          , stop : Nothing
          , duration : 0
          , created_with : Just "PurelyScriptable"
          , tags : []
          , duronly : Nothing
          , at : ""
          } # TimeEntryRequest >>> encodeJson >>> stringify
