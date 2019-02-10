module PurelyScriptable.Toggl.Workspaces
  ( Workspaces(..)
  , Workspace(..)
  , WorkspaceId
  , RoundingType(..)
  , getWorkspaces
  ) where

import Control.Applicative (pure)
import Control.Bind (bind, (>=>))
import Control.Semigroupoid ((>>>))
import Data.Argonaut (class DecodeJson, decodeJson, toNumber, (.:), (.:?))
import Data.Argonaut.Decode.Class (decodeJArray, decodeJObject)
import Data.Boolean (otherwise)
import Data.Either (Either(..), note)
import Data.Eq (class Eq, (==))
import Data.Function ((#))
import Data.Functor (map)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Ring (negate)
import Data.Semigroup ((<>))
import Data.Show (class Show, show)
import Data.Traversable (sequence)
import Effect.Aff (Aff)
import PurelyScriptable.Request (Header, Method(..), loadDecodable)
import PurelyScriptable.Toggl.Common (togglRequest)

data RoundingType = Down | Nearest | Up
derive instance eqRoundingType :: Eq RoundingType

instance showRoundingType :: Show RoundingType where
  show Down = "Down"
  show Nearest = "Nearest"
  show Up = "Up"

caseRoundingType :: Number -> Either String RoundingType
caseRoundingType num
  | num == -1.0 = Right Down
  | num == 0.0  = Right Nearest
  | num == 1.0  = Right Up
  | otherwise   = Left  "Invalid number for Rounding Type"
                
instance decodeRoundingType :: DecodeJson RoundingType where
  decodeJson = toNumber >>> note "Not a number for Rounding Type" >=> caseRoundingType

type WorkspaceId = String

newtype Workspace = Workspace
  { id :: WorkspaceId
  , name :: String
  , premium :: Boolean
  , admin :: Boolean
  , default_hourly_rate :: Number
  , default_currency :: String
  , only_admins_may_create_projects :: Boolean
  , only_admins_see_billable_rates :: Boolean
  , rounding :: RoundingType
  , rounding_minutes :: Int
  , at :: String
  , logo_url :: Maybe String
  }
derive instance eqWorkspace :: Eq Workspace
derive instance newtypeWorkspace :: Newtype Workspace _

instance showWorkspace :: Show Workspace where
  show (Workspace w) = "Workspace: " <> w.name

instance decodeWorkspace :: DecodeJson Workspace where
  decodeJson json = do
    obj                             <- decodeJObject json
    id_asInt                        <- obj .:  "id"
    name                            <- obj .:  "name"
    premium                         <- obj .:  "premium"
    admin                           <- obj .:  "admin"
    default_hourly_rate             <- obj .:  "default_hourly_rate"
    default_currency                <- obj .:  "default_currency"
    only_admins_may_create_projects <- obj .:  "only_admins_may_create_projects"
    only_admins_see_billable_rates  <- obj .:  "only_admins_see_billable_rates"
    rounding                        <- obj .:  "rounding"
    rounding_minutes                <- obj .:  "rounding_minutes"
    at                              <- obj .:  "at"
    logo_url                        <- obj .:? "logo_url"
    id                              <- (id_asInt :: Int) # show >>> pure
    Workspace
              { id
              , name
              , premium
              , admin
              , default_hourly_rate
              , default_currency
              , only_admins_may_create_projects
              , only_admins_see_billable_rates
              , rounding
              , rounding_minutes
              , at
              , logo_url
              } # pure

newtype Workspaces = Workspaces (Array Workspace)
derive instance eqWorkspaces :: Eq Workspaces
derive instance newtypeWorkspaces :: Newtype Workspaces _

instance showWorkspaces :: Show Workspaces where
  show (Workspaces ws) = show ws

instance decodeWorkspaces :: DecodeJson Workspaces where
  decodeJson json = do
    array <- decodeJArray json
    ws <- array # map decodeJson >>> sequence
    ws # Workspaces >>> pure

getWorkspaces :: Header -> Aff Workspaces
getWorkspaces header = togglRequest header GET ["workspaces"] [] # loadDecodable
