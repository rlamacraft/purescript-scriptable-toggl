module PurelyScriptable.Toggl.Projects
  ( Projects(..)
  , Project(..)
  , ProjectId
  , ClientId
  , TemplateId
  , getWorkspaceProject
  ) where

import Color (Color, rgb)
import Control.Applicative (pure)
import Control.Bind (bind, (>>=))
import Control.Semigroupoid ((>>>))
import Data.Argonaut (class DecodeJson, decodeJson, (.:), (.:?))
import Data.Argonaut.Decode.Class (decodeJArray, decodeJObject)
import Data.Boolean (otherwise)
import Data.Either (Either(..))
import Data.Eq (class Eq, (==))
import Data.Function ((#), ($))
import Data.Functor (map)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, un)
import Data.Semigroup ((<>))
import Data.Show (class Show, show)
import Data.Traversable (sequence)
import Effect.Aff (Aff)
import PurelyScriptable.Request (Header, loadDecodable)
import PurelyScriptable.Toggl.Common (togglRequest)
import PurelyScriptable.Toggl.Workspaces (Workspace(..), WorkspaceId)
import PurelyScriptable.UITable (TextAlignment(..), Header(..)) as UIT
import PurelyScriptable.UITable (class Rowable, Cell(..), Row(..), backgroundColor, defaultRow, present_singleSelect)

type ProjectId = String
type ClientId = String
type TemplateId = String
type ColorId = String

newtype Project = Project
  { id :: ProjectId
  , name :: String
  , wid :: WorkspaceId
  , cid :: Maybe ClientId
  , active :: Boolean
  , is_private :: Boolean
  , template :: Maybe Boolean
  , template_id :: Maybe TemplateId
  , billable :: Maybe Boolean
  , auto_estimates :: Maybe Boolean
  , estimated_hours :: Maybe Int
  , at :: String
  , color :: Maybe Color
  , rate :: Maybe Number
  }
derive instance eqProject :: Eq Project
derive instance newtypeProject :: Newtype Project _

instance showProject :: Show Project where
  show (Project p) = "Project: " <> p.name

instance decodeProject :: DecodeJson Project where
  decodeJson json = do
    obj             <- decodeJObject json
    id_asInt        <- obj .:  "id" 
    name            <- obj .:  "name"
    wid_asInt       <- obj .:  "wid"
    cid_asMaybeInt  <- obj .:? "cid"
    active          <- obj .:  "active"
    is_private      <- obj .:  "is_private"
    template        <- obj .:? "template"
    template_id     <- obj .:? "template_id"
    billable        <- obj .:? "billable"
    auto_estimates  <- obj .:? "auto_estimates"
    estimated_hours <- obj .:? "estimated_hours"
    at              <- obj .:  "at"
    colorId         <- obj .:? "color"
    rate            <- obj .:? "rate"
    id              <- (id_asInt :: Int) # show >>> pure
    wid             <- (wid_asInt :: Int) # show >>> pure
    cid             <- (cid_asMaybeInt :: Maybe Int) # maybe (Right Nothing) (show >>> Just >>> Right)
    color           <- colorId # maybe (Right Nothing) (colorIdToColor >>> map Just)
    Project
      { id
      , name
      , wid
      , cid
      , active
      , is_private
      , template
      , template_id
      , billable
      , auto_estimates
      , estimated_hours
      , at
      , color
      , rate
      } # pure

colorIdToColor :: ColorId -> Either String Color
colorIdToColor id
  | id ==  "0" = Right $ rgb 6   170 245
  | id == " 1" = Right $ rgb 197 107 255
  | id ==  "2" = Right $ rgb 234 70  141
  | id ==  "3" = Right $ rgb 251 139 20
  | id ==  "4" = Right $ rgb 199 116 28
  | id ==  "5" = Right $ rgb 75  200 0
  | id ==  "6" = Right $ rgb 4   187 155
  | id ==  "7" = Right $ rgb 255 154 134
  | id ==  "8" = Right $ rgb 55  80  181
  | id ==  "9" = Right $ rgb 160 26  165
  | id == "10" = Right $ rgb 241 195 63
  | id == "11" = Right $ rgb 32  85  0
  | id == "12" = Right $ rgb 137 0   0
  | id == "13" = Right $ rgb 226 5   5
  | id == "14" = Right $ rgb 0   0   0
  | otherwise  = Left "Unknown color id"

instance rowableProject :: Rowable Project where
  rowable (Project p) = Row config cells where
    cells = [Text (Just p.name) (Just p.id) UIT.Left]
    config = { cellSpacing : Nothing
             , height : Nothing
             , backgroundColor : p.color >>= backgroundColor }
  header = UIT.Header $ defaultRow [Text (Just "Projects") (Just "Select one") UIT.Left]

newtype Projects = Projects (Array Project)
derive instance eqProjects :: Eq Projects
derive instance newtypeProjects :: Newtype Projects _

instance showProjects :: Show Projects where
  show (Projects ps) = show ps

instance decodeProjects :: DecodeJson Projects where
  decodeJson json = do
    array <- decodeJArray json
    ps <- array # map decodeJson >>> sequence
    ps # Projects >>> pure                 

getWorkspaceProject :: Header -> Workspace -> Aff (Either String Projects)
getWorkspaceProject header (Workspace w) = togglRequest header ["workspaces", w.id, "projects"] [] # loadDecodable

askProject :: Projects -> Aff Project
askProject = un Projects >>> present_singleSelect
