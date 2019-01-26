module Test.Workspaces where

import Control.Bind ((>=>))
import Control.Monad.Free (Free)
import Control.Semigroupoid ((>>>))
import Data.Argonaut (Json, decodeJson, jsonParser)
import Data.Array (singleton)
import Data.Either (Either(..))
import Data.Function ((#))
import Data.Maybe (Maybe(..))
import Data.Unit (Unit)
import PurelyScriptable.Toggl.Workspaces (RoundingType(Up), Workspace(..), Workspaces(..))
import Test.Unit (suite, test, TestF)
import Test.Unit.Assert (equal)

testWorkspaces :: Free TestF Unit
testWorkspaces = do
  testDecodeWorkspace

testDecodeWorkspace :: Free TestF Unit
testDecodeWorkspace = suite "decodeWorkspace" do
  test "from JSON literal" do
    expected `equal` actual where
    expected = 
      { id : "3161255"
      , name : "Hello's workspace"
      , premium : false
      , admin : true
      , default_hourly_rate : 0.0
      , default_currency : "USD"
      , only_admins_may_create_projects : false
      , only_admins_see_billable_rates : false
      , rounding : Up
      , rounding_minutes : 0
      , at : "2019-01-08T20:02:27+00:00"
      , logo_url : Nothing
      } # Workspace >>> singleton >>> Workspaces >>> Right
    actual = "[{\"id\":3161255,\"profile\":0,\"ical_enabled\":true,\"projects_billable_by_default\":true,\"premium\":false,\"rounding_minutes\":0,\"only_admins_may_create_projects\":false,\"rounding\":1,\"only_admins_see_team_dashboard\":false,\"admin\":true,\"api_token\":\"XXX\",\"default_hourly_rate\":0,\"only_admins_see_billable_rates\":false,\"default_currency\":\"USD\",\"at\":\"2019-01-08T20:02:27+00:00\",\"name\":\"Hello's workspace\"}]" # (jsonParser >=> (decodeJson :: Json -> Either String Workspaces))
