module Tests.Projects where

import Control.Bind ((>=>))
import Control.Monad.Free (Free)
import Control.Semigroupoid ((>>>))
import Data.Argonaut (Json, decodeJson, jsonParser)
import Data.Either (Either(..))
import Data.Function ((#))
import Data.Maybe (Maybe(..))
import Data.Unit (Unit)
import PurelyScriptable.Toggl.Projects (Project(..))
import Test.Unit (TestF, suite, test)
import Test.Unit.Assert (equal)

testProjects :: Free TestF Unit
testProjects = do
  testDecodeProject

testDecodeProject :: Free TestF Unit
testDecodeProject = suite "decodeProject" do
  test "from JSON literal" do
    expected `equal` actual where
      expected =
        { id : "909"
        , name : "Very lucrative project"
        , wid : "777"
        , cid : Just "987"
        , active : true
        , is_private : true
        , template : Nothing
        , template_id : Nothing
        , billable : Just false
        , auto_estimates : Nothing
        , estimated_hours : Nothing
        , at : "2013-03-06T09:15:18+00:00"
        , color : Nothing
        , rate : Nothing
        } # Project >>> Right
      actual = "{\"id\":909,\"wid\":777,\"cid\":987,\"name\":\"Very lucrative project\",\"billable\":false,\"is_private\":true,\"active\":true,\"at\":\"2013-03-06T09:15:18+00:00\"}" # (jsonParser >=> (decodeJson :: Json -> Either String Project))
