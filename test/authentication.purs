module Test.Authentication (testAuthentication) where

import Control.Monad.Free (Free)
import Control.Semigroupoid ((>>>))
import Data.Function ((#))
import Data.Unit (Unit)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Test.Unit (suite, test, TestF)

testAuthentication :: Free TestF Unit
testAuthentication = do
  testPasswordAuth

testPasswordAuth :: Free TestF Unit
testPasswordAuth = suite "passwordAuth" do
  test "togglApiExample" do
    "Node.js doesn't have native btoa" # log >>> liftEffect
