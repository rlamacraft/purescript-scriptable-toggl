module Test.Main where

import Prelude
import Effect (Effect)
import Test.Unit.Main (runTest)
import Test.Authentication (testAuthentication)

main :: Effect Unit
main = runTest do
  testAuthentication
