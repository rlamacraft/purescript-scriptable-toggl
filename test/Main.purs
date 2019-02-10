module Test.Main where

import Prelude

import Effect (Effect)
import Test.Authentication (testAuthentication)
import Test.Unit.Main (runTest)
import Test.Workspaces (testWorkspaces)
import Tests.Projects (testProjects)

main :: Effect Unit
main = runTest do
  testAuthentication
  testWorkspaces
  testProjects
  
