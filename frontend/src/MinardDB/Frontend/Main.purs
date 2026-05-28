module MinardDB.Frontend.Main where

import Prelude

import Effect (Effect)
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)
import MinardDB.Frontend.App as App

main :: Effect Unit
main = runHalogenAff do
  body <- awaitBody
  runUI App.component unit body
