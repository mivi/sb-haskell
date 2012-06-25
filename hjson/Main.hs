module Main (main) where

import HJson
import PutHJson

main = let a = (JObject [("hello", JArray [JObject[("hi", JString "world")], JNumber 23]),("second", JBool True)])
           in print (renderJValue a)
