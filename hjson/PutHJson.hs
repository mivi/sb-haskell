module PutHJson where

import HJson
import Data.List (intercalate)

renderJValue :: JValue -> String

renderJValue (JString s) = show s
renderJValue (JNumber d) = show d
renderJValue (JBool True) = "true"
renderJValue (JBool False) = "false"
renderJValue JNull = "null"

renderJValue (JArray a) = "[" ++ pairs a ++ "]"
    where pairs [] = ""
          pairs aa = intercalate ", " (map renderJValue aa)

renderJValue (JObject o) = "{" ++ pairs o ++ "}"
    where pairs [] = ""
          pairs o = intercalate ", " (map renderPairs o)
          renderPairs (k,v) = show k ++ ": " ++ renderJValue v    
