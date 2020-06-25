module ParseJSON
  (
    parseJSON
  ) where 
import JSON
import System.IO

parseJSON :: Handle -> JValue

parseJSON _ = JNull
