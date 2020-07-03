module ParseJSON
  (
    parseJSON
  ) where 
import JSON
import System.IO
import Text.ParserCombinators.Parsec


parseJSON :: String -> JValue

parseJSON "{}" = JObject []
parseJSON _ = JString "hi"
