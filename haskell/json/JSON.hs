module JSON (JValue(..),
             getString,
             getInt,
             getDouble,
             getObject,
             getArray,
             isNull,
             render,
             genRender,
             example,
             format) where

import Data.List (intercalate)

data JValue = JString String
            | JNumber Double
            | JBool Bool
            | JNull
            | JObject [(String, JValue)]
            | JArray [JValue]
              deriving (Eq, Ord, Show)

getString :: JValue -> Maybe String

getString (JString s) = Just s
getString _           = Nothing

getInt (JNumber n)    = Just (truncate n)
getInt _              = Nothing

getDouble (JNumber n) = Just n
getDouble _           = Nothing

getObject (JObject o) = Just o
getObject _           = Nothing

getArray (JArray a)   = Just a
getArray _            = Nothing

isNull v              = v == JNull

example = JObject [("baz",
                    JArray [
                       JObject [("foo", JNumber 1), ("bar", JBool False)],
                       JObject [("foo", JNumber 1), ("bar", JBool False)],
                       JBool True,
                       JBool False,
                       JNull,
                       JString "1",
                       JNumber 1,
                       JNumber 2.5
                       ])]
simple = JObject [("foo", JNumber 1), ("bar", JBool False)]

genRender :: JValue -> String
genRender (JString s) = show s
genRender (JNumber d) = show d
genRender (JBool True) = "true"
genRender (JBool False) = "false"
genRender JNull     = "null"
genRender (JArray []) = "[]"
genRender (JArray (x:xs)) = "[" ++ (genRender x) ++ (genRenderArrayHelper xs) ++ "]"
genRender (JObject []) = "{}"
genRender (JObject (x:xs)) = "{" ++ (show (fst x)) ++ ":" ++ (genRender (snd x)) ++ (genRenderObjectHelper xs) ++ "}"
--render _ = ""

genRenderArrayHelper :: [JValue] -> String
genRenderArrayHelper [] = ""
genRenderArrayHelper (x:xs) = "," ++ genRender x ++ genRenderArrayHelper xs

genRenderObjectHelper :: [(String, JValue)] -> String
genRenderObjectHelper [] = ""
genRenderObjectHelper (x:xs) = "," ++ (show (fst x)) ++ ":" ++ (genRender (snd x)) ++ (genRenderObjectHelper xs)


render :: JValue -> IO()
render x = putStrLn $ genRender x

format :: JValue -> IO()
format x = putStrLn $ genFormat x 0

genFormat :: JValue -> Int -> String
genFormat (JString s) n = replicate n ' ' ++ show s
genFormat (JNumber d) n = replicate n ' ' ++ show d
genFormat (JBool True) n = replicate n ' ' ++ "true"
genFormat (JBool False) n = replicate n ' ' ++ "false"
genFormat (JNull) n = replicate n ' ' ++ "null"

genFormat (JObject o) n = replicate n ' ' ++ "{\n" ++ pairs o ++ "\n" ++ replicate n ' ' ++ "}"
  where pairs ps = intercalate ",\n" (map genFormatPair ps)
        genFormatPair (k, v) = replicate (n + 1) ' ' ++ show k ++ ":\n" ++ genFormat v (n + 1)

genFormat (JArray a) n = replicate n ' ' ++ "[\n" ++ intercalate ",\n" (map (\x -> genFormat x (n + 1)) a) ++ "\n" ++ replicate n ' ' ++ "]"
