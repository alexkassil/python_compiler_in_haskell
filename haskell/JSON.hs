module JSON (JValue(..),
             getString,
             getInt,
             getDouble,
             getObject,
             getArray,
             isNull) where

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

genRender :: JValue -> String
genRender (JString s) = show s
genRender (JNumber d) = show d
genRender (JBool b) = show b
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


