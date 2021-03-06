import Test.HUnit
import ParseJSON
import JSON
import System.IO

makeHandle :: String -> String -> IO Handle
makeHandle json filename = do
                            writeFile filename json
                            openFile filename ReadMode
                            

testJSONSimple = TestList [
  TestCase (do
               let json = JObject []
               h <- makeHandle (genRender json) "o1.json"
               s <- hGetContents h
               assertEqual "" json (parseJSON s)),
  TestCase (do
               let json = JArray []
               h <- makeHandle (genRender json) "a1.json"
               s <- hGetContents h
               assertEqual "" json (parseJSON s)),
  TestCase (do
               let json = JNull
               h <- makeHandle (genRender json) "n1.json"
               s <- hGetContents h
               assertEqual "" json (parseJSON s)),
  TestCase (do
               let json = JBool True
               h <- makeHandle (genRender json) "b1.json"
               s <- hGetContents h
               assertEqual "" json (parseJSON s)),
  TestCase (do
               let json = JBool False
               h <- makeHandle (genRender json) "b2.json"
               s <- hGetContents h
               assertEqual "" json (parseJSON s)),
  TestCase (do
               let json = JString "test"
               h <- makeHandle (genRender json) "s1.json"
               s <- hGetContents h
               assertEqual "" json (parseJSON s)),
  TestCase (do
               let json = JNumber 1
               h <- makeHandle (genRender json) "num1.json"
               s <- hGetContents h
               assertEqual "" json (parseJSON s)),
  TestCase (do
               let json = JObject [("test", JString "test")]
               h <- makeHandle (genRender json) "s2.json"
               s <- hGetContents h
               assertEqual "s2" json (parseJSON s)),
  TestCase (do
               let json = JObject [("bool", JBool True), ("bool", JBool False)]
               h <- makeHandle (genRender json) "s3.json"
               s <- hGetContents h
               assertEqual "s3" json (parseJSON s)),
  TestCase (do
               let json = JObject [("null", JNull), ("1", JNumber 1), ("2", JNumber 2.0)]
               h <- makeHandle (genRender json) "s4.json"
               s <- hGetContents h
               assertEqual "s4" json (parseJSON s)),
  TestCase (do
               let json = JObject [("array", JArray [JNumber 1.0])]
               h <- makeHandle (genRender json) "s5.json"
               s <- hGetContents h
               assertEqual "s5" json (parseJSON s)),
  TestCase (do
               let json = JObject [("obj", JObject [("obj", JObject [("null", JNull)])]),
                                   ("array", JArray [JObject [], JArray [], JString "null", JNull]),
                                   ("null", JNull)]
               h <- makeHandle (genRender json) "s6.json"
               s <- hGetContents h
               assertEqual "s6" json (parseJSON s))
  ]
             
testJSONComplex = TestCase (do
                        h1 <- openFile "1.json" ReadMode
                        h2 <- openFile "2.json" ReadMode
                        h3 <- openFile "3.json" ReadMode
                        s1 <- hGetContents h1
                        s2 <- hGetContents h2
                        s3 <- hGetContents h3
                        assertEqual "1.json equals example" example (parseJSON s1) 
                        assertEqual "2.json equals example" example (parseJSON s2)
                        assertEqual "3.json equals example" example (parseJSON s3)
                    )

-- TODO: Add autogenerated test that makes random JSON
-- and tries parsing it compact, spaced out, and pretty

main = do
       let tests = TestList [testJSONSimple, testJSONComplex]
       runTestTT tests
