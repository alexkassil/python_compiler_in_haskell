import Test.HUnit
import ParseJSON
import JSON
import System.IO

foo x = (1,3)
partA x = (1, 3)
partB x = True

test1 = TestCase (assertEqual "for (foo 3)," (1,2) (foo 3))
test2 = TestCase (do
                     let x = 5
                     let y = 6
                     let b = False
                     assertEqual "for the first result of partA," 5 x
                     assertBool ("(partB " ++ show y ++ ") failed") b)
tests = TestList [TestLabel "test1" test1, TestLabel "test2" test2]
--tests = TestList [TestLabel "test1" test1, TestLabel "test2" test1]

makeHandle :: String -> String -> IO Handle
makeHandle json filename = do
                            writeFile filename json
                            openFile filename ReadMode
                            


testJSONSimple = TestList [
  TestCase (do
               let json = JObject []
               h <- makeHandle (genRender json) "s1.json"
               assertEqual "s1" json (parseJSON h)),
  TestCase (do
               let json = JObject [("test", JString "test")]
               h <- makeHandle (genRender json) "s2.json"
               assertEqual "s2" json (parseJSON h)),
  TestCase (do
               let json = JObject [("bool", JBool True), ("bool", JBool False)]
               h <- makeHandle (genRender json) "s3.json"
               assertEqual "s3" json (parseJSON h)),
  TestCase (do
               let json = JObject [("null", JNull), ("1", JNumber 1), ("2", JNumber 2.0)]
               h <- makeHandle (genRender json) "s4.json"
               assertEqual "s4" json (parseJSON h)),
  TestCase (do
               let json = JObject [("array", JArray [JNumber 1.0])]
               h <- makeHandle (genRender json) "s5.json"
               assertEqual "s5" json (parseJSON h)),
  TestCase (do
               let json = JObject [("obj", JObject [("obj", JObject [("null", JNull)])]),
                                   ("array", JArray [JObject [], JArray [], JString "null", JNull]),
                                   ("null", JNull)]
               h <- makeHandle (genRender json) "s6.json"
               assertEqual "s6" json (parseJSON h))
  ]
             
testJSONComplex = TestCase (do
                        h1 <- openFile "1.json" ReadMode
                        h2 <- openFile "2.json" ReadMode
                        h3 <- openFile "3.json" ReadMode
                        assertEqual "1.json equals example" example (parseJSON h1) 
                        assertEqual "2.json equals example" example (parseJSON h2)
                        assertEqual "3.json equals example" example (parseJSON h3)
                    )

-- TODO: Add autogenerated test that makes random JSON
-- and tries parsing it compact, spaced out, and pretty

main = do
       let tests = TestList [testJSONSimple, testJSONComplex]
       runTestTT tests
