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

testJSON = TestCase (do
                        h1 <- openFile "1.json" ReadMode
                        h2 <- openFile "2.json" ReadMode
                        h3 <- openFile "3.json" ReadMode
                        assertEqual "1.json equals example" (parseJSON h1) example
                        assertEqual "2.json equals example" (parseJSON h2) example
                        assertEqual "3.json equals example" (parseJSON h2) example
                    )
             

main = do
       runTestTT testJSON
