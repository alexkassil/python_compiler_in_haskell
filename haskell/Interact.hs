-- Run with:
-- runghc Interact.hs input.txt output.txt
import System.Environment (getArgs)

interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)

main = mainWith myFunction
  where mainWith function = do
          args <- getArgs
          case args of
            [input, output] -> interactWith function input output
            otherwise -> putStrLn "error: axactly two arguments needed"
        myFunction = \x -> unlines (map (\x -> show ((read x :: Integer) + 1)) (lines x))
