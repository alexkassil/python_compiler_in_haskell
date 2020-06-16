import System.IO
import Data.Char(toUpper)

data Token = NEWLINE
           | INDENT
           | DEDENT
           | ID String
           | LIT String
           | KEYWORD String
           | PUNCT String -- Could split into operators and delimiters
           | ENDMARKER

{--
Next steps tokenize input -> split on whitespace + newlines, then add something
that makes F(X): into 5 seperate tokens, then add difference between ID/Lit/Keyword/Punct,
then add indent/dedent. By this point I should have a python formatter! Test it, add tests
and makefile to easily run the tests
--}
          
main :: IO()
main = do
      inh <- openFile "simple_fun.py" ReadMode
      mainloop inh stdout
      hClose inh
      --hClose outh

mainloop :: Handle -> Handle -> IO ()
mainloop inh outh =
    do ineof <- hIsEOF inh
       if ineof
          then return ()
          else do inpStr <- hGetLine inh
                  hPutStrLn outh (map toUpper inpStr)
                  mainloop inh outh
