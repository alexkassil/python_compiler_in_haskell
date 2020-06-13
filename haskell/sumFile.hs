-- usage:
-- runghc sumFile < inputfile
-- runghc sumFile
-- <enter numbers then press enter>
-- Send EOF with Ctrl-D
main = do
    contents <- getContents
    print (sumFile contents)
  where sumFile = sum . map read . words
