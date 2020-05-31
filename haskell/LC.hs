-- file: LC.hs
-- Counts the number of lines in a file

main = interact wordCount
    where wordCount input = show (length (lines input)) ++ "\n"
