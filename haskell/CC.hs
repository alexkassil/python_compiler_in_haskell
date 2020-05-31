-- file: CC.hs
-- Counts the number of characters in a file

main = interact wordCount
    where wordCount input = show (length input) ++ "\n"
