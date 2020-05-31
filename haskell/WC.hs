-- file: WC.hs
-- Counts the number of words in a file

main = interact wordCount
    where wordCount input = show (length (words input)) ++ "\n"
