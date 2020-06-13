import Text.Regex.Posix
main = do
    -- Returns T/F based on if regex matches
    putStrLn $ show ("my left foot" =~ "foo" :: Bool)
    putStrLn $ show ("my left hand" =~ "foo" :: Bool)
    putStrLn $ show ("my right foot" =~ "(foo|right)" :: Bool)
    -- asking for an Int as the result of =~ counts the number of matches
    putStrLn $ show ("honorificabilitudinitatibus" =~ "[aeiou]" :: Int)
    -- asking for a string gets the first occurence
    putStrLn ("I, B. Ionsonii, uurit a lift'd batch" =~ "(uu|ii)" :: String)
    -- or [String] like this
    putStrLn $ show (getAllTextMatches ("I, B. Ionsonii, uurit a lift'd batch" =~ "(uu|ipei)") :: [String])
    -- Split on a match
    putStrLn $ show ("before foodiebar after" =~ pat :: (String,String,String))
    putStrLn $ show ("no match here" =~ pat :: (String,String,String))
    -- (Int, Int) gives us starting offset of match and length
    putStrLn $ show ("matches the fffffff train" =~ "f+" :: (Int, Int))
    -- returns (12,7) so I know starting at position 12 there are 7 f's
  where
    pat = "(foo[a-z]*bar|quux)"
