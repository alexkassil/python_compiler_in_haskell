-- filename globbing is regex-like pattern matching
-- Using Haskell's built in regex matching, this allows the converstion
-- of an input filename glob to a regex pattern

-- Globbing rules:
 -- matches strings from beginning to end
 -- literal characters match themselves
 -- * means match anything, including empty string
 -- ? matches any character, and . is just a literal
 -- [ starts a character class to be ended by ]
 -- [! starts a negation of a character class
 -- [! and [ cannot be empty, so []a] matches ] or a
 -- character followed by a - (dash) folled by another character is a range

module GlobRegex
    (
      globToRegex
    , matchesGlob
    ) where

import Text.Regex.Posix ((=~))

globToRegex :: String -> String
innerGlobToRegex :: String -> String
matchesGlob :: String -> String -> Bool

-- Wrap everything with ^ and $ to match whole line
globToRegex cs = "^" ++ innerGlobToRegex cs ++ "$"
--innerGlobToRegex ('.':cs) = "\\."  ++ innerGlobToRegex cs
--innerGlobToRegex ('^':cs) = "\\^"  ++ innerGlobToRegex cs
--innerGlobToRegex ('$':cs) = "\\$"  ++ innerGlobToRegex cs
--innerGlobToRegex ('\\':cs) = "\\\\"++ innerGlobToRegex cs
innerGlobToRegex ""             = ""
innerGlobToRegex ('?':cs)       = "."    ++ innerGlobToRegex cs
innerGlobToRegex ('*':cs)       = ".*"   ++ innerGlobToRegex cs
innerGlobToRegex ('[':'!':c:cs) = "[^" ++ [c] ++ charClass cs
innerGlobToRegex ('[':c:cs)     = "["  ++ [c] ++ charClass cs
innerGlobToRegex ('[':_)        = error "unterminated character class"
--innerGlobToRegex (c:cs)   = c       : innerGlobToRegex cs
-- Commented out is my old inefficient way, more elegant from textbook way below
innerGlobToRegex (c:cs) = escape c ++ innerGlobToRegex cs

escape :: Char -> String
escape c | c `elem` regexChars = '\\' : [c]
         | otherwise           = [c]
   where regexChars = "\\+()^$.{}]|"


-- charClass checks that the character class is correctly terminated by
-- passing its input unmodified til it hits a ], then hands control
-- back to innerGlobToRegex
charClass :: String -> String
charClass (']':cs) = ']' : innerGlobToRegex cs
charClass (c:cs)   = c : charClass cs
charClass []       = error "unterminated character class"


matchesGlob cs pat = (cs =~ (globToRegex pat) :: Bool)
