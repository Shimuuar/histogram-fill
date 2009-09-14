module Data.Histogram.Parse ( ws
                            , eol
                            , value
                            , keyword
                            ) where

import Text.Read
import Text.ParserCombinators.ReadP
import Text.ParserCombinators.ReadPrec

-- Whitespaces
ws :: ReadP String
ws = many $ satisfy (`elem` " \t")

-- End of line
eol :: ReadP Char
eol = char '\n'

-- Equal sign
eq :: ReadP ()
eq = ws >> char '=' >> return ()

-- Key
key :: String -> ReadP String
key s = char '#' >> ws >> string s 

-- Key value pair
value :: Read a => String -> ReadPrec a
value str = do 
  lift $ key str >> eq
  x <- readPrec
  lift $ eol
  return x

-- Keyword
keyword :: String -> ReadPrec ()
keyword str = lift $ key str >> ws >> eol >> return ()
