{-# LANGUAGE Safe #-}

{- arch-tag: ConfigParser utils
Copyright (C) 2004-2011 John Goerzen <jgoerzen@complete.org>
Copyright (C) 2016 David Farrell <shokku.ra@gmail.com>
-}

{- |
   Module      : Polar.ConfigFile.Utils
   Copyright   : Copyright (C) 2004-2011 John Goerzen, 2016 David Farrell
   License     : BSD3

   Maintainer  : David Farrell <shokku.ra@gmail.com>
   Stability   : provisional
   Portability : portable

Utils imported from "MissingH" package.
Lexer support for "Polar.ConfigFile".  This module is not intended to be
used directly by your programs.

Copyright (C) 2004-2011 John Goerzen \<jgoerzen\@complete.org\>, 2016 David Farrell \<shokku.ra\@gmail.com\>.
-}

module Polar.ConfigFile.Utils where

import Data.List (intersperse, isPrefixOf)
import Text.ParserCombinators.Parsec
import Control.Monad.Error

{- | Converts a Maybe value to an Either value, using the supplied parameter
 - as the Left value if the Maybe is Nothing.
 -
 - This function can be interpreted as:
 -
 - @maybeToEither :: e -> Maybe a -> Either e a@
 -
 - Its definition is given as it is so that it can be used in the Error and related monads.
 -
 -}
maybeToEither :: MonadError e m =>
                 e                      -- ^ (Left e) will be returned if the Maybe value is Nothing
              -> Maybe a                -- ^ (Right a) will be returned if this is (Just a)
              -> m a
maybeToEither errorval Nothing = throwError errorval
maybeToEither _ (Just normalval) = return normalval

{- | Takes an either and transforms it into something of the more generic
 - MonadError class. -}
eitherToMonadError :: MonadError e m => Either e a -> m a
eitherToMonadError (Left x) = throwError x
eitherToMonadError (Right x) = return x

{- | Given a list and a replacement list, replaces each occurance of the search
 - list with the replacement list in the operation list.
 -
 - Example:
 -
 - >replace "," "." "127,0,0,1" -> "127.0.0.1"
 -
 - This could logically be thought of as:
 -
 - >replace old new l = joinXs new . split old $ l
 -}

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace old new l = joinXs new . split old $ l

{- | Similar to Data.List.span, but performs the test on the entire remaining
 - list instead of just one element. 
 -
 - @spanList p xs@ is the same as @(takeWhileList p xs, dropWhileList p xs)@ 
 -}
spanList :: ([a] -> Bool) -> [a] -> ([a], [a])
spanList _ [] = ([],[])
spanList func list@(x:xs) =
    if func list
        then (x:ys,zs)
        else ([],list)
  where (ys,zs) = spanList func xs

{- | Similar to Data.List.break, but performs the test on the entire remaining
 - list instead of just one element.
 -}
breakList :: ([a] -> Bool) -> [a] -> ([a], [a])
breakList func = spanList (not . func)

{- | Given a delimiter and a list (or string), split into components.
 -
 - Example:
 -
 - > split "," "foo,bar,,baz," -> ["foo", "bar", "", "baz", ""]
 -
 - > split "ba" ",foo,bar,,baz," -> [",foo,","r,,","z,"]
 -}
split :: Eq a => [a] -> [a] -> [[a]]
split _ [] = []
split delim str =
    let (firstline, remainder) = breakList (isPrefixOf delim) str
        in 
        firstline : case remainder of
            [] -> []
            x -> if x == delim
                then [] : []
                else split delim 
                        (drop (length delim) x)

wschars :: String
wschars = " \t\r\n"

-- | Same as 'strip', but applies only to the left side of the string.
lstrip :: String -> String
lstrip s = case s of
    [] -> []
    (x:xs) -> if elem x wschars
                    then lstrip xs
                    else s

-- | Same as 'strip', but applies only to the right side of the string.
rstrip :: String -> String
rstrip = reverse . lstrip . reverse

{- | Given a delimiter and a list of items (or strings), join the items
 - by using the delimiter.
 -
 - Example:
 -
 - > joinXs "|" ["foo", "bar", "baz"] -> "foo|bar|baz"
 -}
joinXs :: [a] -> [[a]] -> [a]
joinXs delim l = concat (intersperse delim l)

{- | Removes any whitespace characters that are present at the start
 - or end of a string. Does not alter the internal contents of a
 - string. If no whitespace characters are present at the start or end
 - of a string, returns the original string unmodified. Safe to use on
 - any string.
 -
 - Note that this may differ from some other similar
 - functions from other authors in that:
 -
 - 1. If multiple whitespace
 - characters are present all in a row, they are all removed;
 -
 - 2. If no
 - whitespace characters are present, nothing is done.
 -}
strip :: String -> String
strip = lstrip . rstrip

type GeneralizedToken a = (SourcePos, a)
type GeneralizedTokenParser a st b = GenParser (GeneralizedToken a) st b

{- | Generate (return) a 'GeneralizedToken'. -}
togtok :: a -> GenParser b st (GeneralizedToken a)
togtok tok = do
    x <- getPosition
    return (x, tok)

{- | Retrieve the next token from a 'GeneralizedToken' stream.
 -   The given function should return the value to use, or Nothing
 -   to cause an error. -}
tokeng :: (Show a) => (a -> Maybe b) -> GeneralizedTokenParser a st b
tokeng test = token (show . snd) (fst) (test . snd)
