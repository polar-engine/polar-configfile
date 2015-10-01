{-# LANGUAGE FlexibleContexts #-}

{-
Copyright (C) 2004-2008 John Goerzen <jgoerzen@complete.org>
Copyright (C) 2015 David Farrell <shokku.ra@gmail.com>
-}

{-|
   Module      : Data.ConfigFile.Parser
   Copyright   : Copyright (C) 2004-2008 John Goerzen, 2015 David Farrell
   License     : BSD3

   Maintainer  : David Farrell <shokku.ra@gmail.com>
   Stability   : unstable
   Portability : non-portable (GHC extensions)

Parser support for "Data.ConfigFile".  This module is not intended to be
used directly by your programs.

Copyright (C) 2004-2008 John Goerzen \<jgoerzen\@complete.org\>, 2015 David Farrell \<shokku.ra\@gmail.com\>.
-}

module Data.ConfigFile.Parser
(
 parseString, parseFile, parseHandle, interpMain, ParseOutput
       --satisfyG,
       --main
) where
import Text.ParserCombinators.Parsec
import Control.Monad.Error(throwError, MonadError)
import Data.String.Utils
import Data.ConfigFile.Lexer
import System.IO(Handle, hGetContents)
import Text.ParserCombinators.Parsec.Utils
import Data.ConfigFile.Types

----------------------------------------------------------------------
-- Exported funcs
----------------------------------------------------------------------

parseString :: MonadError CPError m =>
                String -> m ParseOutput
parseString s =
    detokenize "(string)" $ parse loken "(string)" s

--parseFile :: FilePath -> IO (CPResult ParseOutput)
parseFile :: MonadError CPError m => FilePath -> IO (m ParseOutput)
parseFile f =
    do o <- parseFromFile loken f
       return $ detokenize f o

--parseHandle :: Handle -> IO (CPResult ParseOutput)
parseHandle :: MonadError CPError m => Handle -> IO (m ParseOutput)
parseHandle h =
    do s <- hGetContents h
       let o = parse loken (show h) s
       return $ detokenize (show h) o

----------------------------------------------------------------------
-- Private funcs
----------------------------------------------------------------------
detokenize :: (Show t, MonadError (CPErrorData, [Char]) m) => SourceName
           -> Either t [GeneralizedToken CPTok]
           -> m ParseOutput
detokenize fp l =
    let conv msg (Left err) = throwError $ (ParseError (show err), msg)
        conv _ (Right val) = return val
        in do r <- conv "lexer" l
              conv "parser" $ runParser main () fp r

main :: GeneralizedTokenParser CPTok () ParseOutput
main =
    do {s <- sectionlist; return s}
    <|> try (do
             o <- optionlist
             s <- sectionlist
             return $ ("DEFAULT", o) : s
            )
    <|> do {o <- optionlist; return $ [("DEFAULT", o)] }
    <?> "Error parsing config file tokens"

sectionlist :: GeneralizedTokenParser CPTok () ParseOutput
sectionlist = do {eof; return []}
              <|> try (do
                       s <- sectionhead
                       eof
                       return [(s, [])]
                      )
              <|> do
                  s <- section
                  sl <- sectionlist
                  return (s : sl)

section :: GeneralizedTokenParser CPTok () (String, [(String, String)])
section = do {sh <- sectionhead; ol <- optionlist; return (sh, ol)}

sectionhead :: GeneralizedTokenParser CPTok () String
sectionhead =
    let wf (NEWSECTION x) = Just x
        wf _ = Nothing
        in
        do {s <- tokeng wf; return $ strip s}

optionlist :: GeneralizedTokenParser CPTok () [(String, String)]
optionlist = many coption

coption :: GeneralizedTokenParser CPTok () (String, String)
coption =
    let wf (NEWOPTION x) = Just x
        wf _ = Nothing
        wfx (EXTENSIONLINE x) = Just x
        wfx _ = Nothing
        in
        do o <- tokeng wf
           l <- many $ tokeng wfx
           return (strip (fst o), valmerge ((snd o) : l))

valmerge :: [String] -> String
valmerge vallist =
    let vl2 = map strip vallist
        in join "\n" vl2

----------------------------------------------------------------------
-- Interpolation
----------------------------------------------------------------------

interpval :: Parser String
interpval  = do
            string "%("
            s <- (many1 $ noneOf ")") <?> "interpolation name"
            string ")s"               <?> "end of interpolation name"
            return s

percentval :: Parser String
percentval = do
             string "%%"
             return "%"

interpother :: Parser String
interpother = do
              c <- noneOf "%"
              return [c]

interptok :: (String -> Either CPError String) -> Parser String
interptok lookupfunc = (try percentval)
                       <|> interpother
                       <|> do s <- interpval
                              case lookupfunc s of
                                 Left (InterpolationError x, _) -> fail x
                                 Left _ -> fail $ "unresolvable interpolation reference to \"" ++ s ++ "\""
                                 Right x -> return x


interpMain :: (String -> Either CPError String) -> Parser String
interpMain lookupfunc =
    do r <- manyTill (interptok lookupfunc) eof
       return $ concat r
