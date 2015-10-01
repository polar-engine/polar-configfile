{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-
Copyright (C) 2004-2008 John Goerzen <jgoerzen@complete.org>
Copyright (C) 2015 David Farrell <shokku.ra@gmail.com>
-}

{-|
   Module      : Data.ConfigFile.Types
   Copyright   : Copyright (C) 2004-2008 John Goerzen, 2015 David Farrell
   License     : BSD3

   Maintainer  : David Farrell <shokku.ra@gmail.com>
   Stability   : unstable
   Portability : non-portable (GHC extensions)

Internal types for "Data.ConfigFile".  This module is not intended to be
used directly by your programs.

Copyright (C) 2004-2008 John Goerzen \<jgoerzen\@complete.org\>, 2015 David Farrell \<shokku.ra\@gmail.com\>.
-}

module Data.ConfigFile.Types (
                                    CPOptions, CPData,
                                    CPErrorData(..), CPError, {-CPResult,-}
                                    ConfigParser(..),
                                    SectionSpec,
                                    OptionSpec,
                                    ParseOutput
                                   ) where
import qualified Data.Map as Map
import Data.Char
import Control.Monad.Error

{- | Internal output from parser -}
type ParseOutput = [(String, [(String, String)])]

{- | Names of sections -}
type SectionSpec = String

{- | Names of options -}
type OptionSpec = String

{- | Storage of options. -}
type CPOptions = Map.Map OptionSpec String

{- | The main data storage type (storage of sections).

PLEASE NOTE: This type is exported only for use by other modules under
Data.ConfigFile.  You should NEVER access the FiniteMap in a ConfigParser
directly.  This type may change in future releases of MissingH, which could
break your programs.  Please retrict yourself to the interface in
'Data.ConfigFile'.
 -}
type CPData = Map.Map SectionSpec CPOptions

{- | Possible ConfigParser errors. -}
data CPErrorData = ParseError String        -- ^ Parse error
                 | SectionAlreadyExists SectionSpec -- ^ Attempt to create an already-existing ection
                 | NoSection SectionSpec    -- ^ The section does not exist
                 | NoOption OptionSpec      -- ^ The option does not exist
                 | OtherProblem String      -- ^ Miscellaneous error
                 | InterpolationError String -- ^ Raised by 'Data.ConfigFile.interpolatingAccess' if a request was made for a non-existant option
                   deriving (Eq, Ord, Show)

{- | Indicates an error occurred.  The String is an explanation of the location
of the error. -}
type CPError = (CPErrorData, String)

instance Error CPError where
    noMsg = (OtherProblem "", "")
    strMsg x = (OtherProblem x, "")

{- Removed due to Hugs incompatibility.

| Basic ConfigParser error handling.  The Left value indicates
an error, while a Right value indicates success.
type CPResult a = MonadError CPError m => m a
-}

{- | This is the main record that is used by 'Data.ConfigFile'.
-}
data ConfigParser = ConfigParser
    { -- | The data itself
      content :: CPData,
      -- | How to transform an option into a standard representation
      optionxform :: (OptionSpec -> OptionSpec),
      -- | Function to look up an option, considering a default value
      -- if 'usedefault' is True; or ignoring a default value otherwise.
      -- The option specification is assumed to be already transformed.
      defaulthandler :: ConfigParser -> SectionSpec -> OptionSpec -> Either CPError String,
      -- | Whether or not to seek out a default action when no match
      -- is found.
      usedefault :: Bool,
      -- | Function that is used to perform lookups, do optional
      -- interpolation, etc.  It is assumed that accessfunc
      -- will internally call defaulthandler to do the underlying lookup.
      -- The option value is not assumed to be transformed.
      accessfunc :: (ConfigParser -> SectionSpec -> OptionSpec -> Either CPError String)
    }
