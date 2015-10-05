{-# LANGUAGE Safe #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-
Copyright (C) 2004-2008 John Goerzen <jgoerzen@complete.org>
Copyright (C) 2015 David Farrell <shokku.ra@gmail.com>
-}

{-|
   Module      : Polar.ConfigFile.Types
   Copyright   : Copyright (C) 2004-2008 John Goerzen, 2015 David Farrell
   License     : BSD3

   Maintainer  : David Farrell <shokku.ra@gmail.com>
   Stability   : unstable
   Portability : non-portable (GHC extensions)

Internal types for "Polar.ConfigFile".  This module is not intended to be
used directly by your programs.

Copyright (C) 2004-2008 John Goerzen \<jgoerzen\@complete.org\>, 2015 David Farrell \<shokku.ra\@gmail.com\>.
-}

module Polar.ConfigFile.Types (
                                    Sections, Options,
                                    ConfigErrorType(..), ConfigError, {-CPResult,-}
                                    ConfigParser(..),
                                    SectionName,
                                    OptionName,
                                    Option, Section,
                                    ParseOutput
                                   ) where
import qualified Data.Map as Map
import Control.Monad.Error

{- | Internal output from parser -}
type ParseOutput = [(String, [(String, String)])]

{- | Names of sections -}
type SectionName = String

{- | Names of options -}
type OptionName = String

{- | Representation of sections -}
type Section = (SectionName, Options)

{- | Representation of options -}
type Option = (OptionName, String)

{- | Storage of options. -}
type Options = Map.Map OptionName String

{- | The main data storage type (storage of sections). -}
type Sections = Map.Map SectionName Options

{- | Possible ConfigParser errors. -}
data ConfigErrorType = ParseError String        -- ^ Parse error
                     | SectionAlreadyExists SectionName -- ^ Attempt to create an already-existing section
                     | NoSection SectionName    -- ^ The section does not exist
                     | NoOption OptionName      -- ^ The option does not exist
                     | OtherProblem String      -- ^ Miscellaneous error
                     | InterpolationError String -- ^ Raised by 'Polar.ConfigFile.interpolatingAccess' if a request was made for a non-existant option
                       deriving (Eq, Ord, Show)

{- | Indicates an error occurred.  The String is an explanation of the location
of the error. -}
type ConfigError = (ConfigErrorType, String)

instance Error ConfigError where
    noMsg = (OtherProblem "", "")
    strMsg x = (OtherProblem x, "")

{- Removed due to Hugs incompatibility.

| Basic ConfigParser error handling.  The Left value indicates
an error, while a Right value indicates success.
type CPResult a = MonadError ConfigError m => m a
-}

{- | This is the main record that is used by 'Polar.ConfigFile'.
-}
data ConfigParser = ConfigParser
    { -- | The data itself
      content :: Sections,
      -- | How to transform an option into a standard representation
      optionNameTransform :: (OptionName -> OptionName),
      -- | Function to look up an option, considering a default value
      -- if 'useDefault' is True; or ignoring a default value otherwise.
      -- The option specification is assumed to be already transformed.
      defaultHandler :: ConfigParser -> SectionName -> OptionName -> Either ConfigError String,
      -- | Whether or not to seek out a default action when no match
      -- is found.
      useDefault :: Bool,
      -- | Function that is used to perform lookups, do optional
      -- interpolation, etc.  It is assumed that accessFunction
      -- will internally call defaultHandler to do the underlying lookup.
      -- The option value is not assumed to be transformed.
      accessFunction :: (ConfigParser -> SectionName -> OptionName -> Either ConfigError String)
    }
