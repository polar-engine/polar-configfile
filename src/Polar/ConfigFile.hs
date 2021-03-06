{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

{-
Copyright (C) 2004-2008 John Goerzen <jgoerzen@complete.org>
Copyright (C) 2015 David Farrell <shokku.ra@gmail.com>
-}

{-|
   Module      : Polar.ConfigFile
   Copyright   : Copyright (C) 2004-2008 John Goerzen, 2015 David Farrell
   License     : BSD3

   Maintainer  : David Farrell <shokku.ra@gmail.com>
   Stability   : unstable
   Portability : non-portable (GHC extensions)

Configuration file parsing, generation, and manipulation

Copyright (C) 2004-2008 John Goerzen \<jgoerzen\@complete.org\>, 2015 David Farrell \<shokku.ra\@gmail.com\>.

This module contains extensive documentation.  Please scroll down to the Introduction section to continue reading.
-}

module Polar.ConfigFile
    (
     -- * Introduction
     -- $introduction

     -- ** Features
     -- $features

     -- ** History
     -- $history

     -- * Configuration File Format
     -- $format

     -- ** White Space
     -- $whitespace

     -- ** Comments
     -- $comments

     -- ** Case Sensitivity
     -- $casesens

     -- ** Interpolation
     -- $interpolation

     -- * Usage Examples
     -- $usage

     -- ** Non-Monadic Usage
     -- $usagenomonad

     -- ** Error Monad Usage
     -- $usageerrormonad

     -- ** Combined Error\/IO Monad Usage
     -- $usageerroriomonad

     -- * Types
     -- $types
     SectionName, OptionName, ConfigParser(..),
     ConfigErrorType(..), ConfigError,
     -- * Initialization
     -- $initialization
     emptyCP,

     -- * Building
     -- $building
     (|%|), (|$|), (|=|),

     -- * Configuring the ConfigParser
     -- $configuringcp

     -- ** Access Functions
     simpleAccess, interpolatingAccess,

     -- * Reading
     -- $reading
     readFromFile, readFromHandle, readFromString,

     -- * Accessing Data
     OptionGetter(..),
     sections, hasSection,
     options, hasOption,
     items,

     -- * Modifying Data
     set, setShow, removeOption,
     addSection, removeSection,
     merge,

     -- * Output Data
     toString

    ) where

import Polar.ConfigFile.Types
import Polar.ConfigFile.Parser
import Polar.ConfigFile.Utils
import qualified Data.Map as Map
import System.IO (Handle)
import Data.Char
import Control.Monad.Error

-- For interpolatingAccess
import Text.ParserCombinators.Parsec.Error (errorMessages, Message(..))
import Text.ParserCombinators.Parsec (parse)

----------------------------------------------------------------------
-- Basic types / default values
----------------------------------------------------------------------

{- | The default empty 'Polar.ConfigFile' object.

The content contains only an empty mandatory @DEFAULT@ section.

'optionNameTransform' is set to @map toLower@.

'useDefault' is set to @True@.

'accessFunction' is set to 'simpleAccess'.
-}
emptyCP :: ConfigParser
emptyCP = ConfigParser { content = fromAL [("DEFAULT", [])],
                       defaultHandler = defHandler,
                       optionNameTransform = map toLower,
                       useDefault = True,
                       accessFunction = simpleAccess}

class BuildConfig a where (|%|) :: a -> Section -> ConfigParser
instance BuildConfig ConfigParser where cp |%| (sect, opts) = cp { content = Map.insert sect opts (content cp) }
instance BuildConfig Section where sect1 |%| sect2 = emptyCP |%| sect1 |%| sect2

class BuildSection a where (|$|) :: a -> Option -> Section
instance BuildSection Section where (sect, opts) |$| (oName, oValue) = (sect, Map.insert oName oValue opts)
instance BuildSection SectionName where     sect |$| (oName, oValue) = (sect, Map.singleton oName oValue)

(|=|) :: OptionName -> String -> Option
opt |=| val = (opt, val)

-- the list append function (++) has a precendence of 5
-- and should bind more tightly than these operators
infixl 2 |%|
infixl 3 |$|
infix  4 |=|

{- | Low-level tool to convert a parsed object into a 'Sections'
representation.  Performs no option conversions or special handling
of @DEFAULT@. -}
fromAL :: ParseOutput -> Sections
fromAL origal =
    let conv :: Sections -> (String, [(String, String)]) -> Sections
        conv fm sect = Map.insert (fst sect) (Map.fromList $ snd sect) fm
        in
        foldl conv Map.empty origal

{- | Default (non-interpolating) access function -}
simpleAccess ::  MonadError ConfigError m =>
                 ConfigParser -> SectionName -> OptionName -> m String
simpleAccess cp s o = defHandler cp s (optionNameTransform cp $ o)

{- | Interpolating access function.  Please see the Interpolation section
above for a background on interpolation.

Although the format string looks similar to one used by "Text.Printf",
it is not the same.  In particular, only the %(...)s format is supported.
No width specifiers are supported and no conversions other than s are supported.

To use this function, you must specify a maximum recursion depth for
interpolation.  This is used to prevent a stack overflow in the event that
the configuration file contains an endless interpolation loop.  Values of 10
or so are usually more than enough, though you could probably go into the
hundreds or thousands before you have actual problems.

A value less than one will cause an instant error every time you attempt
a lookup.

This access method can cause 'get' and friends to return a new 'ConfigError':
'InterpolationError'.  This error would be returned when:

 * The configuration file makes a reference to an option that does
   not exist

 * The maximum interpolation depth is exceeded

 * There is a syntax error processing a %-directive in the configuration
   file

An interpolation lookup name specifies an option only.  There is no provision
to specify a section.  Interpolation variables are looked up in the current
section, and, if 'useDefault' is True, in @DEFAULT@ according to the normal
logic.

To use a literal percent sign, you must place @%%@ in the configuration
file when interpolation is used.

Here is how you might enable interpolation:

>let cp2 = cp {accessFunction = interpolatingAccess 10}

The @cp2@ object will now support interpolation with a maximum depth of 10.
 -}
interpolatingAccess :: MonadError ConfigError m =>
                       Int ->
                       ConfigParser -> SectionName -> OptionName
                       -> m String

interpolatingAccess maxdepth cp s o =
    if maxdepth < 1
       then interError "maximum interpolation depth exceeded"
       else do x <- simpleAccess cp s o
               case parse (interpMain $ lookupfunc) (s ++ "/" ++ o) x of
                 Left y -> case head (errorMessages y) of
                                Message z -> interError z
                                _ -> interError (show y)
                 Right y -> return y
    where
    lookupfunc = interpolatingAccess (maxdepth - 1) cp s
    interError x = throwError (InterpolationError x, "interpolatingAccess")

-- internal function: default handler
defHandler ::  MonadError ConfigError m =>
                      ConfigParser -> SectionName -> OptionName -> m String

defHandler cp sectn opt =
    let fm = content cp
        lookUp s o = do sect <- maybeToEither (NoSection s,
                                               "get " ++ formatSO sectn opt) $
                                Map.lookup s fm
                        maybeToEither (NoOption o,
                                       "get " ++ formatSO sectn opt) $
                                Map.lookup o sect
        trydefault e = if (useDefault cp)
                       then
                            lookUp "DEFAULT" opt
                                       -- Use original error if it's not in DEFAULT either
                                       `catchError` (\_ -> throwError e)
                       else throwError e
        in
        lookUp sectn opt `catchError` trydefault


{- | Combines two 'ConfigParser's into one.

Any duplicate options are resolved to contain the value specified in
the second parser.

The 'ConfigParser' options in the resulting object will be set as they
are in the second one passed to this function. -}
merge :: ConfigParser -> ConfigParser -> ConfigParser
merge src dest =
    let conv :: String -> String
        conv = optionNameTransform dest
        convFM :: Options -> Options
        convFM = Map.fromList . map (\x -> (conv (fst x), snd x)) . Map.toList
        mergesects a b = Map.union a b
        in
        dest { content = Map.unionWith mergesects
                         (content dest) (Map.map convFM (content src)) }

{- | Utility to do a special case merge. -}
readutil :: ConfigParser -> ParseOutput -> ConfigParser
readutil old new = merge old $ old { content = fromAL new }

{- | Loads data from the specified file.  It is then combined with the
given 'ConfigParser' using the semantics documented under 'merge' with the
new data taking precedence over the old.  However, unlike
'merge', all the options
as set in the old object are preserved since the on-disk representation
does not convey those options.

May return an error if there is a syntax error.  May raise an exception if the file could not be accessed.
-}
--readFromFile :: ConfigParser -> FilePath ->IO (CPResult ConfigParser)
readFromFile :: MonadError ConfigError m => ConfigParser -> FilePath -> IO (m ConfigParser)
{-
readFromFile cp fp = do n <- parseFile fp
                    return $ do y <- n
                                return $ readutil cp y
-}
readFromFile cp fp = do n <- parseFile fp
                        return $ n >>= (return . readutil cp)

{- | Like 'readFromFile', but uses an already-open handle.  You should
use 'readFromFile' instead of this if possible, since it will be able to
generate better error messages.

Errors would be returned on a syntax error.
-}
--readFromHandle :: ConfigParser -> Handle -> IO (CPResult ConfigParser)
readFromHandle :: MonadError ConfigError m => ConfigParser -> Handle -> IO (m ConfigParser)
readFromHandle cp h = do n <- parseHandle h
                         return $ n >>= (return . (readutil cp))

{- | Like 'readFromFile', but uses a string.  You should use 'readFromFile'
instead of this if you are processing a file, since it can generate
better error messages.

Errors would be returned on a syntax error.
-}
readFromString ::  MonadError ConfigError m =>
               ConfigParser -> String -> m ConfigParser
readFromString cp s = do
                  n <- parseString s
                  return $ readutil cp n

{- | Returns a list of sections in your configuration file.  Never includes
the always-present section @DEFAULT@. -}
sections :: ConfigParser -> [SectionName]
sections = filter (/= "DEFAULT") . Map.keys . content

{- | Indicates whether the given section exists.

No special @DEFAULT@ processing is done. -}
hasSection :: ConfigParser -> SectionName -> Bool
hasSection cp x = Map.member x (content cp)

{- | Adds the specified section name.  Returns a
'SectionAlreadyExists' error if the
section was already present.  Otherwise, returns the new
'ConfigParser' object.-}
addSection ::  MonadError ConfigError m =>
                ConfigParser -> SectionName -> m ConfigParser
addSection cp s =
    if hasSection cp s
       then throwError $ (SectionAlreadyExists s, "addSection")
       else return $ cp {content = Map.insert s Map.empty (content cp)}

{- | Removes the specified section.  Returns a 'NoSection' error if
the section does not exist; otherwise, returns the new 'ConfigParser'
object.

This call may not be used to remove the @DEFAULT@ section.  Attempting to do
so will always cause a 'NoSection' error.
 -}
removeSection ::  MonadError ConfigError m =>
                   ConfigParser -> SectionName -> m ConfigParser
removeSection _ "DEFAULT" = throwError $ (NoSection "DEFAULT", "removeSection")
removeSection cp s =
    if hasSection cp s
       then return $ cp {content = Map.delete s (content cp)}
       else throwError $ (NoSection s, "removeSection")

{- | Removes the specified option.  Returns a 'NoSection' error if the
section does not exist and a 'NoOption' error if the option does not
exist.  Otherwise, returns the new 'ConfigParser' object.
-}
removeOption ::  MonadError ConfigError m =>
                  ConfigParser -> SectionName -> OptionName -> m ConfigParser
removeOption cp s passedo =
    do sectmap <- maybeToEither (NoSection s,
                                 "removeOption " ++ formatSO s passedo) $
                  Map.lookup s (content cp)
       let o = (optionNameTransform cp) passedo
       let newsect = Map.delete o sectmap
       let newmap = Map.insert s newsect (content cp)
       if Map.member o sectmap
          then return $ cp {content = newmap}
          else throwError $ (NoOption o,
                             "removeOption " ++ formatSO s passedo)

{- | Returns a list of the names of all the options present in the
given section.

Returns an error if the given section does not exist.
-}
options ::  MonadError ConfigError m =>
            ConfigParser -> SectionName -> m [OptionName]
options cp x = maybeToEither (NoSection x, "options") $
               do
               o <- Map.lookup x (content cp)
               return $ Map.keys o

{- | Indicates whether the given option is present.  Returns True
only if the given section is present AND the given option is present
in that section.  No special @DEFAULT@ processing is done.  No
exception could be raised or error returned.
-}
hasOption :: ConfigParser -> SectionName -> OptionName -> Bool
hasOption cp s o =
    let c = content cp
        v = do secthash <- Map.lookup s c
               return $ Map.member (optionNameTransform cp $ o) secthash
        in maybe False id v

{- | The class representing the data types that can be returned by "get".
-}
class OptionGetter a where
    {- | Retrieves a string from the configuration file.

When used in a context where a String is expected, returns that string verbatim.

When used in a context where a Bool is expected, parses the string to
a Boolean value (see logic below).

When used in a context where anything that is an instance of Read is expected,
calls read to parse the item.

An error will be returned of no such option could be found or if it could
not be parsed as a boolean (when returning a Bool).

When parsing to a Bool, strings are case-insentively converted as follows:

The following will produce a True value:

 * 1

 * yes

 * on

 * enabled

 * true

The following will produce a False value:

 * 0

 * no

 * off

 * disabled

 * false -}
    get :: MonadError ConfigError m => ConfigParser -> SectionName -> OptionName -> m a

instance {-# OVERLAPPABLE #-} Read t => OptionGetter t where
    get = genericget

instance OptionGetter String where
    get cp s o = eitherToMonadError $ (accessFunction cp) cp s o

instance OptionGetter Bool where
    get = getbool

-- Based on code from Neil Mitchell's safe-0.3.3 package.
readMaybe :: Read a => String -> Maybe a
readMaybe s = case [x | (x, t) <- reads s, ("","") <- lex t] of
                [x] -> Just x
                _   -> Nothing

genericget :: (Read b, MonadError ConfigError m) => ConfigParser -> SectionName -> OptionName -> m b
genericget cp s o = do
    val <- get cp s o
    let errMsg = "couldn't parse value " ++ val ++ " from " ++ formatSO s o
    maybe (throwError (ParseError errMsg, "genericget"))
          return
          $ readMaybe val

getbool ::  MonadError ConfigError m =>
            ConfigParser -> SectionName -> OptionName -> m Bool
getbool cp s o =
    do val <- get cp s o
       case map toLower . strip $ val of
                  "1" -> return True
                  "yes" -> return True
                  "on" -> return True
                  "enabled" -> return True
                  "true" -> return True
                  "0" -> return False
                  "no" -> return False
                  "off" -> return False
                  "disabled" -> return False
                  "false" -> return False
                  _ -> throwError (ParseError $ "couldn't parse bool " ++
                                   val ++ " from " ++ formatSO s o, "getbool")

formatSO :: [Char] -> [Char] -> [Char]
formatSO s o =
    "(" ++ s ++ "/" ++ o ++ ")"


{- | Returns a list of @(optionname, value)@ pairs representing the content
of the given section.  Returns an error the section is invalid. -}
items ::  MonadError ConfigError m =>
          ConfigParser -> SectionName -> m [(OptionName, String)]
items cp s = do fm <- maybeToEither (NoSection s, "items") $
                      Map.lookup s (content cp)
                return $ Map.toList fm

{- | Sets the option to a new value, replacing an existing one if it exists.

Returns an error if the section does not exist. -}
set ::  MonadError ConfigError m =>
        ConfigParser -> SectionName -> OptionName -> String -> m ConfigParser
set cp s passedo val =
    do sectmap <- maybeToEither (NoSection s, "set " ++ formatSO s passedo) $
                  Map.lookup s (content cp)
       let o = (optionNameTransform cp) passedo
       let newsect = Map.insert o val sectmap
       let newmap = Map.insert s newsect (content cp)
       return $ cp { content = newmap}

{- | Sets the option to a new value, replacing an existing one if it exists.
It requires only a showable value as its parameter.
This can be used with bool values, as well as numeric ones.

Returns an error if the section does not exist. -}
setShow :: (Show a, MonadError ConfigError m) =>
           ConfigParser -> SectionName -> OptionName -> a -> m ConfigParser
setShow cp s o val = set cp s o (show val)

{- | Converts the 'ConfigParser' to a string representation that could be
later re-parsed by this module or modified by a human.

Note that this does not necessarily re-create a file that was originally
loaded.  Things may occur in a different order, comments will be removed,
etc.  The conversion makes an effort to make the result human-editable,
but it does not make an effort to make the result identical to the original
input.

The result is, however, guaranteed to parse the same as the original input.
 -}
toString :: ConfigParser -> String
toString cp =
    let gen_option (key, value) =
            key ++ ": " ++ (replace "\n" "\n    " value) ++ "\n"
        gen_section (sect, valfm) = -- gen a section, but omit DEFAULT if empty
            if (sect /= "DEFAULT") || (Map.size valfm > 0)
               then "[" ++ sect ++ "]\n" ++
                        (concat $ map gen_option (Map.toList valfm)) ++ "\n"
               else ""
        in
        concat $ map gen_section (Map.toList (content cp))

----------------------------------------------------------------------
-- Docs
----------------------------------------------------------------------

{- $introduction

Many programs need configuration files. These configuration files are
typically used to configure certain runtime behaviours that need to be
saved across sessions. Various different configuration file formats
exist.

The ConfigParser module attempts to define a standard format that is
easy for the user to edit, easy for the programmer to work with, yet
powerful and flexible.
-}

{- $features

For the programmer, this module provides:

 * Simple calls to both read /and write/ configuration files

 * Call that can generate a string version of a file that is
   re-parsable by this module (useful for, for instance, sending the
   file down a network)

 * Segmented configuration files that let you separate configuration
   into distinct sections, each with its own namespace. This can be
   used to configure multiple modules in one file, to configure
   multiple instances of a single object, etc.

 * On-the-fly parsing of integer, boolean, float, multi-line string values,
   and anything else Haskell's read can deal with

 * It is possible to make a configuration file parsable by this
   module, the Unix shell, and\/or Unix make, though some features are,
   of course, not compatible with these other tools.

 * Syntax checking with error reporting including line numbers

 * Implemented in pure Haskell.  No dependencies on modules outside
   the standard library distributed with Haskell compilers or interpreters.
   All calls except those that read directly from a handle are pure calls
   and can be used outside the IO monad.

 * Comprehensive documentation

 * Extensible API

For the user, this module provides:

 * Easily human-editable configuration files with a clear, concise,
   and consistent format

 * Configuration file format consistent with other familiar formats
   (\/etc\/passwd is a valid ConfigParser file)

 * No need to understand semantics of markup languages like XML
-}

{- $history

This module is based on Python's ConfigParser module at
<http://www.python.org/doc/current/lib/module-ConfigParser.html>.  I had
earlier developed an OCaml implementation as part of my MissingLib library
at <gopher://gopher.quux.org/devel/missinglib>.

While the API of these three modules is similar, and the aim is to preserve all
useful features of the original Python module, there are some differences
in the implementation details.  This module is a complete, clean re-implementation
in Haskell, not a Haskell translation of a Python program.  As such, the feature
set is slightly different.

/\-John Goerzen/
-}

{- $format

The basic configuration file format resembles that of an old-style
Windows .INI file. Here are two samples:

>debug = yes
>inputfile = /etc/passwd
>names = Peter, Paul, Mary, George, Abrahaham, John, Bill, Gerald, Richard,
>        Franklin, Woodrow
>color = red

This defines a file without any explicit section, so all items will
occur within the default section @DEFAULT@. The @debug@ option can be read
as a boolean or a string. The remaining items can be read as a string
only. The @names@ entry spans two lines -- any line starting with
whitespace, and containing something other than whitespace or
comments, is taken as a continuation of the previous line.

Here's another example:

># Default options
>[DEFAULT]
>hostname: localhost
># Options for the first file
>[file1]
>location: /usr/local
>user: Fred
>uid: 1000
>optionaltext: Hello, this  entire string is included
>[file2]
>location: /opt
>user: Fred
>uid: 1001

This file defines three sections. The @DEFAULT@ section specifies an
entry @hostname@. If you attempt to read the hostname option in any
section, and that section doesn't define @hostname@, you will get the
value from @DEFAULT@ instead. This is a nice time-saver. You can also
note that you can use colons instead of the = character to separate
option names from option entries.
-}

{- $whitespace

Whitespace (spaces, tabs, etc) is automatically stripped from the
beginning and end of all strings. Thus, users can insert whitespace
before\/after the colon or equal sign if they like, and it will be
automatically stripped.

Blank lines or lines consisting solely of whitespace are ignored.

A line giving an option or a section name may not begin with white space.
This requirement is necessary so there is no ambiguity between such lines
and continuation lines for multi-line options.

-}

{- $comments

Comments are introduced with the pound sign @#@ or the semicolon @;@. They
cause the parser to ignore everything from that character to the end
of the line.

Comments /may not/ occur within the definitions of options; that is, you
may not place a comment in the middle of a line such as @user: Fred@.
That is because the parser considers the comment characters part
of the string; otherwise, you'd be unable to use those characters in
your strings. You can, however, \"comment out\" options by putting the
comment character at the start of the line.

-}

{- $casesens

By default, section names are case-sensitive but option names are
not. The latter can be adjusted by adjusting 'optionNameTransform'.  -}

{- $interpolation

Interpolation is an optional feature, disabled by default.  If you replace
the default 'accessFunction' ('simpleAccess') with 'interpolatingAccess',
then you get interpolation support with 'get' and the other 'get'-based functions.

As an example, consider the following file:

>arch = i386
>project = test
>filename = test_%(arch)s.c
>dir = /usr/src/%(filename)s
>percent = 5%%

With interpolation, you would get these results:

>get cp "DEFAULT" "filename" -> "test_i386.c"
>get cp "DEFAULT" "dir" -> "/usr/src/test_i386.c"
>get cp "DEFAULT" "percent" -> "5%"

For more details on interpolation, please see the documentation for the
'interpolatingAccess' function.
-}

{- $usage

The basic theory of working with ConfigParser is this:

 1. Parse or build a 'ConfigParser' object

 2. Work with it in one of several ways

 3. To make changes, you discard the original object and use a new one.
    Changes can be "chained" through one of several monads.

The default 'ConfigParser' object that you always start with is 'emptyCP'.
From here, you load data into it (merging data into the empty object),
set up structures yourself, or adjust options.

Let's take a look at some basic use cases.

-}

{- $usagenomonad
You'll notice that many functions in this module return a
@MonadError 'ConfigError'@ over some
type.  Although its definition is not this simple, you can consider this to be
the same as returning @Either ConfigError a@.

That is, these functions will return @Left error@ if there's a problem
or @Right result@ if things are fine.  The documentation for individual
functions describes the specific circumstances in which an error may occur in
more detail.

Some people find it annoying to have to deal with errors manually.
You can transform errors into exceptions in your code by using
'Data.Either.Utils.forceEither'.  Here's an example of this style of programming:

> import Data.Either.Utils
> do
>    val <- readFromFile emptyCP "/etc/foo.cfg"
>    let cp = forceEither val
>    putStrLn "Your setting is:"
>    putStrLn $ forceEither $ get cp "sect1" "opt1"

In short, you can just put @forceEither $@ in front of every call that returns
something that is a MonadError.
This is still a pure functional call, so it can be used outside
of the IO monads.  The exception, however, can only be caught in the IO
monad.

If you don't want to bother with 'forceEither', you can use the error monad.  It's simple and better... read on.
-}

{- $usageerrormonad

The return type is actually defined in terms of the Error monad, which is
itself based on the Either data type.

Here's a neat example of chaining together calls to build up a 'ConfigParser'
object:

>do let cp = emptyCP
>   cp <- addSection cp "sect1"
>   cp <- set cp "sect1" "opt1" "foo"
>   cp <- set cp "sect1" "opt2" "bar"
>   options cp "sect1"

The return value of this little snippet is @Right [\"opt1\", \"opt2\"]@.
(Note to beginners: unlike the IO monad, you /can/ escape from the Error
monad.)

Although it's not obvious, there actually was error checking there.  If
any of those calls would have generated an error, processing would have
stopped immediately and a @Left@ value would have been returned.  Consider
this example:

>do let cp = emptyCP
>   cp <- addSection cp "sect1"
>   cp <- set cp "sect1" "opt1" "foo"
>   cp <- set cp "sect2" "opt2" "bar"
>   options cp "sect1"

The return value from this is @Left ('NoSection' \"sect2\", \"set\")@.  The
second call to 'set' failed, so the final call was skipped, and the result
of the entire computation was considered to be an error.

You can combine this with the non-monadic style to get a final, pure value
out of it:

>forceEither $ do let cp = emptyCP
>                 cp <- addSection cp "sect1"
>                 cp <- set cp "sect1" "opt1" "foo"
>                 cp <- set cp "sect1" "opt2" "bar"
>                 options cp "sect1"

This returns @[\"opt1\", \"opt2\"]@.  A quite normal value.

-}

{- $usageerroriomonad

You've seen a nice way to use this module in the Error monad and get an Either
value out.  But that's the Error monad, so IO is not permitted.
Using Haskell's monad transformers, you can run it in the combined
Error\/IO monad.  That is, you will get an IO result back.  Here is a full
standalone example of doing that:

>import Polar.ConfigFile
>import Control.Monad.Error
>
>main = do
>          rv <- runErrorT $
>              do
>              cp <- join $ liftIO $ readFromFile emptyCP "/etc/passwd"
>              let x = cp
>              liftIO $ putStrLn "In the test"
>              nb <- get x "DEFAULT" "nobody"
>              liftIO $ putStrLn nb
>              foo <- get x "DEFAULT" "foo"
>              liftIO $ putStrLn foo
>              return "done"
>          print rv

On my system, this prints:

>In the test
>x:65534:65534:nobody:/nonexistent:/bin/sh
>Left (NoOption "foo","get")

That is, my @\/etc\/passwd@ file contains a @nobody@ user but not a @foo@ user.

Let's look at how that works.

First, @main@ always runs in the IO monad only, so we take the result from
the later calls and put it in @rv@.  Note that the combined block
is started with @runErrorT $ do@ instead of just @do@.

To get something out of the call to 'readFromFile', we use
@join $ liftIO $ readFromFile@.  This will bring the result out of the IO monad
into the combined monad and process it like usual.  From here on,
everything looks normal, except for IO calls.  They are all executed under
@liftIO@ so that the result value is properly brought into the combined
monad.  This finally returns @\"done\"@.  Since we are in the Error monad, that means that the literal value is @Right \"done\"@.  Since we are also in the IO
monad, this is wrapped in IO.  So the final return type after applying
@runErrorT@ is @IO (Either ConfigError String)@.

In this case, there was an error, and processing stopped at that point just
like the example of the pure Error monad.  We print out the return value,
so you see the error displayed as a @Left@ value.

It all works quite easily.

-}

{- $configuringcp

You may notice that the 'ConfigParser' object has some configurable parameters,
such as 'useDefault'.  In case you're not familiar with the Haskell syntax
for working with these, you can use syntax like this to set these options:

>let cp2 = cp { useDefault = False }

This will create a new 'ConfigParser' that is the same as @cp@ except for
the 'useDefault' field, which is now always False.  The new object will be
called @cp2@ in this example.
-}

{- $reading

You can use these functions to read data from a file.

A common idiom for loading a new object from stratch is:

@cp <- 'readFromFile' 'emptyCP' \"\/etc\/foo.cfg\"@

Note the use of 'emptyCP'; this will essentially cause the file's data
to be merged with the empty 'ConfigParser'.
-}

{- $types

The code used to say this:

>type CPResult a = MonadError ConfigError m => m a
>simpleAccess :: ConfigParser -> SectionName -> OptionName -> CPResult String

But Hugs did not support that type declaration.  Therefore, types are now
given like this:

>simpleAccess :: MonadError ConfigError m =>
>                ConfigParser -> SectionName -> OptionName -> m String

Although it looks more confusing than before, it still means the same.
The return value can still be treated as @Either ConfigError String@ if you so
desire.
-}
