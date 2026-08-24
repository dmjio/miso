{-# LANGUAGE BlockArguments #-}

module Miso.Trace where

import Miso.String
import Miso.FFI
import System.IO.Unsafe (unsafePerformIO)
import Prelude

trace :: (ToMisoString s) => s -> a -> a
trace = traceTo consoleLog

traceId :: (ToMisoString s) => s -> s
traceId = traceWith id

traceWith :: (ToMisoString s) => (a -> s) -> a -> a
traceWith f a = trace (f a) a

traceShow :: (Show a) => a -> b -> b
traceShow = trace . show

traceShowId :: (Show a) => a -> a
traceShowId = traceWith show

traceShowWith :: (Show b) => (a -> b) -> a -> a
traceShowWith f = traceWith (show . f)

traceM :: (ToMisoString s, Applicative f) => s -> f ()
traceM s = trace s $ pure ()

traceShowM :: (Show a, Applicative f) => a -> f ()
traceShowM = traceM . show

traceError :: (ToMisoString s) => s -> a -> a
traceError = traceTo consoleError

traceErrorId :: (ToMisoString s) => s -> s
traceErrorId = traceErrorWith id

traceErrorWith :: (ToMisoString s) => (a -> s) -> a -> a
traceErrorWith f a = traceError (f a) a

traceErrorShow :: (Show a) => a -> b -> b
traceErrorShow = traceError . show

traceErrorShowId :: (Show a) => a -> a
traceErrorShowId = traceErrorWith show

traceErrorShowWith :: (Show b) => (a -> b) -> a -> a
traceErrorShowWith f = traceErrorWith (show . f)

traceErrorM :: (ToMisoString s, Applicative f) => s -> f ()
traceErrorM s = traceError s $ pure ()

traceErrorShowM :: (Show a, Applicative f) => a -> f ()
traceErrorShowM = traceErrorM . show

traceWarn :: (ToMisoString s) => s -> a -> a
traceWarn = traceTo consoleWarn

traceWarnId :: (ToMisoString s) => s -> s
traceWarnId = traceWarnWith id

traceWarnWith :: (ToMisoString s) => (a -> s) -> a -> a
traceWarnWith f a = traceWarn (f a) a

traceWarnShow :: (Show a) => a -> b -> b
traceWarnShow = traceWarn . show

traceWarnShowId :: (Show a) => a -> a
traceWarnShowId = traceWarnWith show

traceWarnShowWith :: (Show b) => (a -> b) -> a -> a
traceWarnShowWith f = traceWarnWith (show . f)

traceWarnM :: (ToMisoString s, Applicative f) => s -> f ()
traceWarnM s = traceWarn s $ pure ()

traceWarnShowM :: (Show a, Applicative f) => a -> f ()
traceWarnShowM = traceWarnM . show

traceTo :: (ToMisoString s) => (MisoString -> IO ()) -> s -> a -> a
traceTo f s a = unsafePerformIO do
    f $ toMisoString s
    pure a
