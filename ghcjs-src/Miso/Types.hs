-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Types
-- Copyright   :  (C) 2016-2018 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Types
  ( App (..)

    -- * The Transition Monad
  , Transition
  , fromTransition
  , toTransition
  , scheduleIO
  , scheduleSub
  ) where

import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.State.Strict (StateT(StateT), execStateT)
import           Control.Monad.Trans.Writer.Strict (WriterT(WriterT), Writer, runWriter, tell)
import qualified Data.Map           as M
import           Miso.Effect
import           Miso.Html.Internal
import           Miso.String

-- | Application entry point
data App model action = App
  { model :: model
  -- ^ initial model
  , update :: action -> model -> Effect action model
  -- ^ Function to update model, optionally provide effects.
  --   See the 'Transition' monad for succinctly expressing model transitions.
  , view :: model -> View action
  -- ^ Function to draw `View`
  , subs :: [ Sub action ]
  -- ^ List of subscriptions to run during application lifetime
  , events :: M.Map MisoString Bool
  -- ^ List of delegated events that the body element will listen for
  , initialAction :: action
  -- ^ Initial action that is run after the application has loaded
  , mountPoint :: Maybe MisoString
  -- ^ root element for DOM diff
  }

-- | A monad for succinctly expressing model transitions in the 'update' function.
--
-- @Transition@ is a state monad so it abstracts over manually passing the model
-- around. It's also a writer monad where the accumulator is a list of scheduled
-- IO actions. Multiple actions can be scheduled using
-- @Control.Monad.Writer.Class.tell@ from the @mtl@ library and a single action
-- can be scheduled using 'scheduleIO'.
--
-- Tip: use the @Transition@ monad in combination with the stateful
-- <http://hackage.haskell.org/package/lens-4.15.4/docs/Control-Lens-Operators.html lens>
-- operators (all operators ending in "@=@"). The following example assumes
-- the lenses @field1@, @counter@ and @field2@ are in scope and that the
-- @LambdaCase@ language extension is enabled:
--
-- @
-- myApp = App
--   { update = 'fromTransition' . \\case
--       MyAction1 -> do
--         field1 .= value1
--         counter += 1
--       MyAction2 -> do
--         field2 %= f
--         scheduleIO $ do
--           putStrLn \"Hello\"
--           putStrLn \"World!\"
--   , ...
--   }
-- @
type Transition action model = StateT model (Writer [Sub action])

-- | Convert a @Transition@ computation to a function that can be given to 'update'.
fromTransition
    :: Transition action model ()
    -> (model -> Effect action model) -- ^ model 'update' function.
fromTransition act = uncurry Effect . runWriter . execStateT act

-- | Convert an 'update' function to a @Transition@ computation.
toTransition
    :: (model -> Effect action model) -- ^ model 'update' function
    -> Transition action model ()
toTransition f = StateT $ \s ->
                   let Effect s' ios = f s
                   in WriterT $ pure (((), s'), ios)

-- | Schedule a single IO action for later execution.
--
-- Note that multiple IO action can be scheduled using
-- @Control.Monad.Writer.Class.tell@ from the @mtl@ library.
scheduleIO :: IO action -> Transition action model ()
scheduleIO ioAction = scheduleSub $ \sink -> ioAction >>= sink

-- | Like 'scheduleIO' but schedules a subscription which is an IO
-- computation that has access to a 'Sink' which can be used to
-- asynchronously dispatch actions to the 'update' function.
--
-- A use-case is scheduling an IO computation which creates a
-- 3rd-party JS widget which has an associated callback. The callback
-- can then call the sink to turn events into actions. To do this
-- without accessing a sink requires going via a @'Sub'scription@
-- which introduces a leaky-abstraction.
scheduleSub :: Sub action -> Transition action model ()
scheduleSub sub = lift $ tell [ sub ]
