{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE KindSignatures  #-}
module Miso
  ( startApp
  , module Miso.Html
  , module Miso.Html.Types
  , module Miso.Types
  , module Miso.Settings
  , module Miso.Effect
  ) where

import Control.Concurrent
import Data.IORef
import GHC.TypeLits
import Control.Monad
import FRP.Elerea.Simple
import JavaScript.Web.AnimationFrame

import Miso.Html.Types
import Miso.Settings
import Miso.Effect
import Miso.Concurrent
import Miso.Event.Delegate
import Miso.Html.Diff
import Miso.Html.Internal
import Miso.Html
import Miso.Isomorphic
import Miso.Signal
import Miso.Types                    hiding (ToAction)
import Miso.Concurrent ( Notify (..), notifier )

startApp
  :: ( HasAction action model stepConfig
     , ToAction action
     , ExtractEvents events, Eq model
     ) => model
       -> (model -> View action)
       -> (action -> model -> Effect action model)
       -> Settings (events :: [(Symbol, Bool)]) stepConfig action
       -> IO ()
startApp initialModel view update Settings{..} = do
  (sig, writer) <- signal
  let mergedSignals = mergeManySignals (sig : extraSignals)
  -- If isomorphic, then copy the dom into the `initialTree`, forego initial diff
  initialVTree <- runView (view initialModel)
  -- Draw initial tree, where isomorphic should be used, remove initial diff?
  if useIsomorphic
    then do
      copyDOMIntoVTree initialVTree
    else do
      Nothing `diff` (Just initialVTree)

  vTreeRef <- newIORef initialVTree

  -- Begin listening for events in the virtual dom
  void . forkIO $ delegator writer vTreeRef events notify
  -- /end delegator fork
  step <- start $ foldp stepConfig update initialModel mergedSignals writer
  forever $ do
    draw notifier >> step >>= \case
      NotChanged _ -> pure ()
      Changed changedModel -> do
        -- Can we perform caching here?
        newVTree <- runView $ view changedModel
        oldVTree <- readIORef vTreeRef
        void $ waitForAnimationFrame
        Just oldVTree `diff` Just newVTree
        writeIORef vTreeRef newVTree

