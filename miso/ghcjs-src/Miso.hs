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
import Control.Monad
import Data.IORef
import JavaScript.Web.AnimationFrame

import Miso.Concurrent
import Miso.Effect
import Miso.Event.Delegate
import Miso.Html
import Miso.Html.Diff
import Miso.Html.Types
import Miso.Isomorphic
import Miso.Settings
import Miso.Signal
import Miso.Types

startApp
  :: ( HasAction action model stepConfig
     , Eq model
     ) => model
       -> (model -> View action)
       -> (action -> model -> Effect action model)
       -> Settings stepConfig action
       -> IO ()
startApp initialModel view update Settings{..} = do
  let mergedSignals = mergeManySignals (fst defaultSignal : extraSignals)
  -- If isomorphic, then copy the dom into the `initialTree`, forego initial diff
  initialVTree <- runView (view initialModel)
  -- Draw initial tree, where isomorphic should be used, remove initial diff?
  if useIsomorphic
    then copyDOMIntoVTree initialVTree
    else Nothing `diff` (Just initialVTree)
  vTreeRef <- newIORef initialVTree
  -- Begin listening for events in the virtual dom
  void . forkIO $ delegator vTreeRef events
  -- /end delegator fork
  step <- start $ foldp stepConfig update initialModel mergedSignals
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
