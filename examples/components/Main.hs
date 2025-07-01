-----------------------------------------------------------------------------
{-# LANGUAGE CPP                #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE OverloadedStrings  #-}
-----------------------------------------------------------------------------
module Main where
-----------------------------------------------------------------------------
import           GHC.Generics
import           Data.Aeson
-----------------------------------------------------------------------------
import           Miso
import           Miso.String
import           Miso.Lens
-----------------------------------------------------------------------------
#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif
-----------------------------------------------------------------------------
data Action
  = AddOne
  | SubtractOne
  | Mount MisoString
  | Subscribe
  | Unsubscribe
  | Notification (Result Message)
  deriving (Show, Eq, Generic)
-----------------------------------------------------------------------------
data Message
  = Increment
  | Decrement
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
-----------------------------------------------------------------------------
main :: IO ()
main = run (startComponent server)
-----------------------------------------------------------------------------
-- | Demonstrates a simple server / client, pub / sub setup for 'Component'
-- In this contrived example, the server component holds the
-- incrementing / decrementing actions, and relays them to the clients
-- via the pub / sub mechanism.
--
-- Notice the server has no 'model' (e.g. `()`)
--
server :: Component () Action
server = defaultComponent () update_ $ \() ->
  div_
  []
  [ "Server component"
  , button_ [ onClick AddOne ] [ "+" ]
  , button_ [ onClick SubtractOne ] [ "-" ]
  , component_ (client_ "client 1")
    [ onMountedWith Mount
    ]
  , component_ (client_ "client 2")
    [ onMountedWith Mount
    ]
  ] where
      update_ :: Action -> Effect () Action
      update_ = \case
        AddOne ->
          publish "command" Increment
        SubtractOne ->
          publish "command" Decrement
        Mount compId ->
          io_ $ consoleLog ("Mounted component: " <> compId)
        _ -> pure ()
-----------------------------------------------------------------------------
client_ :: MisoString -> Component Int Action
client_ name = (clientComponent name) { initialAction = Just Subscribe }
-----------------------------------------------------------------------------
clientComponent :: MisoString -> Component Int Action
clientComponent name = defaultComponent 0 update_ $ \m ->
  div_
  []
  [ br_ []
  , text (name <> " : " <> ms (m ^. _id))
  , button_ [ onClick Unsubscribe ] [ "unsubscribe" ]
  , button_ [ onClick Subscribe ] [ "subscribe" ]
  ] where
      update_ :: Action -> Effect Int Action
      update_ = \case
        AddOne -> do
          _id += 1
        SubtractOne ->
          _id -= 1
        Unsubscribe ->
          unsubscribe "command"
        Subscribe -> do
          io_ (consoleLog "subscribing...")
          subscribe "command" Notification
        Notification (Success Increment) -> do
          io_ $ consoleLog "inside of success increment"
          update_ AddOne
        Notification (Success Decrement) -> do
          io_ $ consoleLog "inside of success decrement"
          update_ SubtractOne
        Notification msg ->
          io_ $ consoleError ("Decode failure: " <> ms (show msg))
        _ -> pure ()
-----------------------------------------------------------------------------
