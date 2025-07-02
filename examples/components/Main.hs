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
import System.IO.Unsafe (unsafePerformIO)
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
  | Notification Message
  deriving (Show, Eq, Generic)
-----------------------------------------------------------------------------
data Message
  = Increment
  | Decrement
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
-----------------------------------------------------------------------------
main :: IO ()
main = run (startComponent server)

-- TODO we need to be able to perform side effects when creating a component
{-# NOINLINE serverInitialModel #-}
serverInitialModel :: TopicName Message
serverInitialModel =
  unsafePerformIO $ do
    putStrLn "unsafe IO being performed!"
    mkTopic "command"
-----------------------------------------------------------------------------
-- | Demonstrates a simple server / client, pub / sub setup for 'Component'
-- In this contrived example, the server component holds the
-- incrementing / decrementing actions, and relays them to the clients
-- via the pub / sub mechanism.
--
-- Notice the server has no 'model' (e.g. `()`)
--
server :: Component (TopicName Message) Action
server = component serverInitialModel update_ $ \topic ->
  div_
  []
  [ "Server component"
  , button_ [ onClick AddOne ] [ "+" ]
  , button_ [ onClick SubtractOne ] [ "-" ]
  , component_ (client_ topic "client 1")
    [ onMountedWith Mount
    ]
  , component_ (client_ topic "client 2")
    [ onMountedWith Mount
    ]
  ] where
      update_ :: Action -> Effect (TopicName Message) Action
      update_ = \case
        AddOne -> do
          topic <- get
          publish topic Increment
        SubtractOne -> do
          topic <- get
          publish topic Decrement
        Mount compId ->
          io_ $ consoleLog ("Mounted component: " <> compId)
        _ -> pure ()
-----------------------------------------------------------------------------
client_ :: TopicName Message -> MisoString -> Component Int Action
client_ topic name = (clientComponent topic name) { initialAction = Just Subscribe }
-----------------------------------------------------------------------------
clientComponent :: TopicName Message -> MisoString -> Component Int Action
clientComponent topic name = component 0 update_ $ \m ->
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
          unsubscribe topic
        Subscribe -> do
          io_ (consoleLog "subscribing...")
          subscribe topic Notification
        Notification Increment -> do
          io_ $ consoleLog "inside of increment"
          update_ AddOne
        Notification Decrement -> do
          io_ $ consoleLog "inside of decrement"
          update_ SubtractOne
        _ -> pure ()

-----------------------------------------------------------------------------
