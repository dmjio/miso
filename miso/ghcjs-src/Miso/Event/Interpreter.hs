{-# LANGUAGE ScopedTypeVariables #-}
module Miso.Event.Interpreter where

import           Control.Monad.Free.Church
import qualified Data.Aeson                 as A
import           Data.Aeson                 hiding (Value(..), Object)
import           Data.Monoid
import qualified GHCJS.DOM.Event            as E
import           GHCJS.DOM.EventTarget
import           GHCJS.DOM.Node             as Node
import           GHCJS.DOM.NodeList
import           GHCJS.DOM.Types
import qualified GHCJS.Foreign.Internal     as Foreign
import           GHCJS.Marshal
import           GHCJS.Marshal.Pure
import           GHCJS.Types
import           JavaScript.Object
import           JavaScript.Object.Internal
import           JavaScript.Array.Internal
import           Miso.Html.Types.Event

evalEventGrammar :: JSVal -> Grammar a -> IO a
evalEventGrammar e = do
   iterM $ \x ->
     case x of
       GetTarget cb -> do
         Just target :: Maybe EventTarget <- E.getTarget (Event e)
         cb (pToJSVal target)
       GetParent obj cb -> do
         Just p <- getParentNode (pFromJSVal obj :: Node)
         cb (pToJSVal p)
       GetField key obj cb -> do
         val <- getProp key (Object obj)
         cb =<< jsToJSON (Foreign.jsTypeOf val) val
       GetEventField key cb -> do
         eventVal <- toJSVal e
         val <- getProp key (Object eventVal)
         cb =<< jsToJSON (Foreign.jsTypeOf val) val
       SetEventField key val cb -> do
         eventVal <- toJSVal e
         jsValue <- toJSVal (toJSON val)
         setProp key jsValue (Object eventVal) >> cb
       GetChildren obj cb -> do
         Just nodeList <- getChildNodes (pFromJSVal obj :: Node)
         cb $ pToJSVal nodeList
       GetItem obj n cb -> do
         result <- item (pFromJSVal obj :: NodeList) (fromIntegral n)
         cb $ pToJSVal <$> result
       GetNextSibling obj cb -> do
         result <- Node.getNextSibling (pFromJSVal obj :: Node)
         cb $ pToJSVal <$> result
       ApplyFunction obj str xs cb ->
         cb =<< convertToJSON
            =<< apply (Object obj) str
            =<< fromList <$> mapM toJSVal (map toJSON xs)

foreign import javascript unsafe "$r = $1[$2].apply(this, $3);"
  apply :: Object -> JSString -> JSArray -> IO JSVal

jsToJSON :: FromJSON v => Foreign.JSType -> JSVal -> IO (Maybe v)
jsToJSON Foreign.Number  g = convertToJSON g
jsToJSON Foreign.Boolean g = convertToJSON g
jsToJSON Foreign.Object  g = convertToJSON g
jsToJSON Foreign.String  g = convertToJSON g
jsToJSON _ _ = pure Nothing

convertToJSON :: FromJSON v => JSVal -> IO (Maybe v)
convertToJSON g = do
  Just (val :: A.Value) <- fromJSVal g
  case fromJSON val of -- Should *always* be able to decode this
     Error e -> Prelude.error $ "Error while decoding Value: " <> e <> " " <> show val
     Success v -> pure (pure v)
