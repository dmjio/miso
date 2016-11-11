{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Miso.Event.Interpreter where

import           Control.Monad.Free.Church
import qualified Data.Aeson                 as A
import           Data.Aeson                 hiding (Value(..), Object)
import           Data.Monoid
import           GHCJS.Marshal
import           GHCJS.Nullable
import           GHCJS.Types
import           JavaScript.Array.Internal
import           JavaScript.Object
import           JavaScript.Object.Internal
import           Miso.Html.Types.Event

evalEventGrammar :: JSVal -> Grammar a -> IO a
evalEventGrammar e = do
   iterM $ \x ->
     case x of
       GetTarget cb ->
         cb =<< getProp "target" (Object e)
       GetParent obj cb -> do
         cb =<< getProp "parentNode" (Object obj)
       GetField key obj cb ->
         cb =<< convertToJSON
            =<< getProp key (Object obj)
       GetEventField key cb -> do
         cb =<< convertToJSON
            =<< getProp key (Object e)
       SetEventField key val cb -> do
         jsValue <- toJSVal (toJSON val)
         setProp key jsValue (Object e) >> cb
       GetChildren obj cb ->
         cb =<< getProp "childNodes" (Object obj)
       GetItem obj n cb ->
         cb . nullableToMaybe =<< Nullable <$> item obj n
       GetNextSibling obj cb ->
         cb . nullableToMaybe =<<
           Nullable <$> getProp "nextSibling" (Object obj)
       Apply obj str xs cb ->
         cb =<< convertToJSON
            =<< applyFunction (Object obj) str
            =<< fromList <$> mapM toJSVal (map toJSON xs)

foreign import javascript unsafe "$r = $1[$2]"
  item :: JSVal -> Int -> IO JSVal

foreign import javascript unsafe "$r = $1[$2].apply($1, $3);"
  applyFunction :: Object -> JSString -> JSArray -> IO JSVal

convertToJSON :: FromJSON v => JSVal -> IO (Maybe v)
convertToJSON g = do
  Just (val :: A.Value) <- fromJSVal g
  case fromJSON val of -- Should *always* be able to decode this
     Error e -> Prelude.error $ "Error while decoding Value: " <> e <> " " <> show val
     Success v -> pure (pure v)
