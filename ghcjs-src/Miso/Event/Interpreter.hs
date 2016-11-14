{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Miso.Event.Interpreter ( evalEventGrammar ) where

import           Control.Monad.Free.Church
import           Data.Aeson                 hiding (Value(..), Object)
import           GHCJS.Marshal
import           GHCJS.Types
import           JavaScript.Array.Internal
import           JavaScript.Object
import           JavaScript.Object.Internal
import           GHCJS.Nullable
import           Miso.FFI
import           Miso.Html.Types.Event

evalEventGrammar :: JSVal -> Grammar a -> IO a
evalEventGrammar e = do
   iterM $ \x ->
     case x of
       GetEvent cb -> cb e
       GetTarget cb -> cb =<< getProp "target" (Object e)
       GetParent obj cb -> cb =<< getProp "parentNode" (Object obj)
       GetField key obj cb ->
         cb . nullableToMaybe =<< Nullable <$> getProp key (Object obj)
       SetField key val cb -> do
         jsValue <- toJSVal (toJSON val)
         setProp key jsValue (Object e) >> cb
       GetChildren obj cb ->
         cb =<< getProp "childNodes" (Object obj)
       GetItem obj n cb ->
         cb . nullableToMaybe =<< Nullable <$> item obj n
       Stringify val cb -> do
         Just v <- fmap fromJSON <$> fromJSVal val
         case v of
           Error err -> error $ "Decode failure: " ++ err
           Success s -> cb s
       ConsoleLog o cb -> do
         cb =<< Miso.FFI.consoleLog o
       GetNextSibling obj cb ->
         cb =<< getProp "nextSibling" (Object obj)
       Apply obj str xs cb ->
         cb =<< applyFunction obj str
            =<< fromList <$> mapM toJSVal xs

foreign import javascript unsafe "$r = $1[$2]"
  item :: JSVal -> Int -> IO JSVal

foreign import javascript unsafe "$r = $1[$2].apply($1, $3);"
  applyFunction :: JSVal -> JSString -> JSArray -> IO JSVal
