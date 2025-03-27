{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad.State
import Control.Concurrent.MVar
import Miso
import Miso.String

#ifdef GHCJS_NEW
import GHC.JS.Foreign.Callback
#endif
import GHCJS.Types
#ifdef GHCJS_OLD
import GHCJS.Foreign.Callback
#endif

-- | Model
newtype Model
  = Model
  { info :: MisoString
  } deriving (Eq, Show)

-- | Action
data Action
    = ReadFile
    | SetContent MisoString
    deriving (Show, Eq)

-- | Main entry point
main :: IO ()
main = run $ startApp (defaultApp (Model "") updateModel viewModel)

-- | Update your model
updateModel :: Action -> Effect Model Action ()
updateModel ReadFile = do
    m <- get
    m <# do
        fileReaderInput <- getElementById "fileReader"
        file <- getFile fileReaderInput
        reader <- newReader
        mvar <- newEmptyMVar
        setOnLoad reader =<< do
            asyncCallback $ do
                r <- getResult reader
                putMVar mvar r
        readText reader file
        SetContent <$> readMVar mvar
updateModel (SetContent c) = modify $ \m -> m { info = c }

-- | View function, with routing
viewModel :: Model -> View Action
viewModel Model{..} = view
  where
    view =
        div_
            []
            [ "FileReader API example"
            , input_
                [ id_ "fileReader"
                , type_ "file"
                , onChange (const ReadFile)
                ]
            , div_ [] [text info]
            ]

#ifdef GHCJS_NEW
foreign import javascript unsafe "(() => { return new FileReader(); })"
  newReader :: IO JSVal

foreign import javascript unsafe "((x) => { return x.files[0]; })"
  getFile :: JSVal -> IO JSVal

foreign import javascript unsafe "((x, y) => { x.onload = y; })"
  setOnLoad :: JSVal -> Callback (IO ()) -> IO ()

foreign import javascript unsafe "((x) => { return x.result; })"
  getResult :: JSVal -> IO MisoString

foreign import javascript unsafe "((x, y) => { x.readAsText(y); })"
  readText :: JSVal -> JSVal -> IO ()
#endif

#ifdef GHCJS_OLD
foreign import javascript unsafe "$r = new FileReader();"
  newReader :: IO JSVal

foreign import javascript unsafe "$r = $1.files[0];"
  getFile :: JSVal -> IO JSVal

foreign import javascript unsafe "$1.onload = $2;"
  setOnLoad :: JSVal -> Callback (IO ()) -> IO ()

foreign import javascript unsafe "$r = $1.result;"
  getResult :: JSVal -> IO MisoString

foreign import javascript unsafe "$1.readAsText($2);"
  readText :: JSVal -> JSVal -> IO ()
#endif
