{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE DeriveAnyClass            #-}
module Miso where

import           Control.Concurrent
import           Control.Lens.Indexed          (imapM_)
import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.Free.Church
import           Control.Monad.State
import           Data.Aeson                    hiding (Object)
import           Data.Bool
import qualified Data.Foldable                 as F
import           Data.IORef
import           Data.JSString.Text
import           Data.List
import qualified Data.Map                      as M
import           Data.Maybe
import           Data.Monoid
import           Data.Proxy
import qualified Data.String.Conversions       as CS
import qualified Data.Text                     as T
import qualified Data.Vector                   as V
import qualified Data.Vector.Mutable           as MV
import qualified FRP.Elerea.Simple             as FRP
import           FRP.Elerea.Simple             hiding (Signal)
import           GHC.TypeLits
import           GHCJS.DOM
import           GHCJS.DOM.CharacterData
import           GHCJS.DOM.Document            hiding (drop, getLocation, focus, input)
import           GHCJS.DOM.Element             (removeAttribute, setAttribute, focus)
import           GHCJS.DOM.Event               (Event)
import qualified GHCJS.DOM.Event               as E
import           GHCJS.DOM.EventTarget         (addEventListener)
import           GHCJS.DOM.EventTargetClosures
import qualified GHCJS.DOM.Node                as Node
import           GHCJS.DOM.Node                hiding (getNextSibling)
import           GHCJS.DOM.NodeList            hiding (getLength)
import qualified GHCJS.DOM.Storage             as S
import           GHCJS.DOM.Types               hiding (Event, Attr, CSS)
import           GHCJS.DOM.Window              (getLocalStorage, getSessionStorage)
import           GHCJS.Foreign                 hiding (Object, Number)
import qualified GHCJS.Foreign.Internal        as Foreign
import           GHCJS.Marshal
import           GHCJS.Marshal.Pure
import qualified GHCJS.Types                   as G
import           JavaScript.Object.Internal
import           JavaScript.Web.AnimationFrame
import           Miso.Html
import           Prelude                       hiding (repeat)

type Signal a = SignalGen (FRP.Signal a)

data Sample a = Changed a | NotChanged a
  deriving (Functor, Show)

fromChanged :: Sample a -> a
fromChanged (Changed x) = x
fromChanged (NotChanged x) = x

jsToJSON :: FromJSON v => JSType -> G.JSVal -> IO (Maybe v)
jsToJSON Foreign.Number  g = convertToJSON g
jsToJSON Foreign.Boolean g = convertToJSON g
jsToJSON Foreign.Object  g = convertToJSON g
jsToJSON Foreign.String  g = convertToJSON g
jsToJSON _ _ = pure Nothing

convertToJSON :: FromJSON v => G.JSVal -> IO (Maybe v)
convertToJSON g = do
  Just (val :: Value) <- fromJSVal g
  case fromJSON val of -- Should *always* be able to decode this
    Error e -> Prelude.error $ "Error while decoding Value: " <> e <> " " <> show val
    Success v -> pure (pure v)

evalEventGrammar :: Event -> Grammar G.JSVal a -> IO a
evalEventGrammar e = do
  iterM $ \x ->
    case x of
      GetTarget cb ->
        cb =<< pToJSVal <$> E.getTarget e
      GetParent obj cb -> do
        Just p <- getParentNode (pFromJSVal obj :: Node)
        cb (pToJSVal p)
      GetField key obj cb -> do
        val <- getProp (textToJSString key) (Object obj)
        cb =<< jsToJSON (jsTypeOf val) val
      GetEventField key cb -> do
        eventVal <- toJSVal e
        val <- getProp (textToJSString key) (Object eventVal)
        cb =<< jsToJSON (jsTypeOf val) val
      SetEventField key val cb -> do
        eventVal <- toJSVal e
        jsValue <- toJSVal (toJSON val)
        setProp (textToJSString key) jsValue (Object eventVal) >> cb
      GetChildren obj cb -> do
        Just nodeList <- getChildNodes (pFromJSVal obj :: Node)
        cb $ pToJSVal nodeList
      GetItem obj n cb -> do
        result <- item (pFromJSVal obj :: NodeList) (fromIntegral n)
        cb $ pToJSVal <$> result
      GetNextSibling obj cb -> do
        result <- Node.getNextSibling (pFromJSVal obj :: Node)
        cb $ pToJSVal <$> result

data Effect action model
  = Effect model (IO action)
  | NoEffect model
  deriving (Functor)

getModelFromEffect :: Effect action model -> model
getModelFromEffect (Effect m _) = m
getModelFromEffect (NoEffect m) = m

noEffect, noEff :: model -> Effect action model
noEff = noEffect
noEffect = NoEffect

effect :: model -> IO action -> Effect action model
effect = Effect

(<#) :: IO action -> model -> Effect action model
action <# model = Effect model action
infixl 0 <#

class ExtractEvents (events :: [ (Symbol, Bool) ]) where
  extractEvents :: Proxy events -> [(T.Text, Bool)]

instance (ExtractEvents events, KnownSymbol event) =>
  ExtractEvents ('(event, 'True) ': events) where
    extractEvents _ = (eventName, True) : extractEvents (Proxy :: Proxy events)
      where
        eventName = T.pack $ symbolVal (Proxy :: Proxy event)

instance ( ExtractEvents events, KnownSymbol event ) =>
  ExtractEvents ('(event, 'False) ': events) where
    extractEvents _ = (eventName, False) : extractEvents (Proxy :: Proxy events)
      where
        eventName = T.pack $ symbolVal (Proxy :: Proxy event)

instance ExtractEvents '[] where extractEvents = const []

type VTree = VTreeBase Node

delegator
  :: forall action events . ExtractEvents events
  => (action -> IO ())
  -> IORef (VTree action)
  -> Proxy events
  -> IO ()
delegator writer ref Proxy = do
  Just doc <- currentDocument
  Just body <- fmap toNode <$> getBody doc
  listener <- eventListenerNew (f body)
  forM_ (extractEvents (Proxy :: Proxy events)) $ \(event, capture) ->
    addEventListener body event (Just listener) capture
    where
      f :: Node -> E.Event -> IO ()
      f body e = do
        Just target <- E.getTarget e
        vtree <- readIORef ref
        eventType <- T.pack <$> E.getType e
        stack <- buildTargetToBody body (castToNode target)
        delegateEvent e writer vtree eventType stack

buildTargetToBody :: Node -> Node -> IO [Node]
buildTargetToBody body target = f target [target]
    where
      f currentNode nodes
        | body == currentNode = pure (drop 2 nodes)
        | otherwise = do
            Just parent <- getParentNode currentNode
            f parent (parent:nodes)

runEvent
  :: HasEvent eventName returnType
  => Event
  -> (action -> IO ())
  -> Proxy eventName
  -> (returnType -> action)
  -> IO ()
runEvent e writer prox action =
  writer =<< action <$>
    evalEventGrammar e (parseEvent prox)

delegateEvent
  :: forall action . Event
  -> (action -> IO ())
  -> VTree action
  -> T.Text -> [Node] -> IO ()
delegateEvent e writer vnode eventName = do
  void . flip execStateT [] . findEvent (vChildren vnode)
    where
      findEvent _ [] = pure ()
      findEvent childNodes [y] = do
       stopPropagate <- do
         result <- liftIO $ findNode childNodes y
         case result of
           Nothing -> pure False
           Just VText{..} -> pure False
           Just VNode { vEvents = Events events } -> do
             case M.lookup eventName events of
               Nothing -> pure False
               Just (EventHandler options prox action) -> do
                 when (preventDefault options) $ liftIO (E.preventDefault e)
                 liftIO $ runEvent e writer prox action
                 pure $ stopPropagation options
       get >>= propagateWhileAble stopPropagate

      findEvent childNodes (y:ys) = do
        result <- liftIO $ findNode childNodes y
        forM_ result $ \parent@VNode{..} -> do
          modify (parent:)
          findEvent vChildren ys

      propagateWhileAble _ [] = pure ()
      propagateWhileAble True _ = pure ()
      propagateWhileAble False (x:xs) = do
        let Events evts = vEvents x
        case M.lookup eventName evts of
          Nothing ->
            propagateWhileAble False xs
          Just (EventHandler options prox action) -> do
            liftIO $ runEvent e writer prox action
            when (preventDefault options) $ liftIO (E.preventDefault e)
            propagateWhileAble (stopPropagation options) xs

      findNode :: MV.IOVector (VTree a) -> Node -> IO (Maybe (VTree a))
      findNode childNodes targetNode = do
        let n = MV.length childNodes - 1
        listToMaybe . catMaybes <$> do
          forM [0..n] $ \i -> do
            v <- MV.read childNodes i
            case isVNode v of
              True -> do
                Just val <- liftIO $ readIORef (vNode v)
                pure $ case val == targetNode of
                  True -> Just v
                  False -> Nothing
              False -> pure Nothing

isVNode :: VTree a -> Bool
isVNode (VText _ _) = False
isVNode _ = True

initTree :: VTree action -> IO ()
initTree initial = datch Nothing (Just initial)

copyDOMIntoView :: Node -> VTree action -> IO ()
copyDOMIntoView node (VText _ ref) = writeIORef ref (Just node)
copyDOMIntoView node VNode{..} = do
  let n = MV.length vChildren - 1
  forM_ [ 0 .. n ] $ \index -> do
    childNode <- MV.read vChildren index
    Just childNodes <- getChildNodes node
    Just child <- item childNodes (fromIntegral index)
    writeIORef vNode (Just child)
    copyDOMIntoView child childNode

datch :: Maybe (VTree action) -> Maybe (VTree action) -> IO ()
datch currentTree newTree = do
  Just document <- currentDocument
  Just body <- fmap toNode <$> getBody document
  goDatch document body currentTree newTree

goDatch
  :: Document
  -> Node
  -> Maybe (VTreeBase Node action)
  -> Maybe (VTreeBase Node action)
  -> IO ()
goDatch _ _ Nothing Nothing = pure ()
goDatch doc parentNode Nothing (Just (VText str newRef)) = do
  newTextNode <- createTextNode doc str
  void $ appendChild parentNode newTextNode
  writeIORef newRef (castToNode <$> newTextNode)

-- Remove a text node
goDatch _ parentNode (Just (VText _ ref)) Nothing = do
  void $ removeChild parentNode =<< readIORef ref

-- Make a new element
goDatch doc parentNode Nothing (Just VNode{..}) = do
  node@(Just newNode) <- fmap toNode <$> createElement doc (Just vType)
  writeIORef vNode node
  diffAttrs newNode (Attrs mempty) vAttrs
  diffProps newNode (Props mempty) vProps
  diffCss newNode (CSS mempty) vCss
  let n = MV.length vChildren - 1
  forM_ [0..n] $ \i -> do
    childNode <- MV.read vChildren i
    goDatch doc newNode Nothing (Just childNode)
  void $ appendChild parentNode node

-- Remove an element
goDatch _ parentNode (Just VNode{..}) Nothing = do
  void $ removeChild parentNode =<< readIORef vNode

-- Replace an element with a text node
goDatch doc parentNode (Just VNode{..}) (Just (VText str newRef)) = do
  currentNode <- readIORef vNode
  newTextNode <- fmap toNode <$> createTextNode doc str
  writeIORef newRef newTextNode
  void $ replaceChild parentNode newTextNode currentNode

-- Replace a text node with an Element
goDatch doc parentNode (Just (VText _ currentRef)) (Just VNode{..}) = do
  node@(Just newNode) <- fmap toNode <$> createElement doc (Just vType)
  writeIORef vNode node
  diffAttrs newNode (Attrs mempty) vAttrs
  diffProps newNode (Props mempty) vProps
  diffCss newNode (CSS mempty) vCss
  let n = MV.length vChildren - 1
  forM_ [0..n] $ \i -> do
    childNode <- MV.read vChildren i
    goDatch doc newNode Nothing (Just childNode)
  void $ replaceChild parentNode node =<< readIORef currentRef

-- Replace a text node with a text node
goDatch _ _ (Just (VText currentStr currRef)) (Just (VText newStr newRef)) = do
  Just currentNode <- readIORef currRef
  writeIORef newRef (Just currentNode)
  when (currentStr /= newStr) $ do
    let txt = castToText currentNode
    oldLength <- getLength txt
    replaceData txt 0 oldLength newStr

-- Diff two nodes together
goDatch doc parent (Just nodeCurrent) (Just nodeNew) = do
 case vType nodeCurrent == vType nodeNew of
   True -> do
      Just currentNode <- readIORef (vNode nodeCurrent)
      writeIORef (vNode nodeNew) $ Just currentNode
      diffAttrs currentNode (vAttrs nodeCurrent) (vAttrs nodeNew)
      diffProps currentNode (vProps nodeCurrent) (vProps nodeNew)
      diffCss currentNode (vCss nodeCurrent) (vCss nodeNew)
      diffChildren doc currentNode (vChildren nodeCurrent) (vChildren nodeNew)
   False -> do
      node@(Just newNode) <- fmap toNode <$> do
        createElement doc $ Just (vType nodeNew)
      writeIORef (vNode nodeNew) node
      diffAttrs newNode (Attrs mempty) (vAttrs nodeNew)
      diffProps newNode (Props mempty) (vProps nodeNew)
      diffCss newNode (CSS mempty) (vCss nodeNew)
      let n = MV.length (vChildren nodeNew) - 1
      forM_ [0..n] $ \i -> do
        childNode <- MV.read (vChildren nodeNew) i
        goDatch doc newNode Nothing (Just childNode)
      void $ replaceChild parent node =<< readIORef (vNode nodeCurrent)

difference :: Eq a => V.Vector a -> V.Vector a -> V.Vector a
difference xs ys = V.filter (\x -> x `V.notElem` ys) xs

iforM_ :: forall a b.  M.Map T.Text a -> (T.Text -> a -> IO b) -> IO ()
iforM_ = flip imapM_

diffAttrs
  :: Node
  -> Attrs
  -> Attrs
  -> IO ()
diffAttrs node (Attrs current) (Attrs new) = do
  let el = castToElement node
  iforM_ current $ \currentKey currentVal -> do
    case M.lookup currentKey new of
      Nothing -> removeAttribute el currentKey
      Just newVal ->
        when (newVal /= currentVal) $ do
          setAttribute el currentKey newVal

  iforM_ new $ \newKey newVal -> do
    case M.lookup newKey current of
      Nothing -> setAttribute el newKey newVal
      Just _ -> pure ()

diffCss
  :: Node
  -> CSS
  -> CSS
  -> IO ()
diffCss node (CSS current) (CSS new) = do
  obj <- Object <$> toJSVal node
  iforM_ current $ \currentKey currentVal -> do
    case M.lookup currentKey new of
      Nothing -> do
        styleObj <- Object <$> getProp "style" obj
        jv <- toJSVal (mempty :: String)
        setProp (textToJSString currentKey) jv styleObj
      Just newVal ->
        when (newVal /= currentVal) $ do
          styleObj <- Object <$> getProp "style" obj
          jval <- toJSVal newVal
          setProp (textToJSString currentKey) jval styleObj
  iforM_ new $ \newKey newVal -> do
    case M.lookup newKey current of
      Nothing -> do
        styleObj <- Object <$> getProp "style" obj
        jval <- toJSVal newVal
        setProp (textToJSString newKey) jval styleObj
      Just _ -> pure ()

diffProps
  :: Node
  -> Props
  -> Props
  -> IO ()
diffProps node (Props current) (Props new) = do
  obj <- Object <$> toJSVal node
  let el = castToElement node
  iforM_ current $ \currentKey currentVal -> do
    case M.lookup currentKey new of
      Nothing -> do
        setProp (textToJSString currentKey) jsNull obj
      Just newVal -> do
        let key = textToJSString currentKey
        Just domVal <- fromJSVal =<< getProp key obj
        when (newVal /= currentVal && toJSON newVal /= domVal) $ do
          val <- toJSVal newVal
          setProp (textToJSString currentKey) val obj
          dispatchObservable currentKey el

  iforM_ new $ \newKey newVal -> do
    case M.lookup newKey current of
      Nothing -> do
        val <- toJSVal newVal
        setProp (textToJSString newKey) val obj
      Just _ -> pure ()

observables :: M.Map T.Text (Element -> IO ())
observables = M.fromList [("autofocus", focus)]

dispatchObservable :: T.Text -> Element -> IO ()
dispatchObservable key el = do
  F.forM_ (M.lookup key observables) $ \f -> f el

isKeyed :: V.Vector (VTree action) -> Bool
isKeyed vec =
  case vec V.!? 0 of
    Nothing -> False
    Just VNode { vKey = Just _ } -> True
    Just _ -> False

makeMap :: V.Vector (VTree action) -> M.Map Key (VTree action)
makeMap vs = M.fromList [ (key, v) | v@VNode { vKey = Just key } <- V.toList vs ]

diffChildren
  :: Document
  -> Node
  -> MV.IOVector (VTree action)
  -> MV.IOVector (VTree action)
  -> IO ()
diffChildren doc parent as bs =
  diffChildren' doc parent as bs
  -- case isKeyed as of
  --   True -> do
  --     swapKids doc parent (makeMap as) as (makeMap bs) bs
  --     diffChildren' doc parent as bs
  --   False ->
  --     diffChildren' doc parent as bs

diffChildren'
  :: Document
  -> Node
  -> MV.IOVector (VTree action)
  -> MV.IOVector (VTree action)
  -> IO ()
diffChildren' doc parent current new = do
  case (MV.null current, MV.null new) of
    (False, True) -> do
      a <- MV.unsafeRead current 0
      let as = MV.unsafeDrop 1 current
      goDatch doc parent (Just a) Nothing
      MV.unsafeNew 0 >>= \empty -> diffChildren doc parent as empty
    (True, False) -> do
      b <- MV.unsafeRead new 0
      let bs = MV.unsafeDrop 1 new
      goDatch doc parent Nothing (Just b)
      MV.unsafeNew 0 >>= \empty -> diffChildren doc parent empty bs
    (False, False) -> do
      a <- MV.unsafeRead current 0
      let as = MV.unsafeDrop 1 current
      b <- MV.unsafeRead new 0
      let bs = MV.unsafeDrop 1 new
      goDatch doc parent (Just a) (Just b)
      diffChildren doc parent as bs
    (True, True) -> pure ()

getHeadAndTail :: MV.IOVector (VTree a) -> IO (VTree a, MV.IOVector (VTree a))
getHeadAndTail xs = do
  h <- MV.unsafeRead xs 0
  pure (h, MV.unsafeDrop 1 xs)

type Events = Proxy [(Symbol, Bool)]

initialize
  :: forall action events . ExtractEvents events
  => SignalGen (FRP.Signal (Sample (View action)))
  -> (action -> IO ())
  -> Proxy events
  -> SignalGen (FRP.Signal ())
initialize gen writer events = do
  sig <- gen
  Changed initialTree <- snapshot sig
  vtreeRef <- execute $ do
    iTree <- runView initialTree
    initTree iTree
    vtreeRef <- newIORef iTree
    void $ forkIO $ delegator writer vtreeRef events
    pure vtreeRef
  flip effectful1 sig $ \tree -> do
    void $ waitForAnimationFrame
    case tree of
      NotChanged _ -> pure ()
      Changed newTree -> do
        nT <- runView newTree
        currentTree <- readIORef vtreeRef
        Just currentTree `datch` Just nT
        writeIORef vtreeRef nT

runSignal :: forall e action . ExtractEvents e
          => Proxy e
          -> (action -> IO ())
          -> FRP.SignalGen (FRP.Signal (Sample (View action)))
          -> IO ()
runSignal events writer gen =
 forever =<< start (initialize gen writer events)

signal :: IO (Signal [a], a -> IO ())
signal = externalMulti

data Settings (events :: [(Symbol,Bool)]) (stepConfig :: [k]) (action :: *) =
   Settings { events :: Proxy events
            , stepConfig :: Proxy stepConfig
            , extraSignals :: [ Signal [ action ] ]
            }

defaultSettings :: Settings DefaultEvents DefaultStepConfig action
defaultSettings = Settings defaultEvents defaultStepConfig []

startApp
  :: (HasAction action model stepConfig, ExtractEvents events, Eq model)
  => model
  -> (model -> View action)
  -> (action -> model -> Effect action model)
  -> Settings (events :: [(Symbol, Bool)]) stepConfig action
  -> IO ()
startApp model view update Settings{..} = do
  (sig, send) <- signal
  runSignal events send $ do
    fmap (fmap (fmap view)) <$> foldp stepConfig update model $
      mergeManyActions (sig : extraSignals)

mergeActions :: Signal [action] -> Signal [action] -> Signal [action]
mergeActions x y = do
  signalX <- x
  signalY <- y
  pure $ (++) <$> signalX <*> signalY

mergeManyActions :: [ Signal [action] ] -> Signal [action]
mergeManyActions = foldl1 mergeActions

getFromStorage
  :: FromJSON model
  => T.Text
  -> IO (Either String model)
getFromStorage key = do
  Just w <- currentWindow
  Just s <- getLocalStorage w
  maybeVal <- S.getItem s key
  pure $ case maybeVal of
    Nothing -> Left "Not found"
    Just m -> eitherDecode (CS.cs (m :: T.Text))

data DebugModel
data DebugActions
data SaveToLocalStorage (key :: Symbol)
data SaveToSessionStorage (key :: Symbol)

type DefaultStepConfig = '[]

defaultStepConfig :: Proxy DefaultStepConfig
defaultStepConfig = Proxy

instance Show model => ToAction actions model DebugModel where
  toAction _ _ m = print m

instance Show actions => ToAction actions model DebugActions where
  toAction _ as _ = print as

instance (ToJSON model, KnownSymbol sym, Show model) =>
  ToAction actions model (SaveToSessionStorage sym) where
  toAction _ _ m = do
    let key = T.pack $ symbolVal (Proxy :: Proxy sym)
    Just w <- currentWindow
    Just s <- getSessionStorage w
    S.setItem s (textToJSString key) (CS.cs (encode m) :: T.Text)

instance ( ToJSON model, KnownSymbol sym, Show model )
  => ToAction actions model (SaveToLocalStorage sym) where
  toAction _ _ m = do
    let key = T.pack $ symbolVal (Proxy :: Proxy sym)
    Just w <- currentWindow
    Just s <- getLocalStorage w
    S.setItem s (textToJSString key) (CS.cs (encode m) :: T.Text)

instance HasAction model action '[] where
    performActions _ _ _ = pure ()

instance ( Nub (e ': es) ~ (e ': es)
         , HasAction action model es
         , ToAction action model e
         , Show model)
  => HasAction action model (e ': es) where
    performActions _ as m =
      toAction nextAction as m >>
        performActions nextActions as m
          where
            nextAction :: Proxy e; nextActions :: Proxy es
            nextActions = Proxy; nextAction = Proxy

class Nub effects ~ effects =>
  HasAction action model effects
    where
      performActions
        :: Proxy effects
        -> [action]
        -> model
        -> IO ()

class ToAction action model effect where
  toAction :: Proxy effect -> [action] -> model -> IO ()


foldp :: forall model action effects
      . ( HasAction action model effects, Eq model )
      => Proxy effects
      -> (action -> model -> Effect action model)
      -> model
      -> Signal [action]
      -> Signal (Sample model)
foldp p update ini signalGen = do
  isInitialDiff <- execute $ newIORef True
  actionSignal <- signalGen
  mfix $ \sig -> do
    modelSignal <- delay (NotChanged ini) sig
    effectful2 (handleUpdate isInitialDiff) actionSignal modelSignal
      where
        handleUpdate :: IORef Bool -> [action] -> Sample model -> IO (Sample model)
        handleUpdate isInitialDiff actions m = do
          goFold m update actions >>= \case
            NotChanged newModel -> do
              value <- readIORef isInitialDiff
              case value of
                True -> do
                  modifyIORef isInitialDiff (const False)
                  pure (Changed newModel)
                False -> pure (NotChanged newModel)
            Changed newModel -> do
              performActions p actions newModel
              pure $ Changed newModel

goFold
  :: Eq model
  => Sample model
  -> (action -> model -> Effect action model)
  -> [action]
  -> IO (Sample model)
goFold m _ [] = pure $ NotChanged (fromChanged m)
goFold initial update as = go initial as
  where
    go = F.foldrM f
    f action model =
      case update action (fromChanged model) of
        NoEffect m -> do
          pure $ case m == fromChanged initial of
            True -> NotChanged m
            False -> Changed m
        Effect m eff -> do
          newAction <- eff
          go (NotChanged m) [newAction]

type DefaultEvents = '[
    '("blur", 'True)
  , '("change", 'False)
  , '("click", 'False)
  , '("dblclick", 'False)
  , '("focus", 'False)
  , '("input", 'False)
  , '("keydown", 'False)
  , '("keypress", 'False)
  , '("keyup", 'False)
  , '("mouseup", 'False)
  , '("mousedown", 'False)
  , '("mouseenter", 'False)
  , '("mouseleave", 'False)
  , '("mouseover", 'False)
  , '("mouseout", 'False)
  , '("dragstart", 'False)
  , '("dragover", 'False)
  , '("dragend", 'False)
  , '("dragenter", 'False)
  , '("dragleave", 'False)
  , '("drag", 'False)
  , '("drop", 'False)
  , '("submit", 'False)
  ]

defaultEvents :: Proxy DefaultEvents
defaultEvents = Proxy

-- | Remove duplicates from a type-level list.
type family Nub xs where
  Nub '[] = '[]
  Nub (x ': xs) = x ': Nub (Remove x xs)

-- | Remove element from a type-level list.
type family Remove x xs where
  Remove x '[]       = '[]
  Remove x (x ': ys) =      Remove x ys
  Remove x (y ': ys) = y ': Remove x ys

-- swapKids
--   :: Document
--   -> Node
--   -> M.Map Key (VTree action)
--   -> V.Vector  (VTree action)
--   -> M.Map Key (VTree action)
--   -> V.Vector  (VTree action)
--   -> IO ()
-- swapKids doc p currentMap cs newMap ns = do
--   case (V.null cs, V.null ns) of
--     (True, True) -> pure () -- | None, skip
--     (False, True) -> pure () -- | No nodes left, remove all remaining
-- --      goDatch doc p (Just (V.head cs)) Nothing
-- --      swapKids doc p currentMap (V.tail cs) newMap V.empty
--     (True, False) -> pure ()
-- --      goDatch doc p Nothing $ Just (V.head ns)
-- --      swapKids doc p currentMap V.empty newMap (V.tail ns)
--     (False, False) -> do
--       let (c, css) = (V.head &&& V.tail) cs
--           (n, nss) = (V.head &&& V.tail) ns
--       case getKeyUnsafe c == getKeyUnsafe n of
--         True -> -- Keys same, continue
--           swapKids doc p currentMap css newMap nss
--         False -> do -- Keys not the same, check if current node has been moved or deleted
--           case M.lookup (getKeyUnsafe c) newMap of
--             -- Current node has been deleted, remove from DOM
--             Nothing -> do
--               let VNode _ _ _ _ _ ref _ = c
--               void $ removeChild p =<< readIORef ref
--               swapKids doc p currentMap css newMap ns
--             -- Current node exists, but does new node exist in current map?
--             Just _ -> do
--               let VNode _ _ _ _ _ currentRef _ = c
--                   VNode ntyp _ _ _ _ newRef _  = n
--               case M.lookup (getKeyUnsafe n) currentMap of
--                 -- New node, doesn't exist in current map, create new node and insertBefore
--                 Nothing -> do
--                   Just newNode <- fmap castToNode <$> createElement doc (Just ntyp)
--                   writeIORef newRef (Just newNode)
--                   void $ insertBefore p (Just newNode) =<< readIORef currentRef
--                   swapKids doc p currentMap cs newMap nss
--                  -- Node has moved, use insertBefore on moved node
--                 Just foundNode -> do
--                   let VNode _ _ _ _ _ movedNode _ = foundNode
--                   mNode <- readIORef movedNode
--                   cNode <- readIORef currentRef
--                   void $ insertBefore p cNode mNode
--                   writeIORef currentRef mNode
--                   writeIORef movedNode cNode
--                   swapKids doc p currentMap css newMap ns
