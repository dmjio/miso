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
import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.Free.Church
import           Control.Monad.Free.TH
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
import           Data.String.Conversions
import qualified Data.Text                     as T
import qualified Data.Vector                   as V
import qualified FRP.Elerea.Simple             as FRP
import           FRP.Elerea.Simple             hiding (Signal)
import           GHC.Generics
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
import           GHCJS.DOM.Types               hiding (Event, Attr)
import           GHCJS.DOM.Window              (getLocalStorage, getSessionStorage)
import           GHCJS.Foreign                 hiding (Object, Number)
import qualified GHCJS.Foreign.Internal        as Foreign
import           GHCJS.Marshal
import           GHCJS.Marshal.Pure
import qualified GHCJS.Types                   as G
import           JavaScript.Object.Internal
import           JavaScript.Web.AnimationFrame
import           Prelude                       hiding (repeat)

type Signal a = SignalGen (FRP.Signal a)

data Sample a = Changed a | NotChanged a
  deriving (Functor, Show)

fromChanged :: Sample a -> a
fromChanged (Changed x) = x
fromChanged (NotChanged x) = x

data Action object a where
  GetTarget :: (object -> a) -> Action object a
  GetParent :: object -> (object -> a) -> Action object a
  GetField  :: FromJSON v => T.Text -> object -> (Maybe v -> a) -> Action object a
  GetEventField  :: FromJSON v => T.Text -> (Maybe v -> a) -> Action object a
  GetChildren ::  object -> (object -> a) -> Action object a
  GetItem :: object -> Int -> (Maybe object -> a) -> Action object a
  GetNextSibling :: object -> (Maybe object -> a) -> Action object a
  SetEventField :: ToJSON v => T.Text -> v -> a -> Action object a

$(makeFreeCon 'GetTarget)
$(makeFreeCon 'GetParent)
$(makeFreeCon 'GetField)
$(makeFreeCon 'GetEventField)
$(makeFreeCon 'SetEventField)
$(makeFreeCon 'GetChildren)
$(makeFreeCon 'GetItem)
$(makeFreeCon 'GetNextSibling)

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

newtype View action =
  View { runView :: forall node . IO (VTreeBase node action) }

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

deriving instance Functor (Action object)

type Grammar obj a = F (Action obj) a

class HasEvent (eventName :: Symbol) returnType where
  parseEvent :: Proxy eventName -> Grammar obj returnType

on :: (KnownSymbol eventName, HasEvent eventName returnType)
   => Proxy eventName
   -> (returnType -> action)
   -> Attribute action
on = onWithOptions defaultOptions

onWithOptions :: (KnownSymbol eventName, HasEvent eventName returnType)
   => Options
   -> Proxy eventName
   -> (returnType -> action)
   -> Attribute action
onWithOptions options proxy f =
  E $ EventHandler options (symbolVal proxy) proxy f

data Options = Options {
    stopPropagation :: !Bool
  , preventDefault :: !Bool
  } deriving (Show, Eq)

defaultOptions :: Options
defaultOptions = Options False False

data EventHandler action
  = forall eventName returnType . HasEvent eventName returnType
  => EventHandler Options String (Proxy eventName) (returnType -> action)

data Prop = Prop T.Text Value
  deriving (Eq, Show)

data Attr = Attr T.Text T.Text
  deriving (Eq, Show)

instance Show (EventHandler action) where
  show (EventHandler _ name _ _) = "<event=" <> name <> ">"

type VTree action = VTreeBase Node action

getKey :: VTreeBase action a -> Maybe Key
getKey (VNode _ _ _ _ maybeKey _ _) = maybeKey
getKey _ = Nothing

getKeyUnsafe :: VTreeBase action a -> Key
getKeyUnsafe (VNode _ _ _ _ (Just key) _ _) = key
getKeyUnsafe _ = Prelude.error "Key does not exist"

data Attribute action =
    E (EventHandler action)
  | A Attr
  | P Prop

data VTreeBase node action where
  VNode :: T.Text
        -> V.Vector (EventHandler action)
        -> V.Vector Attr
        -> V.Vector Prop
        -> Maybe Key
        -> IORef (Maybe node)
        -> V.Vector (VTreeBase node action)
        -> VTreeBase node action
  VText :: T.Text
        -> IORef (Maybe node)
        -> VTreeBase node action

instance ToJSVal Key
instance FromJSVal Key

newtype Key = Key T.Text deriving (Show, Eq, Ord, Generic)

instance Show (VTreeBase action e) where
  show (VText val _ ) = T.unpack val
  show (VNode typ _ _ evts children _ _) =
    "<" ++ T.unpack typ ++ ">" ++ show evts ++
      show children ++ "\n" ++ "</" ++ T.unpack typ ++ ">"

mkNode :: T.Text -> [Attribute action] -> [View action] -> View action
mkNode name as xs = mkNodeKeyed name Nothing as xs

mkNodeKeyed :: T.Text -> Maybe Key -> [Attribute action] -> [View action] -> View action
mkNodeKeyed name key as xs = View $ do
  ref <- newIORef Nothing
  kids <- traverse runView xs
  pure $ VNode name evts atts props key ref (V.fromList kids)
    where
      atts = V.fromList [ a | A a@(Attr _ _) <- as ]
      props = V.fromList [ p | P p@(Prop _ _) <- as ]
      evts = V.fromList [ e | E e@EventHandler{} <- as ]

text_ :: T.Text -> View action
text_ txt = View $ do
  ref <- newIORef Nothing
  pure $ VText txt ref

div_ :: [Attribute action] -> [View action] -> View action
div_  = mkNode "div"

table_ :: [Attribute action] -> [View action] -> View action
table_  = mkNode "table"

thead_ :: [Attribute action] -> [View action] -> View action
thead_  = mkNode "thead"

tbody_ :: [Attribute action] -> [View action] -> View action
tbody_  = mkNode "tbody"

tr_ :: [Attribute action] -> [View action] -> View action
tr_  = mkNode "tr"

th_ :: [Attribute action] -> [View action] -> View action
th_  = mkNode "th"

td_ :: [Attribute action] -> [View action] -> View action
td_  = mkNode "td"

tfoot_ :: [Attribute action] -> [View action] -> View action
tfoot_  = mkNode "tfoot"

section_ :: [Attribute action] -> [View action] -> View action
section_  = mkNode "section"

header_ :: [Attribute action] -> [View action] -> View action
header_  = mkNode "header"

footer_ :: [Attribute action] -> [View action] -> View action
footer_  = mkNode "footer"

btn_ :: [Attribute action] -> [View action] -> View action
btn_ = mkNode "button"

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
        eventType :: String <- E.getType e
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
  -> String -> [Node] -> IO ()
delegateEvent e writer (VNode _ _ _ _ _ _ children) eventName =
  void . flip execStateT [] . findEvent children
    where
      findEvent _ [] = pure ()
      findEvent childNodes [y] = do
       stopPropagate <- do
         result <- liftIO $ findNode childNodes y
         case result of
           Nothing -> pure False
           Just (VText _ _) -> pure False
           Just (VNode _ evts _ _ _ _ _) ->
             case getEventHandler evts of
               Nothing -> pure False
               Just (EventHandler options _ prox action) -> do
                 when (preventDefault options) $ liftIO (E.preventDefault e)
                 liftIO $ runEvent e writer prox action
                 pure $ stopPropagation options
       get >>= propagateWhileAble stopPropagate

      findEvent childNodes (y:ys) = do
        result <- liftIO $ findNode childNodes y
        forM_ result $ \parent@(VNode _ _ _ _ _ _ childrenNext) -> do
          modify (parent:)
          findEvent childrenNext ys

      propagateWhileAble _ [] = pure ()
      propagateWhileAble True _ = pure ()
      propagateWhileAble False ((VNode _ evts _ _ _ _ _):xs) =
        case getEventHandler evts of
           Nothing -> do
             propagateWhileAble False xs
           Just (EventHandler options _ prox action) -> do
             liftIO $ runEvent e writer prox action
             when (preventDefault options) $ liftIO (E.preventDefault e)
             propagateWhileAble (stopPropagation options) xs
      propagateWhileAble _ _ = Prelude.error "never called"

      findNode :: V.Vector (VTree a) -> Node -> IO (Maybe (VTree a))
      findNode childNodes targetNode = do
          vec <- flip V.filterM childNodes $
             \vnode ->
               case isVNode vnode of
                 True -> do
                   let VNode _ _ _ _ _ ref _ = vnode
                   Just val <- liftIO $ readIORef ref
                   pure $ val == targetNode
                 False -> pure False
          pure $ (V.!?) vec 0

      getEventHandler =
        V.find (\(EventHandler _ evtName _ _) -> evtName == eventName)
delegateEvent _ _ _ _ = const $ pure ()

isVNode :: VTree a -> Bool
isVNode (VText _ _) = False
isVNode _ = True

initTree :: VTree action -> IO ()
initTree initial = datch Nothing (Just initial)

-- copies body first child into vtree, to avoid flickering
-- copyDOMIntoView :: Node -> View action -> IO (View action)
-- copyDOMIntoView _ VEmpty = pure VEmpty -- should never get called
-- copyDOMIntoView node (VText s _) = pure $ VText s (Just node)
-- copyDOMIntoView node (VNode name attrs key _ children) = do
--   xs <- forM (zip [0 :: Int ..] children) $ \(index, childNode) -> do
--           Just childNodes <- getChildNodes node
--           Just child <- item childNodes (fromIntegral index)
--           copyDOMIntoView child childNode
--   pure $ VNode name attrs key (Just node) xs

datch :: Maybe (VTree action) -> Maybe (VTree action) -> IO ()
datch currentTree newTree = do
  Just document <- currentDocument
  Just body <- fmap toNode <$> getBody document
  goDatch document body currentTree newTree

-- Make a new text node
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
goDatch doc parentNode Nothing (Just (VNode typ _ attrs props  _ ref children)) = do
  node@(Just newNode) <- fmap toNode <$> createElement doc (Just typ)
  writeIORef ref node
  diffAttrs newNode V.empty attrs
  diffProps newNode V.empty props
  forM_ children $ \childNode ->
    goDatch doc newNode Nothing (Just childNode)
  void $ appendChild parentNode node

-- Remove an element
goDatch _ parentNode (Just (VNode _ _  _ _ _ ref _)) Nothing =
  void $ removeChild parentNode =<< readIORef ref

-- Replace an element with a text node
goDatch doc parentNode (Just (VNode _ _  _ _ _ currentRef _)) (Just (VText str newRef)) = do
  currentNode <- readIORef currentRef
  newTextNode <- fmap toNode <$> createTextNode doc str
  writeIORef newRef (castToNode <$> newTextNode)
  void $ replaceChild parentNode newTextNode currentNode

-- Replace a text node with an Element
goDatch doc parentNode (Just (VText _ currentRef)) (Just (VNode typ _ attrs props _ newRef children)) = do
  node@(Just newNode) <- fmap toNode <$> createElement doc (Just typ)
  diffAttrs newNode V.empty attrs
  diffProps newNode V.empty props
  writeIORef newRef node
  forM_ children $ \childNode ->
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
goDatch doc parent
  (Just (VNode typA _ attrsA propsA _ currentRef childrenA))
  (Just (VNode typB _ attrsB propsB _ newRef childrenB)) = do
 case typA == typB of
   True -> do
      Just currentNode <- readIORef currentRef
      writeIORef newRef $ Just currentNode
      diffAttrs currentNode attrsA attrsB
      diffProps currentNode propsA propsB
      diffChildren doc currentNode childrenA childrenB
   False -> do
      node@(Just newNode) <- fmap toNode <$> createElement doc (Just typB)
      writeIORef newRef node
      void $ diffAttrs newNode V.empty attrsB
      void $ diffProps newNode V.empty propsB
      forM_ childrenB $ \childNode ->
        goDatch doc newNode Nothing (Just childNode)
      void $ replaceChild parent node =<< readIORef currentRef

-- instance L.ToHtml (View action a) where
--   toHtmlRaw = L.toHtml
--   toHtml VEmpty = Prelude.error "VEmpty for internal use only"
--   toHtml (VText x _) = L.toHtml x
--   toHtml (VNode typ attrs _ _ children) =
--     let ele = L.makeElement (toTag typ) (foldMap L.toHtml children)
--     in L.with ele as
--       where
--         as = [ L.makeAttribute k v | Attr k v <- attrs ]
--         toTag = T.toLower

difference :: Eq a => V.Vector a -> V.Vector a -> V.Vector a
difference xs ys = V.filter (\x -> x `V.notElem` ys) xs

diffAttrs
  :: Node
  -> V.Vector Attr
  -> V.Vector Attr
  -> IO ()
diffAttrs node current new = do
  let el = castToElement node
  V.forM_ (current `difference` new) $ \(Attr k _) -> do
    removeAttribute el k
  V.forM_ (new `difference` current) $ \(Attr k v) -> do
    setAttribute el k v

diffProps
  :: Node
  -> V.Vector Prop
  -> V.Vector Prop
  -> IO ()
diffProps node current new = do
  obj <- Object <$> toJSVal node
  let el = castToElement node
  V.forM_ (current `difference` new) $ \(Prop k _) -> do
    setProp (textToJSString k) jsNull obj

  V.forM_ (flip V.filter new $ \(Prop k _) ->
     k `V.notElem` (V.map (\(Prop k' _) -> k') current)) $ \(Prop k1 v1) ->
       do val <- toJSVal v1
          setProp (textToJSString k1) val obj

  V.forM_ new $ \(Prop newKey newVal) ->
    V.forM_ current $ \(Prop currentKey currentVal) -> do
       case newKey == currentKey of
         False -> pure ()
         True ->
           when (newVal /= currentVal) $ do
             val <- toJSVal newVal
             setProp (textToJSString newKey) val obj
             dispatchObservable newKey el


observables :: M.Map T.Text (Element -> IO ())
observables = M.fromList [("autofocus", focus)]

dispatchObservable :: T.Text -> Element -> IO ()
dispatchObservable key el = do
  F.forM_ (M.lookup key observables) $ \f -> f el

-- isKeyed :: [VTree action] -> Bool
-- isKeyed [] = False
-- isKeyed (x : _) = hasKey x
--   where
--     hasKey :: VTree action -> Bool
--     hasKey (VNode _ _ (Just _) _ _) = True
--     hasKey _ = False

-- makeMap :: [VTree action] -> M.Map Key (VTree action)
-- makeMap vs = M.fromList [ (key, v) | v@(VNode _ _ _ _ (Just key) _ _) <- vs ]

diffChildren
  :: Document
  -> Node
  -> V.Vector (VTree action)
  -> V.Vector (VTree action)
  -> IO ()
diffChildren doc parent as bs = do
  diffChildren' doc parent as bs
  -- case isKeyed as of
  --   True -> do
  --     swappedKids <- swapKids parent (makeMap as) as (makeMap bs) bs
  --     diffChildren' doc parent swappedKids bs
  --   False -> diffChildren' doc parent as bs

diffChildren'
  :: Document
  -> Node
  -> V.Vector (VTree action)
  -> V.Vector (VTree action)
  -> IO ()
diffChildren' doc parent current new =
  case (V.null current, V.null new) of
    (True, True) -> pure ()
    (True, False) -> do
      let b = V.head new
          bs = V.tail new
      goDatch doc parent Nothing (Just b)
      diffChildren doc parent V.empty bs
    (False, True) -> do
      let a = V.head current
          as = V.tail current
      goDatch doc parent (Just a) Nothing
      diffChildren doc parent as V.empty
    (False, False) -> do
      let a = V.head current
          as = V.tail current
          b = V.head new
          bs = V.tail new
      goDatch doc parent (Just a) (Just b)
      diffChildren doc parent as bs

type Events = Proxy [(Symbol, Bool)]

runSignal :: forall e action . ExtractEvents e
          => Proxy e
          -> (action -> IO ())
          -> Signal (Sample (View action))
          -> IO ()
runSignal events writer s = do
  emitter <- start s
  Changed initialTree <- emitter
  iTree <- runView initialTree
  initTree iTree
  vtreeRef <- newIORef iTree
  _ <- forkIO $ delegator writer vtreeRef events
  forever $
    waitForAnimationFrame >>
      emitter >>= \case
        NotChanged _ -> pure ()
        Changed newTree -> do
          nT <- runView newTree
          currentTree <- readIORef vtreeRef
          Just currentTree `datch` Just nT
          writeIORef vtreeRef nT

-- Default action, should be identity (mempty) w/o binary associative operator
signal :: IO (Signal [a], a -> IO ())
signal = externalMulti

startApp
  :: (HasAction action model stepConfig, ExtractEvents events, Eq model)
  => model
  -> (model -> View action)
  -> (action -> model -> Effect action model)
  -> Proxy events
  -> Proxy stepConfig
  -> [ Signal [action] ]
  -> IO ()
startApp model view update events stepConfig signals = do
  (sig, send) <- signal
  runSignal events send $ do
    fmap (fmap (fmap view)) <$> foldp stepConfig update model $
      mergeManyActions (sig : signals)

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
    Just m -> eitherDecode (cs (m :: T.Text))

data DebugModel
data DebugActions
data SaveToLocalStorage (key :: Symbol)
data SaveToSessionStorage (key :: Symbol)

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
    S.setItem s (textToJSString key) (cs (encode m) :: T.Text)

instance ( ToJSON model, KnownSymbol sym, Show model )
  => ToAction actions model (SaveToLocalStorage sym) where
  toAction _ _ m = do
    let key = T.pack $ symbolVal (Proxy :: Proxy sym)
    Just w <- currentWindow
    Just s <- getLocalStorage w
    S.setItem s (textToJSString key) (cs (encode m) :: T.Text)

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

attr :: T.Text -> T.Text -> Attribute action
attr k v = A (Attr k v)

prop :: ToJSON a => T.Text -> a -> Attribute action
prop k v = P $ Prop k (toJSON v)

boolProp :: T.Text -> Bool -> Attribute action
boolProp = prop

stringProp :: T.Text -> T.Text -> Attribute action
stringProp = prop

textProp :: T.Text -> T.Text -> Attribute action
textProp = prop

intProp :: T.Text -> Int -> Attribute action
intProp = prop

integerProp :: T.Text -> Integer -> Attribute action
integerProp = prop

doubleProp :: T.Text -> Double -> Attribute action
doubleProp = prop

checked_ :: Bool -> Attribute action
checked_ = boolProp "checked"

form_ :: [Attribute action] -> [View action] -> View action
form_ = mkNode "form"

p_ :: [Attribute action] -> [View action] -> View action
p_ = mkNode "p"

s_ :: [Attribute action] -> [View action] -> View action
s_ = mkNode "s"

ul_ :: [Attribute action] -> [View action] -> View action
ul_ = mkNode "ul"

span_ :: [Attribute action] -> [View action] -> View action
span_ = mkNode "span"

strong_ :: [Attribute action] -> [View action] -> View action
strong_ = mkNode "strong"

li_ :: [Attribute action] -> [View action] -> View action
li_ = mkNode "li"

liKeyed_ :: Key -> [Attribute action] -> [View action] -> View action
liKeyed_ = mkNodeKeyed "li" . pure

h1_ :: [Attribute action] -> [View action] -> View action
h1_ = mkNode "h1"

input_ :: [Attribute action] -> [View action] -> View action
input_ = mkNode "input"

label_ :: [Attribute action] -> [View action] -> View action
label_ = mkNode "label"

a_ :: [Attribute action] -> [View action] -> View action
a_ = mkNode "a"

styleRaw_ :: T.Text -> Attribute action
styleRaw_ = attr "style"

style_ :: M.Map T.Text T.Text -> Attribute action
style_ = A . Attr "style" . M.foldrWithKey go mempty
  where
    go :: T.Text -> T.Text -> T.Text -> T.Text
    go k v xs = T.concat [ k, ":", v, ";" ] <> xs

type_ :: T.Text -> Attribute action
type_ = attr "type"

name_ :: T.Text -> Attribute action
name_ = attr "name"

href_ :: T.Text -> Attribute action
href_ = attr "href"

className_ :: T.Text -> Attribute action
className_ = stringProp "className"

class_ :: T.Text -> Attribute action
class_ = attr "class"

id_ :: T.Text -> Attribute action
id_ = attr "id"

placeholder :: T.Text -> Attribute action
placeholder = attr "placeholder"

autofocus :: Bool -> Attribute action
autofocus = boolProp "autofocus"

template :: View action
template = div_  [] []

-- | (EventName, Capture)
defaultEvents :: Proxy '[
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
defaultEvents = Proxy

instance HasEvent "blur" () where parseEvent _ = pure ()
instance HasEvent "change" Bool where parseEvent _ = checkedGrammar
instance HasEvent "click" () where parseEvent _ = pure ()
instance HasEvent "dblclick" () where parseEvent _ = pure ()
instance HasEvent "focus" () where parseEvent _ = pure ()
instance HasEvent "input" T.Text where parseEvent _ = inputGrammar
instance HasEvent "keydown" Int where parseEvent _ = keyGrammar
instance HasEvent "keypress" Int where parseEvent _ = keyGrammar
instance HasEvent "keyup" Int where parseEvent _ = keyGrammar
instance HasEvent "mouseup" () where parseEvent _ = pure ()
instance HasEvent "mousedown" () where parseEvent _ = pure ()
instance HasEvent "mouseenter" () where parseEvent _ = pure ()
instance HasEvent "mouseleave" () where parseEvent _ = pure ()
instance HasEvent "mouseover" () where parseEvent _ = pure ()
instance HasEvent "mouseout" () where parseEvent _ = pure ()
instance HasEvent "dragstart" () where parseEvent _ = pure ()
instance HasEvent "dragover" () where parseEvent _ = pure ()
instance HasEvent "dragend" () where parseEvent _ = pure ()
instance HasEvent "dragenter" () where parseEvent _ = pure ()
instance HasEvent "dragleave" () where parseEvent _ = pure ()
instance HasEvent "drag" () where parseEvent _ = pure ()
instance HasEvent "drop" () where parseEvent _ = pure ()
instance HasEvent "submit" () where parseEvent _ = pure ()

newtype PreventDefault = PreventDefault Bool deriving (Show, Eq)

onBlur :: action -> Attribute action
onBlur action = on (Proxy :: Proxy "blur") $ \() -> action

onChecked :: (Bool -> action) -> Attribute action
onChecked = on (Proxy :: Proxy "change")

onClick :: action -> Attribute action
onClick action = on (Proxy :: Proxy "click") $ \() -> action

onFocus :: action -> Attribute action
onFocus action = on (Proxy :: Proxy "focus") $ \() -> action

onDoubleClick :: action -> Attribute action
onDoubleClick action = on (Proxy :: Proxy "dblclick") $ \() -> action

onInput :: (T.Text -> action) -> Attribute action
onInput = on (Proxy :: Proxy "input")

onKeyDown :: (Int -> action) -> Attribute action
onKeyDown = on (Proxy :: Proxy "keydown")

onKeyPress :: (Int -> action) -> Attribute action
onKeyPress = on (Proxy :: Proxy "keypress")

onKeyUp :: (Int -> action) -> Attribute action
onKeyUp = on (Proxy :: Proxy "keyup")

onMouseUp :: action -> Attribute action
onMouseUp action = on (Proxy :: Proxy "mouseup") $ \() -> action

onMouseDown :: action -> Attribute action
onMouseDown action = on (Proxy :: Proxy "mousedown") $ \() -> action

onMouseEnter :: action -> Attribute action
onMouseEnter action = on (Proxy :: Proxy "mouseenter") $ \() -> action

onMouseLeave :: action -> Attribute action
onMouseLeave action = on (Proxy :: Proxy "mouseleave") $ \() -> action

onMouseOver :: action -> Attribute action
onMouseOver action = on (Proxy :: Proxy "mouseover") $ \() -> action

onMouseOut :: action -> Attribute action
onMouseOut action = on (Proxy :: Proxy "mouseout") $ \() -> action

onDragStart :: action -> Attribute action
onDragStart action = on (Proxy :: Proxy "dragstart") $ \() -> action

onDragOver :: PreventDefault -> action -> Attribute action
onDragOver (PreventDefault prevent) action =
  onWithOptions defaultOptions{preventDefault = prevent}
    (Proxy :: Proxy "dragover") $ \() -> action

onDragEnd :: action -> Attribute action
onDragEnd action = on (Proxy :: Proxy "dragend") $ \() -> action

onDragEnter :: action -> Attribute action
onDragEnter action = on (Proxy :: Proxy "dragenter") $ \() -> action

onDragLeave :: action -> Attribute action
onDragLeave action = on (Proxy :: Proxy "dragleave") $ \() -> action

onDrag :: action -> Attribute action
onDrag action = on (Proxy :: Proxy "drag") $ \() -> action

onDrop :: PreventDefault -> action -> Attribute action
onDrop (PreventDefault prevent) action =
  onWithOptions defaultOptions{preventDefault = prevent}
    (Proxy :: Proxy "drop") $ \() -> action

onSubmit :: action -> Attribute action
onSubmit action =
  onWithOptions defaultOptions{preventDefault = True}
    (Proxy :: Proxy "submit") $ \() -> action

inputGrammar :: FromJSON a => Grammar obj a
inputGrammar = do
  target <- getTarget
  result <- getField "value" target
  case result of
    Nothing -> Prelude.error "Couldn't retrieve target input value"
    Just value -> pure value

checkedGrammar :: FromJSON a => Grammar obj a
checkedGrammar = do
  target <- getTarget
  result <- getField "checked" target
  case result of
    Nothing -> Prelude.error "Couldn't retrieve target checked value"
    Just value -> pure value

keyGrammar :: Grammar obj Int
keyGrammar = do
  keyCode <- getEventField "keyCode"
  which <- getEventField "which"
  charCode <- getEventField "charCode"
  pure $ head $ catMaybes [ keyCode, which, charCode ]

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
--   :: Node
--   -> M.Map Key (VTree action)
--   -> [ VTree action ]
--   -> M.Map Key (VTree action)
--   -> [ VTree action ]
--   -> IO [ VTree action ]
-- swapKids _ _ [] _ [] = pure []

-- -- | No nodes left, remove all remaining
-- swapKids p currentMap (c:ccs) newMap [] = do
--   let VNode _ _ _ currentNode _ = c
--   void $ removeChild p currentNode
--   swapKids p currentMap ccs newMap []

-- -- | Add remaining new nodes
-- swapKids p currentMap [] newMap (new:nns) = do
--   newNode <- renderNode p new
--   ts <- swapKids p currentMap [] newMap nns
--   pure $ newNode : ts

-- swapKids p currentMap (c:ccs) newMap (new:nns) = do
--   case getKey c == getKey new of
--     -- Keys same, continue
--     True -> do
--       ts <- swapKids p currentMap ccs newMap nns
--       pure (c:ts)
--     -- Keys not the same, check if current node has been moved or deleted
--     False -> do
--       case M.lookup (getKeyUnsafe c) newMap of
--         -- Current node has been deleted, remove from DOM
--         Nothing -> do
--           let VNode _ _ _ node _ = c
--           void $ removeChild p node
--           swapKids p currentMap ccs newMap (new:nns)
--         -- Current node exists, but does new node exist in current map?
--         Just _ -> do
--           let VNode _ _ _ currentNode _ = c
--           case M.lookup (getKeyUnsafe new) currentMap of
--             -- New node, doesn't exist in current map, create new node and insertBefore
--             Nothing -> do
--               newNode@(VNode _ _ _ node _) <- renderDontAppend new
--               void $ insertBefore p node currentNode
--               ts <- swapKids p currentMap (c:ccs) newMap nns
--               pure $ newNode : ts
--             -- Node has moved, use insertBefore on moved node
--             Just n -> do
--               let VNode _ _ _ movedNode _ = n
--               void $ insertBefore p currentNode movedNode
--               ts <- swapKids p currentMap ccs newMap nns
--               pure $ n : ts

renderNode :: Node -> VTree action -> IO ()
renderNode parent (VNode typ _ attrs _ _ ref children) = do
  Just doc <- currentDocument
  Just node <- fmap toNode <$> createElement doc (Just typ)
  writeIORef ref (Just node)
  void $ diffAttrs node V.empty attrs
  forM_ children $ \childNode ->
    goDatch doc node Nothing (Just childNode)
  void $ appendChild parent (Just node)

renderNode parent (VText str ref) = do
  Just doc <- currentDocument
  newTextNode <- createTextNode doc str
  writeIORef ref (castToNode <$> newTextNode)
  void $ appendChild parent newTextNode

renderDontAppend :: VTree action -> IO ()
renderDontAppend (VNode typ _ attrs _ _ ref children) = do
  Just doc <- currentDocument
  Just node <- fmap toNode <$> createElement doc (Just typ)
  writeIORef ref (Just node)
  void $ diffAttrs node V.empty attrs
  forM_ children $ \childNode ->
    goDatch doc node Nothing (Just childNode)
renderDontAppend (VText str ref) = do
  Just doc <- currentDocument
  newTextNode <- createTextNode doc str
  writeIORef ref (castToNode <$> newTextNode)


class ToKey key where toKey :: key -> Key
instance ToKey String  where toKey = Key . T.pack
instance ToKey T.Text  where toKey = Key
instance ToKey Int where toKey = Key . T.pack . show
instance ToKey Double where toKey = Key . T.pack . show
instance ToKey Float where toKey = Key . T.pack . show
instance ToKey Word where toKey = Key . T.pack . show
instance ToKey Key where toKey = id
