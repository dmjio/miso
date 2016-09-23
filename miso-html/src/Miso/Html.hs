{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE StandaloneDeriving    #-}
module Miso.Html where

import           Control.Monad.Free.Church
import           Control.Monad.Free.TH
import           Data.Aeson                (Value, FromJSON, ToJSON, toJSON)
import           Data.IORef
import qualified Data.Map                  as M
import           Data.Maybe
import           Data.Monoid
import           Data.Proxy
import qualified Data.Text                 as T
import qualified Data.Vector               as V
import qualified Data.Vector.Mutable       as MV
import           GHC.Generics
import           GHC.TypeLits
import qualified Lucid                     as L
import qualified Lucid.Base                as L
import           System.IO.Unsafe

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

newtype View action =
  View { runView :: forall node . IO (VTreeBase node action) }

instance Show (View action) where
  show (View xs) = show (unsafePerformIO xs)

data NS = HTML | SVG
  deriving (Show, Eq)

data Attribute action =
    E (T.Text, EventHandler action)
  | A (T.Text, T.Text)
  | C (T.Text, T.Text)
  | P (T.Text, Value)

newtype Events action = Events (M.Map T.Text (EventHandler action))
newtype Props = Props (M.Map T.Text Value)
newtype Attrs = Attrs (M.Map T.Text T.Text)
newtype CSS = CSS (M.Map T.Text T.Text)

data VTreeBase node action where
  VNode :: { vType :: T.Text
           , vNs :: NS
           , vEvents :: Events action
           , vProps :: Props
           , vAttrs :: Attrs
           , vCss :: CSS
           , vKey :: Maybe Key
           , vNode :: IORef (Maybe node)
           , vChildren :: MV.IOVector (VTreeBase node action)
           } -> VTreeBase node action
  VText :: { vText :: T.Text
           , vTextNode :: IORef (Maybe node)
           } -> VTreeBase node action

newtype Key = Key T.Text
  deriving (Show, Eq, Ord, Generic)

instance Show (VTreeBase action e) where
  show VText{..} = T.unpack vText
  show VNode{..} =
    "<" ++ T.unpack vType ++ ">" ++
      show (unsafePerformIO $ V.unsafeFreeze vChildren) ++ "</" ++ T.unpack vType ++ ">"

mkNode :: NS -> T.Text -> [Attribute action] -> [View action] -> View action
mkNode ns name as xs = mkNodeKeyed ns name Nothing as xs

mkNodeHtml :: T.Text -> [Attribute action] -> [View action] -> View action
mkNodeHtml = mkNode HTML

mkNodeSvg :: T.Text -> [Attribute action] -> [View action] -> View action
mkNodeSvg = mkNode SVG

mkNodeKeyed :: NS -> T.Text -> Maybe Key -> [Attribute action] -> [View action] -> View action
mkNodeKeyed vNs vType vKey as xs = View $ do
  vNode <- newIORef Nothing
  vChildren <- V.unsafeThaw =<< V.fromList <$> traverse runView xs
  let vEvents = Events $ M.fromList [ x | E x <- as ]
      vProps = Props $ M.fromList [ x | P x <- as ]
      vAttrs = Attrs $ M.fromList [ x | A x <- as ]
      vCss = CSS $ M.fromList [ x | C x <- as ]
  pure VNode {..}

text_ :: T.Text -> View action
text_ txt = View $ VText txt <$> newIORef Nothing

div_ :: [Attribute action] -> [View action] -> View action
div_  = mkNodeHtml "div"

table_ :: [Attribute action] -> [View action] -> View action
table_  = mkNodeHtml "table"

thead_ :: [Attribute action] -> [View action] -> View action
thead_  = mkNodeHtml "thead"

tbody_ :: [Attribute action] -> [View action] -> View action
tbody_  = mkNodeHtml "tbody"

tr_ :: [Attribute action] -> [View action] -> View action
tr_  = mkNodeHtml "tr"

th_ :: [Attribute action] -> [View action] -> View action
th_  = mkNodeHtml "th"

td_ :: [Attribute action] -> [View action] -> View action
td_  = mkNodeHtml "td"

tfoot_ :: [Attribute action] -> [View action] -> View action
tfoot_  = mkNodeHtml "tfoot"

section_ :: [Attribute action] -> [View action] -> View action
section_  = mkNodeHtml "section"

header_ :: [Attribute action] -> [View action] -> View action
header_  = mkNodeHtml "header"

footer_ :: [Attribute action] -> [View action] -> View action
footer_  = mkNodeHtml "footer"

btn_ :: [Attribute action] -> [View action] -> View action
btn_ = mkNodeHtml "button"

attr :: T.Text -> T.Text -> Attribute action
attr k v = A (k,v)

prop :: ToJSON a => T.Text -> a -> Attribute action
prop k v = P $ (k, toJSON v)

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
form_ = mkNodeHtml "form"

p_ :: [Attribute action] -> [View action] -> View action
p_ = mkNodeHtml "p"

s_ :: [Attribute action] -> [View action] -> View action
s_ = mkNodeHtml "s"

ul_ :: [Attribute action] -> [View action] -> View action
ul_ = mkNodeHtml "ul"

span_ :: [Attribute action] -> [View action] -> View action
span_ = mkNodeHtml "span"

strong_ :: [Attribute action] -> [View action] -> View action
strong_ = mkNodeHtml "strong"

li_ :: [Attribute action] -> [View action] -> View action
li_ = mkNodeHtml "li"

liKeyed_ :: Key -> [Attribute action] -> [View action] -> View action
liKeyed_ = mkNodeKeyed HTML "li" . pure

h1_ :: [Attribute action] -> [View action] -> View action
h1_ = mkNodeHtml "h1"

input_ :: [Attribute action] -> [View action] -> View action
input_ = mkNodeHtml "input"

label_ :: [Attribute action] -> [View action] -> View action
label_ = mkNodeHtml "label"

a_ :: [Attribute action] -> [View action] -> View action
a_ = mkNodeHtml "a"

styleRaw_ :: T.Text -> Attribute action
styleRaw_ = C . (,) "style"

style_ :: M.Map T.Text T.Text -> Attribute action
style_ = C . (,) "style" . M.foldrWithKey go mempty
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
  onWithOptions defaultOptions { preventDefault = prevent }
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

class    ToKey key    where toKey :: key -> Key
instance ToKey T.Text where toKey = Key
instance ToKey String where toKey = Key . T.pack
instance ToKey Int    where toKey = Key . T.pack . show
instance ToKey Double where toKey = Key . T.pack . show
instance ToKey Float  where toKey = Key . T.pack . show
instance ToKey Word   where toKey = Key . T.pack . show

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
 E (T.pack $ symbolVal proxy,  EventHandler options proxy f)

data Options = Options {
    stopPropagation :: !Bool
  , preventDefault :: !Bool
  } deriving (Show, Eq)

defaultOptions :: Options
defaultOptions = Options False False

data EventHandler action
  = forall eventName returnType . HasEvent eventName returnType
  => EventHandler Options (Proxy eventName) (returnType -> action)

getKey :: VTreeBase action a -> Maybe Key
getKey (VNode _ _ _ _ _ _ maybeKey _ _) = maybeKey
getKey _ = Nothing

getKeyUnsafe :: VTreeBase action a -> Key
getKeyUnsafe (VNode _ _ _ _ _ _ (Just key) _ _) = key
getKeyUnsafe _ = Prelude.error "Key does not exist"

instance L.ToHtml (View action) where
  toHtmlRaw = L.toHtml
  toHtml (View xs) = L.toHtml (unsafePerformIO xs)

instance L.ToHtml (VTreeBase node action) where
  toHtmlRaw = L.toHtml
  toHtml (VText x _) = L.toHtml x
  toHtml VNode{..} =
    let ele = L.makeElement (toTag vType) (foldMap L.toHtml $ unsafePerformIO $ V.unsafeFreeze vChildren)
      in L.with ele as
      where
        Attrs xs = vAttrs
        as = [ L.makeAttribute k v | (k,v) <- M.toList xs ]
        toTag = T.toLower
