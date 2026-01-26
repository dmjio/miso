{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <&>" #-}

module HtmlGen3 where

import GHC.Generics
import Data.Aeson (ToJSON, FromJSON)
import Miso hiding (on, Src, Checked)
import Miso.Html.Element hiding (title_, data_)
import Miso.Html.Property hiding (label_, form_)
import Test.QuickCheck
import Data.Char (isControl, isSpace)
import Control.Monad (replicateM)

maxDepth :: Int
maxDepth = 20

type HtmlAttributeValue = MisoString

type HtmlAttribute = (HtmlAttributeType, HtmlAttributeValue)

data HtmlAttributeType
    = Class
    | Id
    | Title
    | Colspan
    | Rowspan
    | Method
    | Action
    | Alt
    | Src
    | Value
    | Type
    | Checked
    deriving (Eq, Generic, ToJSON, FromJSON, Show)


data ChildHavingHtmlTag
    = Div
    | Span
    | P
    | Pre
    | Ul
    | Ol
    | Li
    | Section
    | Header
    | Footer
    | Nav
    | Article
    | H1
    | H2
    | H3
    | H4
    | Strong
    | Em
    | Table
    | Thead
    | Tbody
    | Tr
    | Td
    | Th
    | Form
    | Label
    | Button
    | Fieldset
    | Legend
    | Dl
    | Dt
    | Dd
    | Figure
    | Figcaption
    | A
    deriving (Eq, Enum, Bounded, Generic, ToJSON, FromJSON, Show)

instance Arbitrary ChildHavingHtmlTag where
  arbitrary = chooseBoundedEnum


data ChildlessHtmlTag
    = Hr
    | Br
    | Img
    | Input
    | Wbr
    deriving (Eq, Enum, Bounded, Generic, ToJSON, FromJSON, Show)

instance Arbitrary ChildlessHtmlTag where
  arbitrary = chooseBoundedEnum


data HTML
    = Elem ChildHavingHtmlTag [ HtmlAttribute ] [ HTML ]
    | VoidElem ChildlessHtmlTag [ HtmlAttribute ]
    | Text MisoString
    deriving (Eq, Generic, ToJSON, FromJSON, Show)

instance Arbitrary HTML where
  arbitrary = genHtml


type MkTag model action = [ Attribute action ] -> [ View model action ] -> View model action

type MkTag2 model action = [ Attribute action ] -> View model action

type Tag = Either ChildlessHtmlTag ChildHavingHtmlTag

type TagGen = Gen Tag


nextGenerator :: Tag -> TagGen
nextGenerator (Right Ul) = return $ Right Li
nextGenerator (Right Ol) = return $ Right Li
nextGenerator (Right Table) = Right <$> elements [ Thead, Tbody, Tr ]
nextGenerator (Right Thead) = return $ Right Tr
nextGenerator (Right Tbody) = return $ Right Tr
nextGenerator (Right Tr) = Right <$> elements [ Th, Td ]
nextGenerator (Right Th) = safeBlockElem
nextGenerator (Right Td) = safeBlockElem
nextGenerator (Right Dl) = Right <$> elements [ Dt, Dd ]
nextGenerator (Right Dt) = Right <$> elements [ Dt, Dd ]
-- nextGenerator _ = safeBlockElem


render :: HTML -> View model action
render (Elem tag attrs children)
    = t tag (renderAttrs attrs) (map render children)
render (VoidElem tag attrs)
    = vt tag (renderAttrs attrs)
render (Text s) = text s


renderAttrs :: [ HtmlAttribute ] -> [ Attribute action ]
renderAttrs = map renderAttr


renderAttr :: HtmlAttribute -> Attribute action
renderAttr = uncurry mkAttr


mkAttr :: HtmlAttributeType -> MisoString -> Attribute action
mkAttr Class = class_
mkAttr Id = id_
mkAttr Title = title_
mkAttr Colspan = colspan_
mkAttr Rowspan = rowspan_
mkAttr Action = action_
mkAttr Method = method_
mkAttr Src = src_
mkAttr Alt = alt_
mkAttr Value = value_
mkAttr Type = type_
mkAttr Checked = checked_ . read . fromMisoString


-- get appropriate miso constructor for element having children
t :: ChildHavingHtmlTag -> MkTag model action
t Div = div_
t Span = span_
t P = p_
t Pre = pre_
t Ul = ul_
t Ol = ol_
t Li = li_
t Section = section_
t Header = header_
t Footer = footer_
t Nav = nav_
t Article = article_
t H1 = h1_
t H2 = h2_
t H3 = h3_
t H4 = h4_
t Strong = strong_
t Em = em_
t Table = table_
t Thead = thead_
t Tbody = tbody_
t Tr = tr_
t Td = td_
t Th = th_
t Form = form_
t Label = label_
t Button = button_
t Fieldset = fieldset_
t Legend = legend_
t Dl = dl_
t Dt = dt_
t Dd = dd_
t Figure = figure_
t Figcaption = figcaption_
t A = a_


safeBlocklTags :: [ ChildHavingHtmlTag ]
safeBlocklTags = [ Div, P, Pre, Ul, Ol, Section, Header, Footer, Nav, Article,
    H1, H2, H3, H4, Table, Form, Fieldset, Dl, Figure, Figcaption ]


safeBlockElem :: TagGen
safeBlockElem = Right <$> elements safeBlocklTags


inlineTags :: [ ChildHavingHtmlTag ]
inlineTags = [Span, Strong, Em, Label, Button, Legend, A]


-- get appropriate miso constructor for childless elem
vt :: ChildlessHtmlTag -> MkTag2 model action
vt Hr = hr_
vt Br = br_
vt Img = img_
vt Input = input_
vt Wbr = wbr_


genHtml :: Gen HTML
-- genHtml = sized $ \n -> genSubtree (n `mod` maxDepth)
genHtml = genSubtree 3 safeBlockElem


genSubtree :: Int -> TagGen -> Gen HTML
genSubtree depth gen
  | depth <= 1 = genText
  | otherwise = do
        tag <- gen
        case tag of
            Left voidTag -> undefined
            Right nonVoidTag -> do
                siblingCount <- choose (0, depth)
                siblings <- replicateM siblingCount $ oneof
                    [ genLeaf
                    , do
                        siblingTag <- arbitrary
                        siblingAttrs <- getAttributeGen siblingTag
                        siblingContent <- genLeaf
                        return $ Elem siblingTag siblingAttrs [ siblingContent ]
                    ]
                attrs <- getAttributeGen nonVoidTag
                children <- genSubtree (depth - 1) (nextGenerator nonVoidTag)
                return $ Elem nonVoidTag attrs $ siblings ++ [ children ]


genLeaf :: Gen HTML
genLeaf = oneof
    [ genText
    , do
        voidTag <- arbitrary
        VoidElem voidTag <$> getVoidAttributeGen voidTag
    ]


getAttributeGen :: ChildHavingHtmlTag -> Gen [ HtmlAttribute ]
getAttributeGen Table = tableAttributes
getAttributeGen Td = tableAttributes
getAttributeGen Th = tableAttributes
getAttributeGen _ = baseAttributes


getVoidAttributeGen :: ChildlessHtmlTag -> Gen [ HtmlAttribute ]
getVoidAttributeGen Img = imageAttributes
getVoidAttributeGen Input = inputAttributes
getVoidAttributeGen _ = baseAttributes


baseAttributes :: Gen [ HtmlAttribute ]
baseAttributes = do
    attrs <- sublistOf
        [ (Class,) <$> genCssIdent
        , (Id,) <$> genCssIdent
        , (Title,) <$> genSafeMisoString
        ]
    sequence attrs


tableAttributes :: Gen [ HtmlAttribute ]
tableAttributes = baseAttributes


tableCellAttributes :: Gen [ HtmlAttribute ]
tableCellAttributes = do
    base <- baseAttributes
    col <- frequency [(3, pure []), (1, pure [(Colspan, "2")])]
    row <- frequency [(3, pure []), (1, pure [(Rowspan, "2")])]
    return (base ++ col ++ row)


formAttributes :: Gen [ HtmlAttribute ]
formAttributes = do
  base <- baseAttributes
  return (base ++ [(Action, "/submit"), (Method, "post")])


imageAttributes :: Gen [ HtmlAttribute ]
imageAttributes = baseAttributes >>=
    return .
        (++
        [ (Src, placeholderImage)
        , (Alt, placeholderAltText)
        ])


inputAttributes :: Gen [ HtmlAttribute ]
inputAttributes = do
    typ <- elements ["text", "password", "checkbox", "radio", "submit", "number", "email", "tel"]
    base <- baseAttributes
    let extraAttrs =
            case typ of
                "checkbox" -> [(Value, "on"), (Checked, "True")]
                "radio"    -> [(Value, "option1"), (Checked, "True")]
                "submit"   -> [(Value, "Submit")]
                _          -> [(Value, "test-value")]
    return $ (Type, typ) : base ++ extraAttrs


chooseBoundedEnum :: (Bounded a, Enum a) => Gen a
chooseBoundedEnum = elements [minBound .. maxBound]


genText :: Gen HTML
genText = Text . toMisoString <$> genUnicodeString


-- | Generate Unicode strings for text nodes
genUnicodeString :: Gen String
genUnicodeString = listOf1 $ oneof
    [ choose ('a','z')
    , choose ('A','Z')
    , choose ('0','9')
    , elements " .,!?-_@#$%^&*()[]{}<>|\\/:;\"'"
    , choose ('\192','\255')   -- Latin-1 supplement
    , choose ('\1024','\1279') -- Cyrillic
    , choose ('\1280','\1327') -- Greek
    , choose ('\2304','\2431') -- Devanagari
    -- Emojis and pictographs
    , choose ('\x1F600','\x1F64F')   -- Emoticons
    , choose ('\x1F300','\x1F5FF')   -- Miscellaneous Symbols and Pictographs
    , choose ('\x1F680','\x1F6FF')   -- Transport and Map Symbols
    , choose ('\x1F900','\x1F9FF')   -- Supplemental Symbols and Pictographs
    -- Additional language blocks
    , choose ('\x0400','\x04FF')     -- Cyrillic (extended)
    , choose ('\x0530','\x058F')     -- Armenian
    , choose ('\x0590','\x05FF')     -- Hebrew
    , choose ('\x0600','\x06FF')     -- Arabic
    , choose ('\x0900','\x097F')     -- Devanagari (extended)
    , choose ('\x3040','\x309F')     -- Hiragana
    , choose ('\x30A0','\x30FF')     -- Katakana
    , choose ('\x4E00','\x9FFF')     -- CJK Unified Ideographs (common Chinese/Japanese characters)
    -- Symbols and special characters
    , choose ('\x2100','\x214F')     -- Letterlike Symbols
    , choose ('\x2190','\x21FF')     -- Arrows
    , choose ('\x2200','\x22FF')     -- Mathematical Operators
    , choose ('\x25A0','\x25FF')     -- Geometric Shapes
    , choose ('\x2600','\x26FF')     -- Miscellaneous Symbols
    , choose ('\x2700','\x27BF')     -- Dingbats
    , choose ('\x20A0','\x20CF')     -- Currency Symbols
    ] `suchThat` (\c -> not (isControl c) && c /= '\0' && c /= '\x200B' && c /= '\xFEFF')


genCssIdent :: Gen MisoString
genCssIdent = toMisoString <$> do
    len <- choose (1, 15)
    first <- elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['_']
    rest <- replicateM (len - 1) $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_-"
    return (first:rest)


genSafeMisoString :: Gen MisoString
genSafeMisoString = toMisoString <$> genSafeString


genSafeString :: Gen String
genSafeString = listOf1 $ oneof
  [ choose ('a','z')
  , choose ('A','Z')
  , choose ('0','9')
  , elements " .,!?-_"
  ] `suchThat` (not . isSpace)


placeholderImage :: MisoString
placeholderImage = "data:image/gif;base64,R0lGODlhAQABAIAAAAAAAP///yH5BAEAAAAALAAAAAABAAEAAAIBRAA7"


placeholderAltText :: MisoString
placeholderAltText = "Test image"
