{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <&>" #-}

module HtmlGen where

import GHC.Generics
import Miso hiding (on, Src, Checked, Object)
import Miso.Html.Element hiding (title_, data_)
import Miso.Html.Property hiding (label_, form_)
import Miso.JSON
    ( FromJSON (..)
    , ToJSON (..)
    , object
    , (.:)
    , (.=)
    , Value (..)
    , Parser
    )
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
    deriving (Eq, Generic, FromJSON, Show)

instance ToJSON HtmlAttributeType where
    toJSON Class   = String "Class"
    toJSON Id      = String "Id"
    toJSON Title   = String "Title"
    toJSON Colspan = String "Colspan"
    toJSON Rowspan = String "Rowspan"
    toJSON Method  = String "Method"
    toJSON Action  = String "Action"
    toJSON Alt     = String "Alt"
    toJSON Src     = String "Src"
    toJSON Value   = String "Value"
    toJSON Type    = String "Type"
    toJSON Checked = String "Checked"


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
    deriving (Eq, Enum, Bounded, Generic, FromJSON, Show)

instance ToJSON ChildHavingHtmlTag where
    toJSON Div        = String "Div"
    toJSON Span       = String "Span"
    toJSON P          = String "P"
    toJSON Pre        = String "Pre"
    toJSON Ul         = String "Ul"
    toJSON Ol         = String "Ol"
    toJSON Li         = String "Li"
    toJSON Section    = String "Section"
    toJSON Header     = String "Header"
    toJSON Footer     = String "Footer"
    toJSON Nav        = String "Nav"
    toJSON Article    = String "Article"
    toJSON H1         = String "H1"
    toJSON H2         = String "H2"
    toJSON H3         = String "H3"
    toJSON H4         = String "H4"
    toJSON Strong     = String "Strong"
    toJSON Em         = String "Em"
    toJSON Table      = String "Table"
    toJSON Thead      = String "Thead"
    toJSON Tbody      = String "Tbody"
    toJSON Tr         = String "Tr"
    toJSON Td         = String "Td"
    toJSON Th         = String "Th"
    toJSON Form       = String "Form"
    toJSON Label      = String "Label"
    toJSON Button     = String "Button"
    toJSON Fieldset   = String "Fieldset"
    toJSON Legend     = String "Legend"
    toJSON Dl         = String "Dl"
    toJSON Dt         = String "Dt"
    toJSON Dd         = String "Dd"
    toJSON Figure     = String "Figure"
    toJSON Figcaption = String "Figcaption"
    toJSON A          = String "A"

instance Arbitrary ChildHavingHtmlTag where
  arbitrary = chooseBoundedEnum


data ChildlessHtmlTag
    = Hr
    | Br
    | Img
    | Input
    | Wbr
    deriving (Eq, Enum, Bounded, Generic, FromJSON, Show)

instance ToJSON ChildlessHtmlTag where
    toJSON Hr     = String "Hr"
    toJSON Br     = String "Br"
    toJSON Img    = String "Img"
    toJSON Input  = String "Input"
    toJSON Wbr    = String "Wbr"

instance Arbitrary ChildlessHtmlTag where
  arbitrary = chooseBoundedEnum


data HTML
    = Elem ChildHavingHtmlTag [ HtmlAttribute ] [ HTML ]
    | VoidElem ChildlessHtmlTag [ HtmlAttribute ]
    | Text MisoString
    | UserSuppliedElement
    deriving (Eq, Show)

instance ToJSON HTML where
    toJSON (Text s) =
        object
            [ "type"  .= ("Text" :: MisoString)
            , "value" .= s
            ]

    toJSON (Elem tag attrs children) =
        object
            [ "type"     .= ("Elem" :: MisoString)
            , "tag"      .= tag
            , "attrs"    .= attrs
            , "children" .= children
            ]

    toJSON (VoidElem tag attrs) =
        object
            [ "type"  .= ("VoidElem" :: MisoString)
            , "tag"   .= tag
            , "attrs" .= attrs
            ]

    toJSON UserSuppliedElement =
        object
            [ "type" .= ("UserSuppliedElement" :: MisoString)
            ]


instance FromJSON HTML where
    parseJSON (Object o) = do
        typ <- (o .: "type") :: Parser MisoString

        case typ of
            "Text"     -> Text     <$> o .: "value"

            "Elem"     -> Elem     <$> o .: "tag"
                                   <*> o .: "attrs"
                                   <*> o .: "children"

            "Voidelem" -> VoidElem <$> o .: "tag"
                                   <*> o .: "attrs"

            "UserSuppliedElement" -> pure UserSuppliedElement
            _          -> fail $ "Unknown HTML constructor type: " ++ show typ

    parseJSON _ = fail "Expected JSON Object for HTML deserialization"


instance Arbitrary HTML where
  arbitrary = genHtml


type MkTag model action = [ Attribute action ] -> [ View model action ] -> View model action
type MkTag2 model action = [ Attribute action ] -> View model action


nextGenerator :: ChildHavingHtmlTag -> Gen ChildHavingHtmlTag
nextGenerator Ul = return Li
nextGenerator Ol = return Li
nextGenerator Table = return Tbody
nextGenerator Thead = return Tr
nextGenerator Tbody = return Tr
nextGenerator Tr = elements [ Th, Td ]
nextGenerator Th = safeInlineElem
nextGenerator Td = anyElem
nextGenerator Dl = elements [ Dt, Dd ]
nextGenerator Dt = safeInlineElem
nextGenerator Dd = safeInlineElem
nextGenerator H1 = safeInlineElem
nextGenerator H2 = safeInlineElem
nextGenerator H3 = safeInlineElem
nextGenerator H4 = safeInlineElem
nextGenerator Span = safeInlineElem
nextGenerator Strong = safeInlineElem
nextGenerator Em = safeInlineElem
nextGenerator Label = safeInlineElem
nextGenerator Button = safeInlineElem
nextGenerator Legend = safeInlineElem
nextGenerator A = safeInlineElem
nextGenerator P = safeInlineElem
nextGenerator Pre = safeInlineElem
nextGenerator _ = anyElem


render :: View model action -> HTML -> View model action
render v (Elem tag attrs children)
    = t tag (renderAttrs attrs) (map (render v) children)
render _ (VoidElem tag attrs)
    = vt tag (renderAttrs attrs)
render _ (Text s) = text s
render v UserSuppliedElement = v


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


safeBlockTags :: [ ChildHavingHtmlTag ]
-- Header, Footer, H2-H4
safeBlockTags = [ Div, P, Pre, Ul, Ol, Section, Nav, Article,
    H1, Table, Fieldset, Figure]


inlineTags :: [ ChildHavingHtmlTag ]
-- inlineTags = [Span, Strong, Em, Label, Button, A]
inlineTags = [Span, Strong, Em, Label]


safeInlineTags :: [ ChildHavingHtmlTag ]
safeInlineTags = [Span, Strong, Em]


anyElem :: Gen ChildHavingHtmlTag
anyElem = frequency $ [ (10, elements safeBlockTags),  (1, elements inlineTags) ]


safeInlineElem :: Gen ChildHavingHtmlTag
safeInlineElem = elements safeInlineTags


tagRequiresChildren :: [ ChildHavingHtmlTag ]
tagRequiresChildren = [ Table, Ol, Ul, Dl, Tbody, Thead, Tr ]


-- get appropriate miso constructor for childless elem
vt :: ChildlessHtmlTag -> MkTag2 model action
vt Hr = hr_
vt Br = br_
vt Img = img_
vt Input = input_
vt Wbr = wbr_


genHtml :: Gen HTML
genHtml = sized $ \n -> genSubtree n (elements safeBlockTags)


genSubtree :: Int -> Gen ChildHavingHtmlTag -> Gen HTML
genSubtree depth gen
  | depth <= 1 = return UserSuppliedElement
  | otherwise = do
        nonVoidTag <- gen
        siblingCount <- choose (0, depth)
        siblings <- replicateM siblingCount $
            do
                siblingTag <- nextGenerator nonVoidTag
                siblingAttrs <- getAttributeGen siblingTag
                siblingContent <-
                    if elem siblingTag tagRequiresChildren then
                        genSubtree 2 (nextGenerator siblingTag)
                    else
                        genLeaf
                return $ Elem siblingTag siblingAttrs [ siblingContent ]

        attrs <- getAttributeGen nonVoidTag

        let j = if elem nonVoidTag tagRequiresChildren then 0 else 1
        children <- genSubtree (depth - j) (nextGenerator nonVoidTag)

        return $ Elem nonVoidTag attrs $ siblings ++ [ children ]


genLeaf :: Gen HTML
genLeaf = oneof
    [ genText
    , do
        voidTag <- arbitrary `suchThat` (/= Hr) -- hr tag causes issues
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
        -- , (Id,) <$> genCssIdent
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
    return $ (Type, typ) : base ++ (addValue typ)

    where
        -- addValue "checkbox" = [(Value, "on"), (Checked, "True")]
        -- addValue "radio"    = [(Value, "option1"), (Checked, "True")]
        addValue "checkbox" = [(Value, "on")]
        addValue "radio"    = [(Value, "option1")]
        addValue "submit"   = [(Value, "Submit")]
        addValue "email"    = [(Value, "email@example.com")]
        addValue "tel"      = [(Value, "+1 (555)-5555")]
        addValue "number"   = [(Value, "12345")]
        addValue _          = [(Value, "test-value")]


chooseBoundedEnum :: (Bounded a, Enum a) => Gen a
chooseBoundedEnum = elements [minBound .. maxBound]


genText :: Gen HTML
genText = Text . toMisoString <$> genUnicodeString


-- genUnicodeString :: Gen String
-- genUnicodeString = listOf1 $ elements ">\""


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
