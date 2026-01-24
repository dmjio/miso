-- Disclaimer: This is 99% vibe coded
-- Generates an html tree of depth n, with only one path down to the bottom, with
-- some extra sibling nodes along the way.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module HtmlGen2 where

import Miso hiding (on)
import Miso.Html.Element hiding (title_, data_)
import Miso.Html.Property hiding (label_, form_)
import Test.QuickCheck
import Control.Monad (replicateM)
import Data.Char (isControl, isSpace)
import Data.List (nubBy)
import Data.Function (on)

-- | Safe HTML generator for hydration testing
genHtml :: Gen (View model action)
genHtml = -- sized $ \n -> genSubtree (n `mod` 20)
    do
        -- n <- choose (1, 10)
        genSubtree 30
--genHtml = genSubtree 3

-- | Generate subtree with guaranteed depth
genSubtree :: Int -> Gen (View model action)
genSubtree depth
  | depth <= 1 = genText
  | otherwise = do
      (makeEl, attrGen) <- elements nonVoidElementSpecs
      attrs <- attrGen
      siblingCount <- choose (0, depth)
      siblings <- replicateM siblingCount $ oneof
        [ genLeaf
        , do
            (mkSibling, siblingAttrGen) <- elements nonVoidElementSpecs
            siblingAttrs <- siblingAttrGen
            siblingContent <- genLeaf
            return $ mkSibling siblingAttrs [siblingContent]
        ]
      children <- genSubtree (depth - 1)
      return $ makeEl attrs (siblings ++ [children])

genText :: Gen (View model action)
genText = text . toMisoString <$> genUnicodeString

-- | Generate leaf nodes (text or void elements)
genLeaf :: Gen (View model action)
genLeaf = oneof
  [ genText
  , do
      (makeVoid, attrGen) <- elements voidElementSpecs
      makeVoid <$> attrGen
  ]

-- | Base attributes safe for most elements (no duplicates)
baseAttributes :: Gen [Attribute action]
baseAttributes = do
  attrs <- sublistOf
    [ class_ <$> genCssIdent
    , id_ <$> genCssIdent
    , title_ <$> genSafeMisoString
    ]
  sequence attrs

-- | Table-specific attributes
tableAttributes :: Gen [Attribute action]
tableAttributes = baseAttributes

-- | Table cell specific attributes
tableCellAttributes :: Gen [Attribute action]
tableCellAttributes = do
  base <- baseAttributes
  col <- frequency [(3, pure []), (1, pure [colspan_ "2"])]
  row <- frequency [(3, pure []), (1, pure [rowspan_ "2"])]
  return (base ++ col ++ row)

-- | Form-specific attributes
formAttributes :: Gen [Attribute action]
formAttributes = do
  base <- baseAttributes
  return (base ++ [action_ "/submit", method_ "post"])

-- | Link-specific attributes
linkAttributes :: Gen [Attribute action]
linkAttributes = do
  base <- baseAttributes
  return (base ++ [href_ "/test-path", target_ "_blank"])

-- | Image-specific attributes (with required safe properties)
imageAttributes :: Gen [Attribute action]
imageAttributes = do
  base <- baseAttributes
  -- Ensure unique attributes by property name
  return $ nubBy ((==) `on` attrName) (base ++ [src_ placeholderImage, alt_ placeholderAltText])
  where
    attrName (Property n _) = n
    attrName _ = ""

-- | Input-specific attributes with safe type selection
inputAttributes :: Gen [Attribute action]
inputAttributes = do
  typ <- elements ["text", "password", "checkbox", "radio", "submit", "number", "email", "tel"]
  base <- baseAttributes
  let typeAttr = type_ typ
      extraAttrs = case typ of
                     "checkbox" -> [value_ "on", checked_ True]
                     "radio"    -> [value_ "option1", checked_ True]
                     "submit"   -> [value_ "Submit"]
                     _          -> [value_ "test-value"]
  return $ nubBy ((==) `on` attrName) (typeAttr : base ++ extraAttrs)

  where
    attrName (Property n _) = n
    attrName _ = ""

-- | Element specifications with custom attribute generators
type ElementSpec model action =
  ( [Attribute action] -> [View model action] -> View model action
  , Gen [Attribute action]
  )

nonVoidElementSpecs :: [ElementSpec model action]
nonVoidElementSpecs =
  [ (div_, baseAttributes)
  , (span_, baseAttributes)
  , (p_, baseAttributes)
  , (ul_, baseAttributes)
  , (ol_, baseAttributes)
  , (li_, baseAttributes)
  , (section_, baseAttributes)
  , (header_, baseAttributes)
  , (footer_, baseAttributes)
  , (nav_, baseAttributes)
  , (article_, baseAttributes)
  , (h1_, baseAttributes)
  , (h2_, baseAttributes)
  , (h3_, baseAttributes)
  , (h4_, baseAttributes)
  , (strong_, baseAttributes)
  , (em_, baseAttributes)
  , (table_, tableAttributes)
  , (thead_, baseAttributes)
  , (tbody_, baseAttributes)
  , (tr_, baseAttributes)
  , (td_, tableCellAttributes)
  , (th_, tableCellAttributes)
  , (form_, formAttributes)
  , (label_, baseAttributes)
  , (button_, baseAttributes)
  , (fieldset_, baseAttributes)
  , (legend_, baseAttributes)
  , (dl_, baseAttributes)
  , (dt_, baseAttributes)
  , (dd_, baseAttributes)
  , (figure_, baseAttributes)
  , (figcaption_, baseAttributes)
  , (a_, linkAttributes)  -- Added anchor element with safe href
  ]

type VoidElementSpec model action =
  ( [Attribute action] -> View model action
  , Gen [Attribute action]
  )

voidElementSpecs :: [VoidElementSpec model action]
voidElementSpecs =
  [ (hr_, baseAttributes)
  , (br_, baseAttributes)
  , (img_, imageAttributes)  -- Constructive safety
  , (input_, inputAttributes)  -- Multiple safe types
  , (wbr_, baseAttributes)
  ]

-- | Generate valid CSS identifiers
-- genCssIdent :: Gen MisoString
-- genCssIdent = sized $ \n -> toMisoString <$> do
--   len <- choose (1, max 1 (min n 15))
--   first <- elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['_']
--   rest <- replicateM (len - 1) $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_-"
--   return (first:rest)

genCssIdent :: Gen MisoString
genCssIdent = toMisoString <$> do
  len <- choose (1, 15)
  first <- elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['_']
  rest <- replicateM (len - 1) $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_-"
  return (first:rest)

genSafeMisoString :: Gen MisoString
genSafeMisoString = toMisoString <$> genSafeString

-- | Generate plain ASCII strings for attributes
genSafeString :: Gen String
genSafeString = listOf1 $ oneof
  [ choose ('a','z')
  , choose ('A','Z')
  , choose ('0','9')
  , elements " .,!?-_"
  ] `suchThat` (not . isSpace)

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

-- | Constants for safety
placeholderImage :: MisoString
placeholderImage = "data:image/gif;base64,R0lGODlhAQABAIAAAAAAAP///yH5BAEAAAAALAAAAAABAAEAAAIBRAA7"

placeholderAltText :: MisoString
placeholderAltText = "Test image"
