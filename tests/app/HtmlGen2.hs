{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module HtmlGen2 where

import Miso
import Miso.Html.Element hiding (title_, data_)
import Miso.Html.Property hiding (label_, form_)
import Test.QuickCheck
import Control.Monad (replicateM)

-- | Safe HTML generator for hydration testing
genHtml :: Gen (View model action)
genHtml = -- sized genSubtree
    genSubtree 10

-- | Generate subtree with guaranteed depth
genSubtree :: Int -> Gen (View model action)
genSubtree depth
  | depth <= 1 = genText
  | otherwise = do        -- Above max depth, always generate parents
      makeEl <- elements nonVoidElements
      -- attrs <- genSafeAttributes
      siblingCount <- choose (0, depth)
      siblings <- replicateM siblingCount $ do
        oneof
            [ genLeaf
            , do
                mkSibling <- elements nonVoidElements
                -- siblingAttrs <- genSafeAttributes
                siblingContent <- genLeaf
                return $ mkSibling [] [ siblingContent ]
            ]

      children <- genSubtree (depth - 1)
      return $ makeEl [] (siblings ++ [ children ])


genText :: Gen (View model action)
genText = text . toMisoString <$> genSafeString


-- | Generate leaf nodes (text or void elements)
genLeaf :: Gen (View model action)
genLeaf = oneof
  [ genText
  , do
      makeVoid <- elements voidElements
      -- makeVoid <$> genSafeAttributes
      return $ makeVoid []
  ]

nonVoidElements :: [[Attribute action] -> [View model action] -> View model action]
nonVoidElements =
  [ div_, span_, p_, ul_, ol_, li_
  , section_, header_, footer_, nav_, article_
  , h1_, h2_, h3_, h4_, strong_, em_
  , table_, thead_, tbody_, tr_, td_, th_
  , form_, label_, button_, fieldset_, legend_
  , dl_, dt_, dd_, figure_, figcaption_
  ]

voidElements :: [[Attribute action] -> View model action]
voidElements =
  [ hr_
  , br_
  , img_ . ensureImgAttrs
  , input_ . ensureInputAttrs
  , wbr_
  ]

placeholderAltText :: MisoString
placeholderAltText = "Placeholder"

-- | Ensure safe attributes for img elements by filtering out unsafe properties
ensureImgAttrs :: [Attribute action] -> [Attribute action]
ensureImgAttrs attrs =
  let safeAttrs = filter (not . isUnsafeImgAttr) attrs
  in safeAttrs ++ [src_ placeholderImage, alt_ placeholderAltText]
  where
    isUnsafeImgAttr :: Attribute action -> Bool
    isUnsafeImgAttr (Property name _) = 
      name == "src" || name == "alt"
    isUnsafeImgAttr _ = False

-- | Ensure safe attributes for input elements by filtering out type attributes
ensureInputAttrs :: [Attribute action] -> [Attribute action]
ensureInputAttrs attrs =
  let safeAttrs = filter (not . isTypeAttr) attrs
  in safeAttrs ++ [type_ "text"]
  where
    isTypeAttr :: Attribute action -> Bool
    isTypeAttr (Property name _) = name == "type"
    isTypeAttr _ = False

-- | SAFE ATTRIBUTE GENERATOR (no scripts/events)
genSafeAttributes :: Gen [Attribute action]
genSafeAttributes = listOf $ oneof
  [ class_ <$> genClassName
  , id_ <$> genIdName
  , title_ <$> genSafeMisoString
  , hidden_ <$> arbitrary
  , href_ <$> elements [ "#", "/test", "https://example.com/test" ]
  , alt_ <$> genSafeMisoString
  , colspan_ <$> elements ["1", "2"]
  , rowspan_ <$> elements ["1", "2"]
  , value_ <$> genSafeMisoString
  , checked_ <$> arbitrary
  , pure disabled_ 
  , name_ <$> genSafeMisoString
  , placeholder_ <$> genSafeMisoString
  ]

genClassName :: Gen MisoString
genClassName = elements
  ([ "container"
  , "item"
  , "header"
  , "content"
  , "footer"
  , "button"
  , "active"
  ] :: [ MisoString ])

genIdName :: Gen MisoString
genIdName = elements
  ([ "main", "sidebar", "content", "nav", "form", "submit", "test-id" ] :: [ MisoString ])

genSafeMisoString :: Gen MisoString
genSafeMisoString = toMisoString <$> genSafeString

genSafeString :: Gen String
genSafeString = listOf1 $ elements $
  ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " .,!?-"

-- | Constants for safety
placeholderImage :: MisoString
placeholderImage =
  "data:image/gif;base64,R0lGODlhAQABAIAAAAAAAP///yH5BAEAAAAALAAAAAABAAEAAAIBRAA7"
