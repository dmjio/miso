{-# LANGUAGE OverloadedStrings #-}

module HtmlGen where

import Test.QuickCheck
import Control.Monad (replicateM)
import Miso
import Miso.Html.Element hiding (title_)
import Miso.Html.Property
import Data.Maybe (catMaybes)

-- Generate body content with appropriate structure
genBodyContent :: Gen [View model action]
genBodyContent = sized $ \size ->
  if size <= 0
    then pure [text "Base content"]
    else do
      n <- choose (1, min 5 (size `div` 2))
      vectorOf n (resize (max 1 (size `div` n)) genSectionalElement)

-- Elements that can be direct children of body
genSectionalElement :: Gen (View model action)
genSectionalElement = oneof [
    genHeader,
    genNav,
    genMain,
    genArticle,
    genSection,
    genAside,
    genFooter,
    genDiv
  ]

-- Generate a header element
genHeader :: Gen (View model action)
genHeader = sized $ \size -> do
  attrs <- genCommonAttributes "header"
  let contentSize = max 0 (size - 1)
  children <- genHeaderContent contentSize
  return (header_ attrs children)

genHeaderContent :: Int -> Gen [View model action]
genHeaderContent size = do
  n <- choose (0, min 3 size)
  replicateM n $ oneof [
    genHeading,
    genNav,
    genDiv,
    pure (img_ [src_ "data:image/gif;base64,R0lGODlhAQABAIAAAAAAAP///yH5BAEAAAAALAAAAAABAAEAAAIBRAA7",
                alt_ "Logo"])
    ]

-- Generate a section element (rewritten for consistency)
genSection :: Gen (View model action)
genSection = sized $ \size -> do
  attrs <- genCommonAttributes "section"
  let contentSize = max 0 (size - 1)
  children <- genSectionContent contentSize
  return (section_ attrs children)

genSectionContent :: Int -> Gen [View model action]
genSectionContent size = do
  n <- choose (0, min 4 size)
  replicateM n $ oneof [
    genHeading,
    genParagraph,
    genList,
    genDiv,
    genTable
    ]

-- Generate a table with valid structure
genTable :: Gen (View model action)
genTable = sized $ \size -> do
  attrs <- genCommonAttributes "table"
  let tableSize = max 0 (size - 1)
  
  -- Always include a caption for accessibility testing
  caption <- genCaption
  
  -- Table head is optional but recommended
  hasHead <- choose (True, False)
  thead <- if hasHead 
           then Just <$> genTableHead (tableSize `div` 3)
           else pure Nothing
  
  -- Table body is required
  tbody <- genTableBody (tableSize `div` 2)
  
  -- Table foot is optional
  hasFoot <- choose (True, False)
  tfoot <- if hasFoot && size > 3
           then Just <$> genTableFoot (tableSize `div` 4)
           else pure Nothing
  
  return $ table_ attrs $ catMaybes [
    Just caption,
    fmap (thead_ []) thead,
    Just (tbody_ [] tbody),
    fmap (tfoot_ []) tfoot
    ]

genCaption :: Gen (View model action)
genCaption = do
  textContent <- elements ["Test Data", "Sample Information", "Metrics"]
  return (caption_ [] [text textContent])

genTableHead :: Int -> Gen [View model action]
genTableHead size = do
  nRows <- choose (1, min 2 size)
  replicateM nRows $ do
    nCells <- choose (1, min 4 size)
    cells <- replicateM nCells $ do
      content <- elements ["Header", "Column", "Field", "Value"]
      return (th_ [scope_ "col"] [text content])
    return (tr_ [] cells)

genTableBody :: Int -> Gen [View model action]
genTableBody size = do
  nRows <- choose (1, min 3 size)
  replicateM nRows $ do
    nCells <- choose (1, min 4 size)
    cells <- replicateM nCells $ do
      content <- elements ["Data", "Info", "Value", "Item"]
      return (td_ [] [text content])
    return (tr_ [] cells)

genTableFoot :: Int -> Gen [View model action]
genTableFoot size = do
  nRows <- choose (1, min 1 size)  -- Usually just one row for footer
  replicateM nRows $ do
    nCells <- choose (1, min 4 size)
    cells <- replicateM nCells $ do
      content <- elements ["Total", "Summary", "Average"]
      return (td_ [] [text content])
    return (tr_ [] cells)

-- Generate heading elements (h1-h6)
genHeading :: Gen (View model action)
genHeading = do
  level <- choose (1 :: Int, 6)
  textContent <- elements ["Introduction", "Main Content", "Section Title", "Important Note", "Summary"]
  let attrs = [class_ ("heading heading-" <> toMisoString (show level))]
  case level of
    1 -> return (h1_ attrs [text textContent])
    2 -> return (h2_ attrs [text textContent])
    3 -> return (h3_ attrs [text textContent])
    4 -> return (h4_ attrs [text textContent])
    5 -> return (h5_ attrs [text textContent])
    _ -> return (h6_ attrs [text textContent])

-- Generate a paragraph with inline elements
genParagraph :: Gen (View model action)
genParagraph = do
  attrs <- genCommonAttributes "p"
  content <- genParagraphContent
  return (p_ attrs content)

genParagraphContent :: Gen [View model action]
genParagraphContent = do
  n <- choose (1, 4)
  replicateM n $ frequency [
    (5, text <$> elements ["Simple text ", "More content ", "Additional information "]),
    (2, genInlineElement),
    (1, pure (br_ []))
    ]

-- Generate inline elements for text content
genInlineElement :: Gen (View model action)
genInlineElement = oneof [
    genSpan,
    genStrong,
    genEm,
    genA,
    genCode
  ]

genSpan :: Gen (View model action)
genSpan = do
  attrs <- genCommonAttributes "span"
  content <- text <$> elements ["highlighted text", "special content", "formatted text"]
  return (span_ attrs [content])

genStrong :: Gen (View model action)
genStrong = do
  content <- text <$> elements ["important", "critical", "essential"]
  return (strong_ [class_ "emphasis"] [content])

genEm :: Gen (View model action)
genEm = do
  content <- text <$> elements ["emphasis", "note", "caution"]
  return (em_ [] [content])

genA :: Gen (View model action)
genA = do
  attrs <- pure [href_ "#", class_ "test-link"]
  content <- text <$> elements ["click here", "more details", "external resource"]
  return (a_ attrs [content])

genCode :: Gen (View model action)
genCode = do
  content <- text <$> elements ["code()", "function{}", "variable"]
  return (code_ [class_ "code-sample"] [content])

-- Generate a simple list
genList :: Gen (View model action)
genList = do
  isOrdered <- choose (True, False)
  nItems <- choose (1, 4)
  items <- replicateM nItems $ do
    content <- genListItemContent
    return (li_ [] content)
  
  if isOrdered
    then return (ol_ [] items)
    else return (ul_ [] items)

genListItemContent :: Gen [View model action]
genListItemContent = do
  hasNested <- choose (True, False)

  if hasNested && False -- disabled for now to keep structure simpler
    then oneof [
      (:[]) <$>genList,
      (:[]) <$> genParagraph
      ]
    else do
        elem_ <- elements ["List item", "Bullet point", "Menu option"]
        return [text elem_]

-- Generate a simple div with content
genDiv :: Gen (View model action)
genDiv = sized $ \size -> do
  attrs <- genCommonAttributes "div"
  children <- if size > 1
              then resize (size - 1) genDivContent
              else pure [text "Simple content"]
  return (div_ attrs children)

genDivContent :: Gen [View model action]
genDivContent = sized $ \size -> do
  n <- choose (0, min 3 size)
  replicateM n $ frequency [
    (5, genTextInline),
    (3, genInlineElement),
    (2, genParagraph),
    (1, genList)
    ]

genTextInline :: Gen (View model action)
genTextInline = text <$> elements [
    "Hello world",
    "Test content",
    "Sample text",
    "Some data",
    "More information"
  ]

-- Generate common safe attributes
genCommonAttributes :: MisoString -> Gen [Attribute action]
genCommonAttributes elemType = do
  idInt <- chooseAny :: Gen Int

  let baseAttrs =
          [ class_ ("test-" <> elemType <> "-" <> toMisoString (show idInt))
          , id_ ("id-" <> elemType)
          ]
  
  extraAttrs <- case elemType of
    "input" -> pure [type_ "text", placeholder_ "Test input"]
    "img" -> pure [alt_ "Test image"]
    "a" -> pure [href_ "#"]
    "button" -> pure [type_ "button"]
    _ -> pure []
  
  n <- choose (0, 2)
  extra <- vectorOf n $ elements [
      hidden_ False,
      lang_ "en",
      draggable_ True,
      title_ ("Test " <> elemType)
    ]
  
  return (baseAttrs <> extraAttrs <> extra)


-- Generate a nav element
genNav :: Gen (View model action)
genNav = sized $ \size -> do
  attrs <- genCommonAttributes "nav"
  let contentSize = max 0 (size - 1)
  children <- genNavContent contentSize
  return (nav_ attrs children)

genNavContent :: Int -> Gen [View model action]
genNavContent size = do
  n <- choose (0, min 3 size)
  replicateM n $ oneof [
    genList,
    genA,
    genDiv
    ]


-- Generate a main element
genMain :: Gen (View model action)
genMain = sized $ \size -> do
  attrs <- genCommonAttributes "main"
  let contentSize = max 0 (size - 1)
  children <- genMainContent contentSize
  return (main_ attrs children)

genMainContent :: Int -> Gen [View model action]
genMainContent size = do
  n <- choose (0, min 4 size)
  replicateM n $ oneof [
    genHeading,
    genParagraph,
    genArticle,
    genSection,
    genAside,
    genDiv
    ]


-- Generate an article element
genArticle :: Gen (View model action)
genArticle = sized $ \size -> do
  attrs <- genCommonAttributes "article"
  let contentSize = max 0 (size - 1)
  children <- genArticleContent contentSize
  return (article_ attrs children)

genArticleContent :: Int -> Gen [View model action]
genArticleContent size = do
  n <- choose (0, min 4 size)
  replicateM n $ oneof [
    genHeading,
    genParagraph,
    genSection,
    genAside,
    genDiv,
    genList
    ]


-- Generate an aside element
genAside :: Gen (View model action)
genAside = sized $ \size -> do
  attrs <- genCommonAttributes "aside"
  let contentSize = max 0 (size - 1)
  children <- genAsideContent contentSize
  return (aside_ attrs children)

genAsideContent :: Int -> Gen [View model action]
genAsideContent size = do
  n <- choose (0, min 3 size)
  replicateM n $ oneof [
    genHeading,
    genParagraph,
    genList,
    genDiv
    ]


-- Generate a footer element
genFooter :: Gen (View model action)
genFooter = sized $ \size -> do
  attrs <- genCommonAttributes "footer"
  let contentSize = max 0 (size - 1)
  children <- genFooterContent contentSize
  return (footer_ attrs children)

genFooterContent :: Int -> Gen [View model action]
genFooterContent size = do
  n <- choose (0, min 3 size)
  replicateM n $ oneof [
    genParagraph,
    genAddress,
    genDiv,
    pure (text "© 2026 Example Corp")
    ]

-- Helper for address content (used in footer)
genAddress :: Gen (View model action)
genAddress = do
  attrs <- genCommonAttributes "address"
  content <- genAddressContent
  return (address_ attrs content)

genAddressContent :: Gen [View model action]
genAddressContent = do
  n <- choose (1, 3)
  replicateM n $ oneof [
    elements ["Example Corp", "123 Main St", "contact@example.com"],
    pure $ br_ []
    ]
