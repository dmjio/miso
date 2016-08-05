{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import           Data.Aeson   hiding (Object)
import           Data.Bool
import qualified Data.Map.Strict as M
import           Data.Maybe (isJust)
import           Data.Monoid
import           Data.Proxy
import qualified Data.Text    as T
import           GHC.Generics (Generic)
import           Miso

-- MODEL


data Model = Model
  { drag :: Maybe Drag
  , grid :: [[Maybe (ChessColor, ChessPiece)]]
  } deriving (Show, Generic, Eq)
instance ToJSON Model
instance FromJSON Model


data Drag = Drag
  { payload     :: DragPayload
  , hoverCoords :: Maybe Coords
  } deriving (Show, Generic, Eq)
instance ToJSON Drag
instance FromJSON Drag

data Coords = Coords { row :: Int, col :: Int }
              deriving (Show, Generic, Eq)
instance ToJSON Coords
instance FromJSON Coords

data DragPayload = DragPayload
  { piece :: (ChessColor, ChessPiece)
  , fromCoords :: Maybe Coords
  } deriving (Show, Generic, Eq)
instance ToJSON DragPayload
instance FromJSON DragPayload

data ChessColor = White | Black
                  deriving (Show, Generic, Eq, Enum, Bounded)
instance ToJSON ChessColor
instance FromJSON ChessColor

data ChessPiece = Pawn | Knight | Bishop | Rook | Queen | King
                  deriving (Show, Generic, Eq, Enum, Bounded)
instance ToJSON ChessPiece
instance FromJSON ChessPiece

stepConfig :: Proxy '[SaveToLocalStorage "chess"]
stepConfig = Proxy

getInitialModel :: IO Model
getInitialModel = do
 getFromStorage "chess" >>= \case
    Left x  -> putStrLn x >> pure emptyModel
    Right Model{grid=grid} -> pure emptyModel{grid=grid}

emptyModel :: Model
emptyModel = Model
  { drag = Nothing
  , grid = replicate 8 emptyRow
  }

emptyRow :: [Maybe (ChessColor, ChessPiece)]
emptyRow = replicate 8 Nothing


main :: IO ()
main = do
  model <- getInitialModel
  startApp model view update defaultEvents stepConfig []


-- UPDATE


data Msg
    = DragStart DragPayload
    | DragDrop Coords
    | DragEnd
    | HoverOver (Maybe Coords)
    deriving (Show, Generic, Eq)


updateDragStart :: DragPayload -> Model -> Model
updateDragStart payload model =
  model { drag = Just $ Drag
    { payload     = payload
    , hoverCoords = Nothing
    }
  }

-- 0-indexed; partial!
replaceAt :: Int -> a -> [a] -> [a]
replaceAt n value xs = take n xs ++ [value] ++ drop (n+1) xs

replaceCoord :: Coords -> a -> [[a]] -> [[a]]
replaceCoord Coords{..} value xss =
  replaceAt row (replaceAt col value (xss !! row)) xss


updateDragEnd :: DragPayload -> Coords -> Model -> Model
updateDragEnd DragPayload{..} coords model@Model{grid=grid} =
  model{drag = Nothing, grid = newGrid}
  where
    newGrid = replaceCoord coords (Just piece) $ case fromCoords of
      Nothing -> grid
      Just fromCoords' -> replaceCoord fromCoords' Nothing grid


update :: Msg -> Model -> Effect Msg Model
update msg model@Model{..} = noEff $ case msg of
  DragStart payload -> updateDragStart payload model

  DragDrop coords -> case drag of
    Nothing -> model
    Just Drag{..} -> updateDragEnd payload coords model

  DragEnd -> model{drag=Nothing}

  HoverOver coords -> case drag of
    Nothing    -> model { drag = Nothing }
    Just drag' -> model { drag = Just drag'{ hoverCoords = coords } }


-- VIEW


view :: Model -> VTree Msg
view model =
  div_ [class_ "container"] [
    div_ [class_ "row"] [
      div_ [class_ "col s12"] [
        colorPalette Black,
        div_ [class_ "center-align"] [board model],
        colorPalette White
      ]
    ]
  ]


colorPalette :: ChessColor -> VTree Msg
colorPalette color =
  div_ [class_ "palette center-align"] [
    palettePiece (color, piece) | piece <- [minBound..maxBound]
  ]


palettePiece :: (ChessColor, ChessPiece) -> VTree Msg
palettePiece colorPiece =
  div_
    [ onDragStart $ DragStart $ DragPayload{piece = colorPiece, fromCoords = Nothing}
    , attr "draggable" "true"
    , class_ "palette-piece"
    ]
    [symbolFor colorPiece]


matchingHoverCoords :: Coords -> Maybe ChessColor -> Model -> Bool
matchingHoverCoords coords targetColor Model{drag=drag} = case drag of
  Nothing       -> False
  Just Drag{..} -> case hoverCoords of
    Nothing           -> False
    Just hoverCoords' ->
      let piece'@(dragColor, _) = piece payload
          capturing = maybe False (dragColor /=) targetColor
          valid = case fromCoords payload of
            Nothing          -> True
            Just fromCoords' -> validMove capturing fromCoords' hoverCoords' piece'
       in hoverCoords' == coords && valid


classList :: [(T.Text, Bool)] -> Attribute msg
classList xs = class_ $ T.intercalate " " [x | (x, test) <- xs, test]


board :: Model -> VTree Msg
board model@Model{..} =
  div_
    [ class_ "board"
    , onDragLeave $ HoverOver Nothing
    , onDragEnd $ DragEnd
    ]
    [ div_ [class_ "vertical-flex"] $
        iterMap grid $ \row cols ->
           div_ [class_ "horizontal-flex"] $
             iterMap cols $ \col cell ->
             let coords = Coords{col = col, row = row}

                 valid = case drag of
                   Nothing -> False
                   Just Drag{payload=DragPayload{..}} -> case fromCoords of
                     Nothing -> True
                     Just fromCoords' ->
                       let (dragColor, _) = piece
                           capturing = maybe False (dragColor /=) (fst <$> cell)
                        in validMove capturing fromCoords' coords piece

                 matches = matchingHoverCoords coords (fst <$> cell) model
             in
               div_
                (
                  [ classList [ ("flex-item", True), ("hovering", matches), ("invalid", isJust drag && not valid) ] ]
                  <>
                  (case drag of
                    Nothing -> []
                    Just _  ->
                      [ onDragOver (PreventDefault matches) $ HoverOver (Just coords)
                      , onDrop (PreventDefault matches) $ DragDrop coords
                      ]
                  )
                )
                (case cell of
                   Nothing         -> []
                   Just colorPiece ->
                     [ div_
                       [ attr "draggable" "true"
                       , style_ $ M.singleton "cursor" "pointer"
                       , onDragStart $ DragStart $ DragPayload{piece = colorPiece, fromCoords = Just coords}
                       ]
                       [symbolFor colorPiece]
                     ]
                )
    ]


iterMap :: [a] -> (Int -> a -> b) -> [b]
iterMap xs fn = uncurry fn <$> zip [0..] xs

symbolFor :: (ChessColor, ChessPiece) -> VTree msg
symbolFor colorPiece = text_ $ case colorPiece of
  (White, King  ) -> "♔"
  (White, Queen ) -> "♕"
  (White, Rook  ) -> "♖"
  (White, Bishop) -> "♗"
  (White, Knight) -> "♘"
  (White, Pawn  ) -> "♙"
  (Black, King  ) -> "♚"
  (Black, Queen ) -> "♛"
  (Black, Rook  ) -> "♜"
  (Black, Bishop) -> "♝"
  (Black, Knight) -> "♞"
  (Black, Pawn  ) -> "♟"


validMovesPawn :: ChessColor -> Bool -> Coords -> Coords -> Bool
validMovesPawn color capturing Coords{col=fromCol, row=fromRow} Coords{col=toCol, row=toRow} =
  any (\v -> toCol == fromCol + v) colRange && any (\v -> toRow == fromRow + v) rowRange
  where
    rowRange = case capturing of
      False -> case (color, fromRow) of
        (White, 6) -> [-1, -2]
        (White, _) -> [-1]
        (Black, 1) -> [1, 2]
        (Black, _) -> [1]
      True -> case color of
        White -> [-1]
        Black -> [1]

    colRange = case capturing of
      False -> [0]
      True  -> [-1, 1]

validMove :: Bool -> Coords -> Coords -> (ChessColor, ChessPiece) -> Bool
validMove capturing from to (color, piece) = case piece of
  Pawn -> validMovesPawn color capturing from to
  _    -> True
