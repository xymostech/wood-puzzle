module Main where

import Prelude hiding (flip)
import qualified Data.Set as Set
import qualified Data.List as List

data Side = L | R
  deriving (Show, Eq, Ord)

type CamPoint = (Int, Int, Side)
type Board = Set.Set CamPoint

mainBoard = Set.fromList $
  [ (0, 1, R), (0, 2, L), (0, 2, R), (0, 3, L), (0, 3, R), (0, 4, L), (0, 4, R), (0, 5, R), (0, 6, L), (0, 6, R)
  , (1, 0, R), (1, 1, L), (1, 1, R), (1, 2, L), (1, 2, R), (1, 3, L), (1, 3, R), (1, 4, L), (1, 4, R), (1, 5, L), (1, 5, R), (1, 6, L), (1, 6, R)
  , (2, 0, L), (2, 0, R), (2, 1, L), (2, 1, R), (2, 2, L), (2, 2, R), (2, 3, L), (2, 3, R), (2, 4, L), (2, 4, R), (2, 5, L), (2, 5, R), (2, 6, L)
  , (3, 0, L), (3, 0, R), (3, 1, L), (3, 1, R), (3, 2, L), (3, 2, R), (3, 3, L), (3, 3, R), (3, 4, L), (3, 4, R), (3, 5, L)
  , (4, 0, L), (4, 0, R), (4, 1, L), (4, 1, R), (4, 2, L), (4, 2, R), (4, 3, L), (4, 3, R), (4, 4, L)
  , (5, 0, L), (5, 0, R), (5, 1, L), (5, 1, R), (5, 2, L), (5, 2, R), (5, 3, L)
  ] :: Board

otherBoard = Set.fromList $
  [ (0, 5, R), (0, 6, L), (0, 6, R)
  , (1, 1, R), (1, 2, L), (1, 2, R), (1, 3, L), (1, 3, R), (1, 4, L), (1, 4, R), (1, 5, L), (1, 5, R), (1, 6, L), (1, 6, R)
  , (2, 0, R), (2, 1, L), (2, 1, R), (2, 2, L), (2, 2, R), (2, 3, L), (2, 3, R), (2, 4, L), (2, 4, R), (2, 5, L), (2, 5, R), (2, 6, L), (2, 6, R)
  , (3, 0, L), (3, 0, R), (3, 1, L), (3, 1, R), (3, 2, L), (3, 2, R), (3, 3, L), (3, 3, R), (3, 4, L), (3, 4, R), (3, 5, L), (3, 5, R), (3, 6, L)
  , (4, 0, L), (4, 0, R), (4, 1, L), (4, 1, R), (4, 2, L), (4, 2, R), (4, 3, L), (4, 3, R), (4, 4, L), (4, 4, R), (4, 5, L), (4, 5, R)
  , (5, 1, L), (5, 1, R), (5, 2, L), (5, 2, R), (5, 3, L), (5, 3, R), (5, 4, L), (5, 4, R), (5, 5, L)
  ] :: Board

type PieceOrientation = [CamPoint]
type Piece = [PieceOrientation]

rotate60 :: [CamPoint] -> [CamPoint]
rotate60 = map rotate60Point
  where
    rotate60Point (x, y, L) = (x + y - 1, -x, R)
    rotate60Point (x, y, R) = (x + y, -x, L)

flip :: [CamPoint] -> [CamPoint]
flip = map flipPoint
  where
    flipPoint (x, y, L) = (x, 1 - x - y, L)
    flipPoint (x, y, R) = (x, -x - y, R)

makePiece :: [CamPoint] -> Piece
makePiece orig =
  map (\func -> func orig)
  [ id
  , rotate60
  , rotate60 . rotate60
  , rotate60 . rotate60 . rotate60
  , rotate60 . rotate60 . rotate60 . rotate60
  , rotate60 . rotate60 . rotate60 . rotate60 . rotate60
  , flip
  , rotate60 . flip
  , rotate60 . rotate60 . flip
  , rotate60 . rotate60 . rotate60 . flip
  , rotate60 . rotate60 . rotate60 . rotate60 . flip
  , rotate60 . rotate60 . rotate60 . rotate60 . rotate60 . flip
  ]

piece1 = makePiece $
  [ (1, 0, L), (1, 2, L), (1, 2, R)
  , (0, 0, R), (0, 1, L), (0, 1, R), (0, 2, L), (0, 2, R)
  , (2, 2, L), (2, 2, R), (2, 3, L), (2, 3, R)
  ]

piece2 = makePiece $
  [ (0, 1, L), (0, 0, R)
  , (1, 0, L), (1, 0, R), (1, 3, R), (1, 4, L)
  , (2, 0, L), (2, 0, R), (2, 1, L), (2, 1, R), (2, 2, L), (2, 2, R), (2, 3, L)
  ]

modifiedPiece2 = makePiece $
  [ (0, 5, R), (0, 6, L)
  , (1, 0, R), (1, 1, L), (1, 1, R), (1, 2, L), (1, 2, R), (1, 3, L), (1, 3, R), (1, 4, L), (1, 4, R), (1, 5, L)
  , (2, 0, L)
  ]

piece3 = makePiece $
  [ (0, 0, R), (0, 1, L), (0, 1, R), (0, 2, L), (0, 2, R)
  , (1, 2, L), (1, 2, R), (1, 3, L)
  , (2, 2, L), (2, 2, R)
  , (3, 2, L), (3, 1, R), (3, 1, L)
  ]

piece4 = makePiece $
  [ (0, 0, R), (0, 1, L), (0, 1, R)
  , (1, 1, L), (1, 0, R), (1, 1, R)
  , (2, 1, L), (2, 1, R), (2, 2, L), (2, 2, R), (2, 3, L)
  ]

translatePiece :: PieceOrientation -> Int -> Int -> PieceOrientation
translatePiece piece x y = map (\(x', y', o) -> (x + x', y + y', o)) piece

type Placement = (Board, [PieceOrientation])

canAddPieceToPlacement :: Placement -> PieceOrientation -> Bool
canAddPieceToPlacement (board, pieces) newPiece =
  and (fitsInBoard : map (not . piecesOverlap) pieces)
  where
    newPieceSet = Set.fromList newPiece
    fitsInBoard = Set.isSubsetOf newPieceSet board
    piecesOverlap piece = (Set.intersection (Set.fromList piece) newPieceSet) /= Set.empty

addPieceToPlacement :: Placement -> PieceOrientation -> Placement
addPieceToPlacement (board, pieces) piece = (board, piece : pieces)

validPlacesOnBoard :: PieceOrientation -> Board -> [(Int, Int)]
validPlacesOnBoard piece board =
  List.nub $ lSpots ++ rSpots
  where
    isL (_, _, L) = True
    isL (_, _, R) = False

    isR (_, _, L) = False
    isR (_, _, R) = True

    maybeFirstL = List.find isL piece
    maybeFirstR = List.find isR piece

    posSubtract (pieceX, pieceY, _) (boardX, boardY, _) = (boardX - pieceX, boardY - pieceY)

    boardElems = Set.elems board

    lSpots = case maybeFirstL of
      Just spot -> map (posSubtract spot) $ filter isL boardElems
      Nothing -> []

    rSpots = case maybeFirstR of
      Just spot -> map (posSubtract spot) $ filter isR boardElems
      Nothing -> []

startPlacement = (mainBoard, [])
mainPieces = [piece1, piece2, piece3, piece4]

addPieceToBoard :: Placement -> Piece -> [Placement]
addPieceToBoard placement [] = []
addPieceToBoard (placement@(board, _)) (piece : pieces) =
  newPlacements ++ (addPieceToBoard placement pieces)
  where
    validPlaces = validPlacesOnBoard piece board
    possiblePieces = map (\(x, y) -> translatePiece piece x y) validPlaces
    newPlacements =
      foldl (\placements possiblePiece ->
        if canAddPieceToPlacement placement possiblePiece
        then (addPieceToPlacement placement possiblePiece) : placements
        else placements) [] possiblePieces

addPiecesToBoard :: Placement -> [Piece] -> [Placement]
addPiecesToBoard placement [] = [placement]
addPiecesToBoard placement (piece : pieces) = do
  newPlacement <- addPieceToBoard placement piece
  addPiecesToBoard newPlacement pieces

printRow :: [CamPoint] -> String -> Int -> Int -> Int -> Int -> IO ()
printRow shape char minRow minCol maxCol row = do
  mapM_ putStr $ replicate (2 * (row - minRow) + 2) " "
  mapM_ putStr $ replicate (maxCol - minCol + 1) "x---"
  putStrLn "x"
  mapM_ putStr $ replicate (2 * (row - minRow) + 1) " "
  mapM_ (printPos row) [minCol..maxCol]
  putStrLn "/"
  where
    printPos x y = do
      putStr (if elem (x, y, L) shape
                then "/" ++ char
                else "/ ")
      putStr (if elem (x, y, R) shape
                then "\\" ++ char
                else "\\ ")

allPoints :: Placement -> [CamPoint]
allPoints (board, pieces) =
  (Set.elems board) ++ concat pieces

printPlacement :: Placement -> IO ()
printPlacement (placement@(_, pieces)) =
  mapM_ (printRow (pieces !! 0) "1" minRow minCol maxCol) $ reverse [minRow..maxRow]
  where
    points = allPoints placement

    rowOf (x, _, _) = x
    colOf (_, y, _) = y

    maxRow = List.maximum $ map rowOf points
    minRow = List.minimum $ map rowOf points

    maxCol = List.maximum $ map colOf points
    minCol = List.minimum $ map colOf points

printShape :: [CamPoint] -> IO ()
printShape shape =
  mapM_ (printRow shape "@" minRow minCol maxCol) $ reverse [minRow..maxRow]
  where
    rowOf (x, _, _) = x
    colOf (_, y, _) = y

    maxRow = List.maximum $ map rowOf shape
    minRow = List.minimum $ map rowOf shape

    maxCol = List.maximum $ map colOf shape
    minCol = List.minimum $ map colOf shape

main :: IO ()
main = do
  putStrLn "Solutions:"
  print $ addPiecesToBoard startPlacement mainPieces
