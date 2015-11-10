import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Extent (Coord)
import Data.Function (on)
import Data.List (sortBy)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Maybe
import Data.Tree
import System.Random
import Debug.Trace
import Graphics.Gloss.Game hiding (play)
import Codec.BMP
import qualified Data.ByteString as B
import Control.Applicative
import Control.Monad
import Control.Monad.ST
import Control.Monad.Random
import Data.Array.ST
import GHC.Arr

derrange :: RandomGen g => [a] -> Rand g [a]
derrange xs = do
    let l = length xs
    rands <- take l `fmap` getRandomRs (0, l-1)
    let ar = runSTArray $ do
        ar <- thawSTArray $ listArray (0, l-1) xs
        forM_ (zip [0..(l-1)] rands) $ \(i, j) -> do
            vi <- readSTArray ar i
            vj <- readSTArray ar j
            writeSTArray ar j vi
            writeSTArray ar i vj
        return ar
    return (elems ar)

genShuffle :: [(Side, (Int, Int), Int, Int)] -> [Int] -> Int -> [(Side, (Int, Int), Int, Int)]
genShuffle xs [] _ = []
genShuffle xs (y:ys) p = do
  let (r, (a1, b1), c , d) = (xs !! p)
  let (r1, (a2, b2), c1, d1) = (xs !! y)
  (r, (a2, b2), c, d) : genShuffle xs ys (p + 1)

--shuffle :: [(Side, (Int, Int), Int, Int)] -> RandomGen -> [(Side, (Int, Int), Int, Int)]
shuffle [] r = []
shuffle xs r = do
  let len = length xs
  --let t = permute len
  let idx = [0..len - 1]
  let shuffle_idx = evalRand (derrange idx) r
  genShuffle xs shuffle_idx 0

boardSize :: Int
boardSize = 4

searchDepth :: Int
searchDepth = 3


hexRadius :: Float
hexRadius = 1

hexWidth :: Float
hexWidth = (sqrt 3) / 2

hexColor :: Coord -> Color
hexColor = cycleColors [ (85, 106, 47), (94, 117, 52), (77, 96, 43) ]

img_height :: Int
img_height = 400

img_width :: Int
img_width = 400

num_split :: Int
num_split = 4

boardCoords :: [Coord]
boardCoords = [ (i, j) |
    i <- [-boardSize..boardSize],
    j <- [-boardSize..boardSize],
    j - i <= boardSize,
    i - j <= boardSize ]

data Side = Red | Blue deriving Eq
type Piece = (Side, Coord, Int, Int)


--type Pair = (Int, Int)
type Pieces = Map Coord Coord
type Board = ([Piece],Pieces,Pieces)

sideColor :: Side -> Color
sideColor side = case side of
    Red -> makeColor8 255 190 180 255
    Blue -> makeColor8 180 190 255 255

cycleColors :: [ (Int, Int, Int) ] -> Coord -> Color
cycleColors rgbs (i, j) = colors!!((i + j) `mod` (length colors))
    where colors = map (\(r, g, b) -> makeColor8 r g b 255) rgbs

hex :: Picture
hex = Polygon [ (-x,-y), (x,-y), (x,y), (-x,y) ]
    where x = 1
          y = 1

data HeldPiece = HeldPiece {
    hpCoordOriginal :: Coord, -- piece's original position on the board
    hpMouseOffset :: Point -- mouse's offset from piece when it was picked up
    }

data Game = Game {
    board :: Board,
    mousePos :: Point,
    heldPiece :: Maybe Coord,
    mainGame :: Bool,
    randomState :: StdGen,
    endState :: Maybe Bool
    }


low :: Int
low = 1

high :: Int
high = 5

main :: IO ()
main = do
    r <- getStdGen
    let (tempimg,_) = randomR (low, high) r
    splitimg ("ImagesInput/new" ++ show tempimg ++ ".bmp") num_split (-num_split-1)
    --splitimg ("new1.bmp") num_split (-num_split-1)
    splitimg "white.bmp" num_split 1
    play
      displayMode
      backgroundColor
      framesPerSecond
      (newGame r)
      drawGame
      handleInputEvent
      stepTime
    where displayMode = InWindow "Jigsaw" (sizeX, sizeY) (5, 5)
          backgroundColor = makeColor8 170 180 145 255
          framesPerSecond = 100
          sizeX = 1200
          sizeY = 700

--CHUNK
chunk bs len1 a b = B.index bs ((a*len1)+b)

make_chunks :: B.ByteString -> Int -> Int -> Int -> [(Int, Int)] -> Int -> IO()
make_chunks bs len2 part_width part_height [] _ = putStrLn ""
make_chunks bs len2 part_width part_height (x:xs) y = do
  let (i, j) = x
  let a = j * part_width * 4
  let b = i * part_height
  let c = a + part_width * 4 - 1
  let d = b + part_height - 1
  
  let part = chunk bs len2 <$> [b..d] <*> [a..c]
  let part1 = B.pack part
  let sample_img = packRGBA32ToBMP part_width part_height part1

  writeBMP ("images/sample_img_" ++ show (j + y) ++ "_" ++ show (i - 1) ++ ".bmp") sample_img
  make_chunks bs len2 part_width part_height xs y


--SPLIT
splitimg:: FilePath -> Int -> Int -> IO() 
splitimg filename num_split y = do
  Right img <- readBMP filename
  let (width, height) = bmpDimensions img
  let bs = unpackBMPToRGBA32 img
  let len = B.length bs
  let len1 = (fromIntegral len )/ (fromIntegral height)
  let len2 = round len1
  let part_width = round ((fromIntegral width)/(fromIntegral num_split)) --128
  let part_height = round ((fromIntegral height)/(fromIntegral num_split)) --128
  let lst = [0..num_split-1]
  let x = [(a, b) | a <- lst, b <- lst]
  make_chunks bs len2 part_width part_height x y

  

stepTime :: Float -> Game -> Game
stepTime _ = id

--newGame ::  Game
newGame r = Game {
    board = initialBoard r,
    mousePos = (1000, 0),
    heldPiece = Nothing,
    mainGame = False,
  
initiate board = Map.fromList (redPieces ++ bluePieces) where
    lst = [0..num_split - 1]
    pieces = [(i, j) | i <- lst, j <- lst]
    redPieces  = [( (i + 1, j - 1), (i + 1, j - 1) ) | (i, j) <- pieces]
    bluePieces = [(pos, (x,y)) | (_,pos,x,y) <- board ]

drawGame :: Game -> Picture
drawGame g@(Game board mousePos heldPiece mainGame randomState _) = Pictures [
    drawBackground,
    drawPieces board,
    mouseHighlight g,
    showEndGame g]

showEndGame :: Game -> Picture
showEndGame g@(Game _ _ _ _ _ endState) = do
  if (isNothing endState)
    then Blank
    else
      if (fromJust endState)
        then pieceKindPicture 0 0
        else scale 2 2 (pieceKindPicture 0 1)

drawBackground :: Picture
drawBackground = translateFloat (2.5 , 0.5) (bmp "white.bmp")

drawPieces :: Board -> Picture
drawPieces (p,_,_) = Pictures $ map drawPiece p 

drawPiece :: Piece -> Picture
drawPiece (side, pos, i , j) =
    Color (sideColor side) $ translateCoord pos $ (pieceKindPicture i j)

--pieceKindPicture :: PieceKind -> Picture
pieceKindPicture i j = bmp ("images/sample_img_" ++ show i ++ "_" ++ show j ++ ".bmp")
  --trace (" "++ show(i) ++ " "++ show(j)  ) (bmp ("images/sample_img_" ++ show i ++ "_" ++ show j ++ ".bmp"))

translateFloat :: (Float, Float) -> Picture -> Picture
translateFloat = (uncurry Translate) . floatToGrid

floatToGrid :: (Float, Float) -> Point
floatToGrid (i, j) = (x, y)
    where x = (i * fromIntegral img_width) / fromIntegral (num_split)
          y = (j * fromIntegral img_height) / fromIntegral ( num_split)

translateCoord :: Coord -> Picture -> Picture
translateCoord = (uncurry Translate) . screenFromGrid

screenFromGrid :: Coord -> Point
screenFromGrid (i, j) = (x, y)
    where x = fromIntegral (i * img_width) / fromIntegral (num_split)
          y = fromIntegral (j * img_height) / fromIntegral ( num_split)

rQuadrant :: Float -> Float -> Bool
rQuadrant x y =
  do
    let part_width = fromIntegral img_width / fromIntegral num_split
    let part_height = fromIntegral img_height / fromIntegral num_split
    let startw = (fromIntegral img_width) / (fromIntegral num_split) - part_width / 2
    let starth = (fromIntegral img_height) / (fromIntegral num_split) + part_height / 2
    if x >= startw  && x <= (startw + fromIntegral img_width) && y >= (-1 * starth) && y <= (-1 * starth + fromIntegral img_height)
      then True
    else False

lQuadrant :: Float -> Float -> Bool
lQuadrant x y =
  do
    let part_width = fromIntegral img_width / fromIntegral num_split
    let part_height = fromIntegral img_height / fromIntegral num_split
    let startw = (fromIntegral ((-1 * num_split - 1) * img_width)) / (fromIntegral num_split) - part_width / 2
    let starth = (fromIntegral img_height) / (fromIntegral num_split) + part_height / 2
    if x >= startw  && x <= (startw + fromIntegral img_width) && y >= (-1 * starth) && y <= (-1 * starth + fromIntegral img_height)
      then True
    else False

detect :: (Float, Float) -> Maybe Point
detect (x, y) = 
  do
    let tmp = (fromIntegral img_width) / (fromIntegral num_split)
    let p1 = round ((x - tmp) / tmp)
    let p2 = round ((x + fromIntegral (num_split + 1) * tmp) / tmp)
    let q = round ((y + tmp) / tmp)
    if (rQuadrant x y) 
      then Just (screenFromGrid (p1 + 1, q - 1))
      else if (lQuadrant x y)
        then Just (screenFromGrid (-num_split - 1 + p2, q - 1))
        else
          Nothing
mouseHighlight :: Game -> Picture
mouseHighlight g@(Game board mousePos heldPiece mainGame randomState _) = case detect mousePos of
  Just x -> mouseHighlight1 x (fromIntegral img_width / fromIntegral num_split) (fromIntegral img_height / fromIntegral num_split) 
  Nothing -> Blank


mouseHighlight1 x part_width part_height= Color white $ hexHighlight x part_width part_height

--hexHighlight :: Coord -> Picture
hexHighlight (x,y) part_width part_height = do
  let p = x - part_width / fromIntegral 2
  let q = y - part_height / fromIntegral 2
  lineLoop [ (p, q), (p + part_width, q), (p + part_width, q + part_height), (p, q + part_height) ]


handleInputEvent :: Event -> Game -> Game
handleInputEvent e g = case e of
    --(EventKey (Char 'e') Down _ pos) -> (onMouseMove pos g) { mousePos = (1,1000) }
    (EventKey (Char 'e') Down _ pos) -> endGame (onMouseMove pos g)
    (EventKey (MouseButton LeftButton) Down _ pos) -> firstMouseDown (onMouseMove pos g)
    _ -> g

onMouseMove :: Point -> Game -> Game
onMouseMove p g = g { mousePos = p }