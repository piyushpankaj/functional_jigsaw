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
