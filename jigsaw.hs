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