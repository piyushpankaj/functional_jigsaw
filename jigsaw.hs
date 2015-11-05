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