import Data.IORef
import FRP.Yampa                  as Yampa
import Graphics.UI.SDL            as SDL
import Graphics.UI.SDL.Primitives as SDL

width :: Num a => a
width  = 640
height :: Num a => a
height = 480

main = do
  -- Initialise SDL
  SDL.init [InitVideo]
  setVideoMode width height 16 [SWSurface]

  timeRef <- newIORef (0 :: Int)
  reactimate (return ())
             (\_ -> do
                dtSecs <- yampaSDLTimeSense timeRef
                return (dtSecs, Nothing))
             (\_ e -> display e >> return False)
             (inCircles >>> arr (zoomAt (320, 240) 10))

inCircles :: SF () (Double, Double)
inCircles = localTime >>>
  (arr $ \time -> ( width  / 2 + cos time * radius
                  , height / 2 + sin time * radius)
  )
 where radius = 30

zoomAt :: (Double, Double) -> Double
       -> (Double, Double) -> (Double, Double)
zoomAt (centerX, centerY) factor (x, y) =
  ( centerX + (x - centerX) * factor
  , centerY + (y - centerY) * factor)

display :: (Double, Double) -> IO()
display (x, y) = do 
  -- Obtain surface
  screen <- getVideoSurface

  -- Paint screen green
  let format = surfaceGetPixelFormat screen
  green <- mapRGB format 0 0xFF 0
  fillRect screen Nothing green

  let side = 10
  -- Paint small red square, at an angle 'angle' with respect to the center
      x'   = round x
      y'   = round y
  filledCircle screen x' y' side (Pixel 0xFF0000FF)

  -- Double buffering
  SDL.flip screen

-- | Updates the time in an IO Ref and returns the time difference
updateTime :: IORef Int -> Int -> IO Int
updateTime timeRef newTime = do
  previousTime <- readIORef timeRef
  writeIORef timeRef newTime
  return (newTime - previousTime)

yampaSDLTimeSense :: IORef Int -> IO Yampa.DTime
yampaSDLTimeSense timeRef = do
  -- Get time passed since SDL init
  newTime <- fmap fromIntegral SDL.getTicks

  -- Obtain time difference
  dt <- updateTime timeRef newTime
  let dtSecs = fromIntegral dt / 1000
  return dtSecs
