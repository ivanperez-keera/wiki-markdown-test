Wii remotes, also known as WiiMotes, are Nintendo's Wii console remote controllers.

<p align="center">
<img src="https://raw.githubusercontent.com/wiki/keera-studios/haskell-game-programming/Hardware/wiimote.png">
</p>

Wiimotes are wireless devices that communicate over bluetooth.
They feature multiple push buttons, accelerometers, and an infrared
camera.  Wiimotes can be used in combination with the wii bar (a bar with 4 IR
leds that sits on top of your TV) to provide a unique point of reference.  This
lets your game know which part of the screen the wiimote it pointing towards, and
can be used to **turn the wiimote into a gun and play first-person shooting
games**. Wiimotes have an extension port that can be used to connect additional
devices, like nunchakus (a small extension with a joystick-like controller and
a fire button; see image below) and more precise accelerometers. 

<p align="center">
<img src="https://raw.githubusercontent.com/wiki/keera-studios/haskell-game-programming/Hardware/nunchaku.jpg">
</p>

:star: **Cool stuff!**

> People have been doing all kinds of hacks with wiimotes, including
> <a href="https://www.youtube.com/watch?v=QgKCrGvShZs" target="_blank">low-cost
> whiteboards, touch screens, head-tracking and 3D screens</a>, slide presenters,
> connecting them to Android/iPhones, and
> <a href="https://www.youtube.com/watch?v=Bhte58Fqw0A"
> target="_blank">extremely amazing games</a>. Head over to
> <a href="https://www.youtube.com/results?search_query=cool+wiimote+projects" target="_blank">
> youtube</a> for a few ideas for your next wiimote project.

On Linux, the library CWiid provides comprehensive support for many Wii
devices[<sup>**1**</sup>](#footnotes).
[Hcwiid](https://github.com/ivanperez-keera/hcwiid) is a Haskell library that
provides bindings for CWiid. While the capabilities of Hcwiid are behind those
of CWiid or its newer fork, the API provided is enought to access button,
infrared and accelerometer information.

In this section we will learn how to:
* Create a Haskell program that connects to a Wiimote.
* Poll the wiimote regularly for the information we are interested in.
* Present that information on the screen.
* Process the information to obtain more sophisticated and useful data.

At the end of this chapter you should be able to use a Wiimote to control the
games and examples provided in others lessons (eg. [SDL1](SDL » SDL1)), and to create a
Haskell application that uses a Wiimote to interact with it. In chapter
[[Examples]] we will see how to create a small game and enable wiimote support.

:exclamation: **Portability**

> Currently cwiid is only available for Linux. If your game uses wiimotes,
> consider allowing an alternative input method (if possible), and disabling
> all wii-related code on Windows and MacOSX. See
> <a href="https://github.com/ivanperez-keera/haskanoid" target="_blank">Haskanoid</a> for an example of
> how to use the C preprocessor and cabal flags to implement conditional wiimote
> support for different OSs.

# Baby steps

Connecting to the wiimote the first time could not be simpler.

``` haskell
import System.CWiid

main :: IO ()
main = do
  putStrLn "Initializing WiiMote. Please press 1+2 to connect."
  wm <- cwiidOpen
  case wm of
    Just _aWiimote -> putStrLn "Connected"
    Nothing        -> putStrLn "Could not connect"
```

:key: **Compilation**

> GHC's Runtime System uses a POSIX signal needed by cwiid to interact with the
> wiimote. To make wiimotes work on linux, you need to disable the RTS clock
> completely[<sup>**2**</sup>](#footnotes). Compile your program with the flag
> `-rtsopts` and pass the runtime option `-V0` to your program during
> execution, like so:
>
> ``` bash
> $ cabal exec -- ghc Main.hs -rtsopts
> $ ./Main +RTS -V0
> Initializing WiiMote. Please press 1+2 to connect.
> ```

If you run the program above, you should see the message "Press 1+2 to
connect". There is a standard delay of a few seconds during which you should
press the buttons 1 and 2 on the wiimote simultaneously and it will enter
discovery mode and connect to your computer. If successful, you should receive
a handle to your wiimote in the form of a `CWiidWiimote` value.

## Connect, poll buttons

Wiimotes run on batteries. The more data that is polled from the wiimote, the
faster the batteries will be drained. To make them last longer, wiimotes
require that you enable and disable different input subdevices before you can
use them. You do that with the function `cwiidSetRptMode` with a numeric argument:
the sum of the independent modes that are enabled. Head over to the [cwiid
documentation](https://github.com/abstrakraft/cwiid/blob/master/libcwiid/cwiid.h#L34)
for a detailed list of the modes available and their numeric codes.

Our next program is going to enable Buttons, IR and Accelerometers on a
wiimote, and poll and report the state of the button A every 100 milliseconds.

``` haskell
import Control.Concurrent
import Control.Monad
import System.CWiid

main :: IO ()
main = do
  putStrLn "Initializing WiiMote. Please press 1+2 to connect."
  wm <- cwiidOpen
  case wm of
    Nothing      -> putStrLn "Could not connect"
    Just wiimote -> do 
      putStrLn "Connected"

      -- NEW

      -- Enable different sensors (15 = 1 + 2 + 4 + 8):
      -- 1 for status, 2 for buttons, 4 for accelerometers, and 8 for IR.
      cwiidSetRptMode wiimote 15

      -- "game" loop
      forever $ do

        -- Input: Poll new state
        allButtons <- cwiidGetBtnState wiimote
        let btnAPushed = cwiidIsBtnPushed allButtons cwiidBtnA

        -- Rendering: Report
        let msg = if btnAPushed then "Down" else "Up"
        putStrLn msg

        -- Introduce a small delay
        threadDelay 100000
```

Try this example. You should see a continuous report stream that changes
to "Down" whenever the button is depressed.

### Summary

* Wiimotes contain multiple inputs, including discrete ones (on/off) ones and
  more analog-like ones (accelerometers, IR camera).

* To interact with a wiimote, one needs to connect to it, set the input reception
  mode, and then poll the state.

* Unlike in SDL, wiimotes do not report input events. They need to be polled
  regularly. Unless polled frequently enough, one might miss intermediate
  states (eg. very fast clicks). In the previous example, if you click the button
  A very quickly, you may see that some button presses are missed.

## Homework

* Head over to the [haddock documentation for
  hcwiid](https://hackage.haskell.org/package/hcwiid-0.0.5/docs/System-CWiid.html)
  and review other buttons available. Modify the program above to get the state of
  other buttons.

* If you did the SDL input lesson, modify the second version of [this
  program](SDL-»-SDL1-»-Input#keyboard-events) to use the wiimote instead of a
  keyboard. (Note: you could create an intermediate layer that turns wiimote
  state changes into [SDL user
  events](http://hackage.haskell.org/package/SDL-0.6.5.1/docs/Graphics-UI-SDL-Events.html)
  and keep a uniform game interface. If you end up doing something so
  sophisticated, please send us a link. However, the purpose of this exercise is
  just to modify the program to use a Wiimote as the input device, in the
  simplest way possible, that is, by writing a new `updateController` function
  that polls the wiimote and adjusts the controller to reflect the wiimote's
  state.)

# Accelerometers

# Infrared

The wiimote has an infrared camera and is capable to detecting and tracking up
to 4 sources of IR with 1024x768 resolution (the hardware is 128x96 monochrome,
but uses subpixel analysis to detect small variations and provide greater
precision).

Wii consoles come with a device commonly known as the wii bar, which contains
infrared leds positioned on the ends of the bar, facing the user. The IR
sources, sitting on top of the monitor, provide a way to know where the wiimote is
pointing to.

:star: **Limits of one wii bar and advanced applications**

> To know what the user is pointing towards, one needs to know her location
> with respect to the TV. Some games assume your location does not change.
> You can also determine absolute location and distance by including a third
> IR source, provided that you know the positions and distances between
> each IR led and not all three are co-aligned. One easy way to do so is to
> include a second IR bar and carry out a perspective transformation. 

The IR data is provided in an list. Note that, even if a point is temporarily
lost or out of screen boundaries, the wiimote will still hold data for it in an
attempt to track it and will successfully detect it when it appears in its
visual field again.

## Basic IR data

Our first example will show IR data as provided by the wiimote, without any
manipulation. This will help you understand how the low-level API works,
and how it actually tracks points.

``` haskell
import Control.Monad
import Graphics.UI.SDL            as SDL
import Graphics.UI.SDL.Primitives as SDL
import System.CWiid

main :: IO ()
main = do
  SDL.init [InitVideo]

  screen <- SDL.setVideoMode width height 32 [SWSurface]

  putStrLn "Initializing WiiMote. Please press 1+2 to connect."
  wm <- cwiidOpen
  case wm of
    Nothing      -> putStrLn "Could not connect"
    Just wiimote -> do
      putStrLn "Connected"

      -- NEW

      -- Enable different sensors (15 = 1 + 2 + 4 + 8):
      -- 1 for status, 2 for buttons, 4 for accelerometers, and 8 for IR.
      cwiidSetRptMode wiimote 15

      -- "game" loop
      forever $ do
        irs   <- cwiidGetIR wiimote

        -- Render
        let format = surfaceGetPixelFormat screen
        white <- mapRGB format 255 255 255
        fillRect screen Nothing white

        forM_ irs $ \ir -> do
          let x = fromIntegral $ cwiidIRSrcPosX ir
              y = fromIntegral $ cwiidIRSrcPosY ir
          circle screen x y 30 (Pixel 0x0000FFFF)    

        SDL.flip screen

width :: Num a => a
width = 1024

height :: Num a => a
height = 768
```

The most important bits are in the last 10 lines of `main`, inside the game
loop, which collected the data with `cwiidGetIR` and, for each IR point,
obtains the coordinates and uses them to show a circle in its place on the
screen.

### Homework

* The Hcwiid library also provides a "valid" boolean field for each IR source,
which determines whether it is being actually seen or just "remembered", and
an `Int` size field. Modify the example above to paint the circles in sizes
proportional to the IR source's size and colors that reflect their validity.

## Reflecting and adjusting the input data

If you've tried the example above, you'll know the image is reflected: when you
move right, the leds move left. That's because you are painting the image as
seen by the camera.

To use the wiimote as a game controller, you need to know what the user
is pointing towards. There is a more general way of approaching this
problem using high-school algebra, but we are going to explore
the simplest solution first to try and understand the transformations
involved.

We need to transform multiple IR sources in camera coordinates into
one unique IR source in screen coordinates:

* The wiimote provides multiple IR sources.
* The IR source positions are horizontally reflected.
* The mid point for the camera is the highest point on our screen (if
  the wii bar sits on top of the monitor).
* The IR positions are in a 1024x768 space, but your game may work with a
  different resolution.

Addressing the first point can be done by calculating the mid point between of
the IR sources. Addressing the second and third can be done by a composition of
horizontal reflection (by subtracting the X coordinate from the camera image's
width, a fixed 1024), and vertical translation (subtracting 384, half of 768,
from the Y coordinate).

The fourth problem can be done by transforming the coordinates into relative
coordinates (eg. 50% left, 32% up) and then multiplying them by the screen
size. Note that this is not the best solution in the general case, but it will
guarantee that, if you work at a resolution higher than 1024x768 (the
wiimote's), you can at least reach the whole screen.

Let's take a look at a small modification of the previous example
that carries out these calculations.

``` haskell
import Control.Concurrent
import Control.Monad
import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Primitives as SDL
import System.CWiid
import Data.IORef

main :: IO ()
main = do
  SDL.init [InitVideo]

  screen <- SDL.setVideoMode width height 32 [SWSurface]

  putStrLn "Initializing WiiMote. Please press 1+2 to connect."
  wm <- cwiidOpen
  case wm of
    Nothing      -> putStrLn "Could not connect"
    Just wiimote -> do 
      putStrLn "Connected"

      -- NEW
      -- Enable different sensors (15 = 1 + 2 + 4 + 8):
      -- 1 for status, 2 for buttons, 4 for accelerometers, and 8 for IR.
      cwiidSetRptMode wiimote 15

      -- "game" loop
      forever $ do
        irs   <- cwiidGetIR wiimote
        
        -- Obtain positions of leds 1 and 2 (with a normal wii bar, those
        -- will be the ones we use).
        let led1 = irs!!0
            led2 = irs!!1
        
        -- Calculate mid point between sensor bar leds
        let posX = ((cwiidIRSrcPosX led1) + (cwiidIRSrcPosX led2)) `div` 2
            posY = ((cwiidIRSrcPosY led1) + (cwiidIRSrcPosY led2)) `div` 2
        
        -- Calculate proportional coordinates
        let propX = fromIntegral (1024 - posX) / 1024.0
            propY = fromIntegral (max 0 (posY - 384)) / 384.0
        
        -- Calculate game area coordinates
        let finX  = width  * propX
            finY  = height * propY
        
        -- Render
        let format = surfaceGetPixelFormat screen
        white <- mapRGB format 255 255 255
        fillRect screen Nothing white

        when (length irs > 1) $ void $ do
          -- Type transformation Int -> Int16
          let x = round finX
              y = round finY
          filledCircle screen x y 30 (Pixel 0xFF0000FF)

        SDL.flip screen
 
width :: Num a => a
width = 1400

height :: Num a => a
height = 1050
```

The code addresses the fact that there are multiple sources in the definition
of `posX` and `posY`, by taking the average of the first two leds.

The second and third point are addressed with the calculations:
* 1024 - posX, the reflected X coordinate.
* max 0 (posY - 384), the rebased Y coordinate, capped at 384 (the bottom
half of the wiimote camera).

The subsequent divisions by the screen resolution (1024 x 384, since we are
only considering one half of the camera image) and multiplications by game size
(`finX`, `finY`) transform from camera units into screen units. What remains
is then to paint the circles on the screen.

## Shooting at the screen

TO BE COMPLETED: This section should explain how to apply a perspective
transform to turn wiimote coordinates with 4 points after calibration
into precise screen coordinates.

Could be used for shooting games, or to create a wiimote-whiteboard program.

END OF TO BE COMPLETED

# Installation

To write Haskell programs with Wiimote support, you need a linux computer,
the cwiid development library and hcwiid. On Ubuntu, all you need to do is

``` bash
$ sudo apt-get install libcwiid-dev
$ cabal install hcwidd # Better to do it in a sandbox
```

# Contribute!

* As you can see, the hcwiid bindings are very incomplete. If you have a wiimote,
please improve wii support for Haskell and send a pull request to the 
[github project](https://github.com/ivanperez-keera/hcwiid).

* Wiimote support is currently only available on linux. Consider writing a
  small version of the bindings for a cross-platform wii library. See [this
wiki](http://wiibrew.org/wiki/Wiimote/Library) for a list of existing
libraries.

# Footnotes

1. From the [Wiibrew project](http://wiibrew.org/wiki/Wiimote/Library#Linux_CWiid): *"While
the original library ceased to be developed around 2010, in 2011 it has been forked
by the Linux Laptop Orchestra (L2Ork) project (http://l2ork.music.vt.edu) and
has since been upgraded to provide comprehensive support for all Nintendo brand
Wii controllers, including Wiimote, Nunchuk, MotionPlus, Classic, Wii Fit
board, and the newer version of the Wiimote with MotionPlus Inside."*

2. From [GHG's documentation](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/runtime-control.html):
*"RTS clock completely, and has the effect of disabling timers that depend on
it: the context switch timer and the heap profiling timer. Context switches
will still happen, but deterministically and at a rate much faster than normal.
Disabling the interval timer is useful for debugging, because it eliminates a
source of non-determinism at runtime."*

# Credits

* Wiimote image was taken from [this Wikipedia
  page](https://en.wikipedia.org/wiki/Wii_Remote#/media/File:Wii_Remote_Image.jpg).

* The nunchaku image was taken from wikipedia, and that one was taken from Flickr.
The original image's author Marcin Chady. Distributed under [CC BY
2.0](http://creativecommons.org/licenses/by/2.0) licence.
