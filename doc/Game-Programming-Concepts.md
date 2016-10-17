In the first chapters we used low-level libraries to create multimedia and
interact with hardware.

In the following chapters we are going to explore higher-level concepts of game
programming: patterns and solutions applied frequently in game programming.

Many ideas presented here come from imperative game programming and you
can find more about them in standard literature. Others will be motivated
by trying to address problems from a purely functional perspective.

# The big picture

At the beginning, games needs to set up the video configuration, load graphics
and audio, render some loading screen and then present a menu to configure settings and
games and load levels.

Games need to produce a pleasing visual and auditory effect, simulating
realistic physics and with perfect synchronisation between images and sounds,
wrapping you in a cloud that protects you from your daily problems, unless your
mum calls you, in which case it must let you pause the game, or even save it
and resume it later.  The game should communicate with other players and a
server, and possibly use Artificial Intelligence to control some game elements
and players.

You can play games using different kinds of inputs (mouse, keyboard) on any
device (console, PC, mobile), and they are expected to work seamlessly and
perform efficiently, without draining your device's battery and letting you
download the latest season of Breaking Bad in the background. Your games will
be featured on online listings open to comments, ratings and discussions, and
for every supporter and lover you will get two demanding teenagers with very low
tolerance for failure who will rate your app -10 because it did not
use their 3000 USD graphics card newest extension for particle effects.

As you read the above, you will probably understand that game development is
partly an art, that game success has a strong component of luck, and that the
complexity of the software, the expectations, and the required level of testing
and robustness, are all very high. And if this is your reaction, honestly, I
don't blame you:

<p align="center">
<img src="https://raw.githubusercontent.com/wiki/keera-studios/haskell-game-programming/fuckthisshit.jpg">
</p>

To attack this problem we need to divide it into subproblems and address each
one independently. In some cases the solution will be a library that we can
import from every project. In other cases, it will be a template that we need
to integrate in our code and adapt.  Finally, some solutions will not be
coded as templates, but rather as patterns or guidelines, and used by many
existing modules and definitions in our code. In general we will provide a
pattern that captures the core idea and a default implementation that
works well in many cases.

We are going to divide the problems above in the following sections:

* [Architectural patterns for increased abstraction,
  scalability and/or performance](Architectural Patterns).

  *Synchronous and asynchronous game loops, state machines, time transformations
  and pausing, Functional Reactive Programming.*

  <!-- Consult this chapter if you want to understand common patterns that appear
  > in nearly every game, where to read about possible solutions and how
  > to create them in a functional way. -->

<!--  * Game loops.
  * Asynchronicity (communication of different threads running different subcomponents).
  * Time transformations and alterations: pausing.
  * Functional Reactive Programming.
  * State machines.
    * Menus (configuring preferences, jumping to specific parts of the game,
    resuming the game).
-->
* [Multimedia, end points and IO](Multimedia).

  *Visual IO (rendering, animations, z-axis, 2.5 and 3D, scrolling, parallax scrolling,
  cameras, on-screen Heads-Up Displays, asset management), Audio (background music,
  effects, audio synchronization), Network and AI players.*

  <!-- Consult this chapter if you want to learn about how to accommodate rendering in your
  > program, how to implement different rendering techniques, how to transform raw on-screen
  > input into meaningful data, how to produce audio, and how to adapt your game
  > to enable playing over a network and controlling players with AI. -->

<!--
  * Visual IO.
    * Rendering.
      * PSEUDO-STARTED Animations.
      * STARTED Z-axis (in 2.5, parallax or 3D).
    * STARTED Cameras (projections, interaction).
    * STARTED Backgrounds and scrolling effects.
    * STARTED On-screen HUDS.
    * Asset management (locating assets, loading them, using them, freeing them,
      recovering from errors loading assets, showing info about loading progress).
  * Audio (background music, sound effects, audio synchronisation).
  * Network communication.
  * AI.
-->

* [[Physics]].

  *Physics and collisions, potential lacks and performance demands of different
  algorithms.*

  <!-- Consult this chapter if you want to understand more about the different ways
  > of implementing physics, detecting collisions, the potential lacks of each
  > way of implementing it and the possible performance cost. -->

* Robustness.

  *Error handling, testing, debugging, collecting player data, performance
  analysis.*

  <!-- Consult this chapter if you want to understand how to produce meaning errors
  > while the game is running, how to collect that data to process it later, and
  > how to debug games. -->

* Usability.

  *Saving or interrupting the game, Preferences, Reconfigurable input devices.*

  <!-- Consult this chapter if you want to understand how to add common usability
  > features, like saving and restarting, preferences, etc. -->

