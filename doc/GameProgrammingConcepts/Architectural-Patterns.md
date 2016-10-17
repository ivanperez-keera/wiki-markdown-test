
# Architectural patterns

## The game loop

Games are interactive visualizations and, as such, they need to process user
input and produce continuous video and/or audio. 

One possible way of structuring them, akin to mathematical functions and
command line invocations, is to repeatedly detect the input, process it (based
on the program's state at the time), present the output after that. This
results in a sense-process-render loop often called the *game loop*.

:star: **Past, present and future**

> Game loops have been used for many years, and they continue to be
> used to this day. See, for example, REFERENCE and REFERENCE for
> an old example and a new one.

Input and output, in the real world, occur in continuous time. Yet your game
gathers input at discrete points, and renders a little bit afterwards. A
sequence of actions as required by a game loop creates a small delay between
some effects and others, and also enforces a certain priority of some over
others. For instance, in your game you might advance the physics (movement of
elements based on velocities and accelerations) before or after you process the
user's input. If you do it afterwards, then the physics at a given time will be
independent of *all the input* since the last time the game was rendered until
now. You might also *run the players actions before or after the artificial
intelligence* that controls the opponents, in which case you are deciding on an
order that might change the game's outcome completely. Depending on you game
pace, this may or may not make a noticeable difference.

In its most basic form, a game loop could look as follows (REF TO ACTUAL GAME
THAT USES THIS):

``` haskell
main :: IO ()
main = do
  initialise
  
  gameLoop initialState

gameLoop :: GameState -> IO ()
gameLoop oldState = do
  input      <- senseInput
  timePassed <- senseTime

  let newState = progress oldState gameLoop timePassed

  render newState

  -- Continue until some gameFinished :: GameState -> Bool
  -- function evaluates to False.
  if (gameFinished newState)  -- Could use Control.Monad.when/unless.
     then return ()
     else gameLoop newState
```

There is a lot that we can deduce from this schematic code already. For
instance, while sensing input, time and rendering are effectful operations (the
type of `senseInput` might be in the example above `IO Input` for some type
`Input`), the actual game logic (and physics) is completely pure. This means we
can define the game state as an abstract datatype and use pure functional
programming to implement it, with the advantages that provides (simpler code,
equational reasoning, compiler optimisations, higher-level descriptions,
declarativeness, parallelism, testing).

:star: **FRP**

> The FRP implementation Yampa contains an execution function called
> `reactimate` that implements a game loop parameterised over an input
> provider, a processing function and an output consumer. The function runs as
> fast as possible by default, although the sensor or the consumer can choose
> to introduce artificial delays to lower CPU consumption or try to approximate
> a specific number of FPS.

This continuous game loop architecture is not without caveats, and is nowadays
only used for testing or in very simple games with low performance demands. The
main reason is that all the functions are synchronized: we sense **once**, we
progress (logic and physics) **once**, we render **once**, and repeat.

Current screens render at 60 frames per second, while some physics simulations
need to be executed at about 100 frames per second to look realistic, and some
input devices produce data at only 25 frames per second (eg. a webcam). A fixed
loop like the one above could lower the global performance of your game, while
unnecessarily increasing the frequency with which some functions get executed.

There are multiple ways of addressing this problem, from executing some
functions more than once per cycle to running them asynchronously. Introducing
asynchronicity (threads) also increases game complexity, as it requires
handling shared memory, signaling, delays, blocks and deadlocks.

:star: **OpenGL and HGL: unless you have something better to do...**

> OpenGL includes a function that can execute one game loop iteration
> when the thread is idle (not rendering).

NOTE: So what?

Also, while some input devices, like wiimotes, need to be polled regularly, others
like kinects (and widget systems like Gtk, which also produce input) work using
callbacks. Callbacks are asynchronous functions that get executed in response to
certain events.

Therefore, an architecture based on polling may not adapt well to devices
that should not or can not be polled regularly.

NOTE: Introduce async structure or point to chapter on async games.

## State machines

During gameplay, your game transitions over several states. At the beginning,
it will probably show a *loading* splash screen to let users know there should
be a delay while assets are being loaded. Then it might show a menu and let
users select and configure devices, quality and game settings select a level
and start playing.

During gameplay, your game can be loading a level, playing, paused, showing
won/game over screens, showing a menu (save, quit, etc.) or depending on the
kind of game, waiting for you or other players, to make a move.  It is easy to
see then that your game is, in some respect, a *state machine*.

Simple state machines can be described by a diagram of states (circles) with
directed links between them (arrows). Arrows can be labeled with inputs, or the
events that would make the machine transition from one state to the next.  An
arrow from a state x to a state y labeled '0' could be interpreted as "if the
user enters '0' while in state 'x', the game can/will transition to state 'y'".
The initial state can be marked with an arrow pointing towards it, and final
states are often circled twice.

:star: **Representation**

> There are other ways of representing state machines. The machine's output
> can also be included in the diagram. See REFERENCES for alternative
> representations and a description of other areas that use this abstraction.

Apart from the idea that your game switches from one state to the next,
it is important to understand that both the input and the output may be
completely different between one state and another.

Consider, for example, a menu. An abstract description of the possible
input actions (without depending on any specific input device or keyboard
layout) might be termed 'up', 'down', 'activate', 'back' and so forth.
During another game stage, your game might need continuous information
from the accelerometers of a wiimote.

While it is possible to use the part of the complete input that each part of
the program is interested in, it pays to create a first layer of filtering that
transforms the raw input into a more declarative description based on the
context of the program.

The same is true about the output: it is best to structure each state's
processing function so that the output is a declarative description of
that particular state. For instance, while in the menu state, we could
create a type to represent the possible input, one to represent the
state while in the menu, and create one function that processes
new input to produce an ever changing state.

:star: **Change**

> Such a function would need to have access to the current state to produce the
> new one. Different abstractions that address this are monads, arrows,
> continuations, stream processors, functional reactive programming and
> explicit state passing. We will explore some of them later (NEEDS REFERENCE).

PLACE FOR AN EXAMPLE WITHOUT FRP, ONE WITH FRP.
