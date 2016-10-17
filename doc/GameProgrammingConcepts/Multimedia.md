This article deals with input and output systems, their connection to the main
program and the game loop. In particular, we will explore:

* Cameras and views
* [[Scrolling]]
* [[Animation]]

## Interactive cameras and views

A common pattern in interactive applications is that the same entity that
consumes or presents output also produces input. This may obviously true
for players: they are the ultimate consumers and producers. With this abstraction
in mind AI-controlled players can also been as pairs of consumer/producer:
they take the game state, "show" it (to an intelligent agent/function),
and produce a set of actions pertaining to some of the players.

In the game programming literature, this abstaction is known as a *view*.
Other names for it are *reactive value* <sup>1</sup>.

Consuming the output and producing a new input may not be synchronous actions.
Network players, for instance, can be abstracted as views: the game state (or a
game update) is sent via the network, and input actions are transmited back to
the server. The player may be receiving updates every few miliseconds (if
latency is low) while only firing a few times per second, max.

Using views also helps abstract from polling vs callbacks, rendering
asynchronously, having multiple cameras for different players, and even using
widget systems.

Example of a view in a monadic game.
Example of views in Arrowized FRP.
Example of views in Keera Hails.

# Cameras

Games often present scenes that do not fit on the screen. Mario Bros. is such a
game. Counter strike is such a game. Any racing game is also of this kind.

In arcade 2D-based games, one can easily simulate this effect by moving all
other elements (oponents, background, etc), when the player "moves" forward.
The player moving forward, then, is just an illusion.  But, as your logic and
physics become more complex, and you add more than one player, this workaround
becomes harder to implement.

While elements are shown in screen coordinates, the internal processing
function should work with *game*, or *world* coordinates.

What's actually going on in these games is that you have a perspective, a point
of view, a camera pointed to the *game world* that only shows you a part of it,
from a specific angle. What's more is that camera is a portal also for the
input: what looks like middle of the screen in screen coordinates may not
represent the center of the world.

It's easy to see that, given a function `f` that transforms game world
coordinates into screen coordinates, we have:

``` haskell
gameForUser   = capScene screenSize $ transformState f gameState
inputFromUser = transformInput f<sup>-1</sup> screenInput
``` 

IN 3D... Game Engine Architecture

In 2D... Whatever

PLACE FOR ANOTHER EXAMPLE

# HUDs, toolbars and overlaid visuals

Heads-on displays are a general name for those information panels 
shown on top of the game with additional information.

PLACE FOR AN IMAGE

What's interesting about them is that:
* They can be enabled or disabled.
* They may be shown or not depending on the game state.
* They may be interactive (handle input apart from presenting output).

The last point is perhaps the most crucial one: while it is easy to display
something in one area of the screen, we also need to:
* Transform screen coordinates into coordinates local for the displayed elements.
* Translate raw input events into more meaningful event descriptions.

For example, given an input event (eg. mouse click at position (x,y)), we need to
determine wether it lies on the game area or a panel, and then translate the
screen coordinates into local coordinates for that element. If it's the game
area, all we need is to apply the inverse camera function. If the panel, we may
just need to rebase them based on the position of the panel on the screen.

The general transformation is as follows:

PLACE FOR THE MATRIX

What we are describing here is true both for HUDs and any other permanent or
temporary on-screen element that does not use the game camera as the main
game area.

PLACE FOR AN EXAMPLE

Note that, after the transformation of the event coordinates, we still need
to transform the low-level input event (click, etc.) into an declarative
depiction of what happened (fire).

You can see this intermediate layer as a simple widget system, and may even
consider creating an abstraction for containers and alignments that facilitates
adapting to different screen sizes and screen rotations during gameplay. I
recommend that you keep it as simple as you can, and as abstract as you can.

PLACE FOR AN EXAMPLE OF CONTAINERS

PLACE FOR AN EXAMPLE OF ALIGNMENT

# 3D

TO BE COMPLETED. Readers will mainly be refered to books and other material.

# References

1. Keera Hails http://github.com/keerastudios/keera-hails
