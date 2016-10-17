Animation can make your application much more interesting. There are two kinds
of animations: discrete-based animation, and continuous animation.

# Frame-by-frame (discrete) animation

Discrete animation is based on the idea of rendering frames in succession. Since your
monitor probably renders at 60 or 120Hz, and your computer is sending still images
to it, all forms of rendering become discretized at some point.

Cinema, as recorded and projected on your screen, also works this way.

<p align="center">
<img src="https://raw.githubusercontent.com/wiki/keera-studios/haskell-game-programming/images/The_Horse_in_Motion.jpg" width="240" />
<img src="https://raw.githubusercontent.com/wiki/keera-studios/haskell-game-programming/images/The_Horse_in_Motion.jpg" width="240" />
<img src="https://raw.githubusercontent.com/wiki/keera-studios/haskell-game-programming/images/The_Horse_in_Motion-anim.gif" width="240" />
<br />
<sup><em>Photographer Eadwear Muybridge's pictures of </em>The Horse in Motion<em>, from
1878. Left, still images. Right, animation created from still images.</em></sup>
</p>

You can achieve a similar effect by rendering different images in succession.
For instance, you can draw a flapping bird from a set of still images:
<p align="center">
<img src="https://raw.githubusercontent.com/wiki/keera-studios/haskell-game-programming/images/birdflapping.gif" width="96" />
<br />
<img src="https://raw.githubusercontent.com/wiki/keera-studios/haskell-game-programming/images/flappingbirdspritesheet.png" />
</p>

Rendering this way has a few benefits and a few caveats:
* It's very easy to implement.
* You can get assets from all over the internet, for free or very cheap.
* If you slow the animation down, it will look jumpy.
* If you want to render a time in between frames, you will have to settle for
the image before or after that time. If your rendering speed is slow-ish and
not uniform, this effect may be noticeable.

PLACE FOR CODE IN SDL

PLACE FOR CODE WITH FRP AND SDL

# Transitions

Continuous animation is a form of continously moving or displacing elements.
Although in the end it is polled at a specific time and therefore discretized,
the idea is that there is a function of time that takes your character or
element smoothly from one state to the next, so that each intermediate state is
unique.

You can use continuous animation both for game characters and for game
movement.  For instance, in a flappy bird game, the animation of the bird
itself may be discrete (frame-based), like the one above, but the movement from
one place to the next may be continuous (we could interpolate intermediate
states smoothly).

PLACE FOR ANIMATION (SLOW, FAST)

PLACE FOR CODE IN FRP (USING OPENGL?)

# One-time animations

Although this has nothing to do with rendering per se, it is very common for some
transitions or animations to be run only once. For instance, you might want to
play a video (until it finishes), or smoothly transition into the game with a fog
effect.

Your logic might know that it needs to wait for, say, 2.5 seconds until the
game loads. But you might also want to separate this completely from the logic,
set the game in "loading" state, and let a second layer tell you when the
effect is over.

If you do it in the former way, make sure that your logic drives the renderer, not
the other way around. The idea is that the game logic tells the renderer how long it
has to transition, and the output system can choose to represent a transition in
any way you want. This simplifies your design by not making your renderer or output
system also an input to your game.

PLACE FOR CODE

PLACE FOR CODE
