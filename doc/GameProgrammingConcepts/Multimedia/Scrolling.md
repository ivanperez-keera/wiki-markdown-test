As we just saw in the chapter about cameras, many games present a portal to a
world larger than it fits on the screen. Backgrounds can lighten an otherwise
dull screen, but what really changes game appearance are moving backgrounds and
a perception of depth.

First, we will explore backgrounds without depth (static and moving). Then we
will explore backgrounds with depth (parallax). In the next section we will
explore depth in a more general way (even in 2D games, often termed 2.5 as
opposed to full 3D).

## Static backgrounds

Implementing static backgrounds could not be more trivial. A static,
uninteractive background can be presented both as an adhoc layer in the render
(see first example) or integrated as one more visual element. What's important
in the second case is that, unlike other visual elements, a static background
is *not* affected by the game camera, but it is affected by transformations
that have to do with laying different elements out on the screen.

A PICTURE IS WORTH A THOUSAND WORDS

PLACE FOR AN EXAMPLE

## Moving backgrounds: a very large, but limited, world

In some cases, the game world is just slightly larger than the screen, but
still limited in size. The background is therefore also larger than the screen,
but finite.  Presenting this kind of world is relatively simple, and it only
requires that we crop part of it to the size of the screen. We can generalise
this approach easily by using transformation/camera matrices applied to
background coordinates.

What's important in this case is to make sure that players can **never** move
out of the world boundaries, or there will be screen areas with no background.

PLACE FOR AN EXAMPLE

## Moving backgrounds: unlimited worlds

Some games gave very large or even unlimited world sizes. In such cases,
backgrounds tend to be designed so that each side matches the opposite
perfectly, like in a rolling paper.

PLACE FOR AN ANIMATION

This was a common trick in animation, and you can see it often
in older TV series:

PLACE FOR AN ANIMATION

There are several ways of implementing this. One way is to crop the background
in two, and present one part and then the other:

PLACE FOR AN EXAMPLE

Depending on the underlying framework, you may get away without cropping:
you can also paint the world twice, and then paint the rest of the scene
relative to its position with respect to the background.

While I personally tend to favour the first approach, whether cropping or
painting twice is performance-wise pretty much depends on the platform and
the underlying graphics layer (some do not let you paint out of screen area,
others easily facilitate rebasing the scene relative to a background).

## Parallax:

Games can achieve a more appealing sense of perception by adding depth. Even if
the action takes place in 2D, a common effect is to add several background
elements moving at different speeds as characters move along the world (so as
to make it look like some elements, the ones moving faster, are closer than
others, moving slower).

PLACE FOR AN IMAGE

While this may seem like a relatively new trick because of its common presence
on the web nowadays, there are old games around there that used this effect
already:

PLACE FOR AN IMAGE

Implementing parallax, so as long as the background layers are all presented
behind all the game action, is just as trivial as presenting one background,
but having one camera transformation matrix for each background.

PLACE FOR AN EXAMPLE

# Depth

## 2.5 dimensions

Even 2D games can achieve a sense of depth by introducing layers. Layers can
be presented without any modification to make them look closer, or with a
camera transformation.

PLACE FOR TWO IMAGES

Implementing these kinds of transformations is relatively trivial. First,
your game elements need to have a third dimension. Unlike the x and y
dimensions, which may have fine precision approximating the continuous,
the z dimension can easily be a integer if all you want is to layer your
scene. Different layers can then be presented to users one by one.

Layering your scene is actually a form of 3D with an integer z coordinate,
and as such it introduces some of the casuistics of 3D:

* If I click on the game area, which element of which layer am I trying
to interact with?

* If interactive layers are presented using parallax effects, how do I
adapt the input?

* If interactive layers are presented using parallax effects, how does
that affect the physics?

TO BE COMPLETED
