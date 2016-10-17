Welcome to our Functional Game Programming documentation wiki.

Here you will find information regarding:
* Graphics and multimedia, including low-level libraries like SDL and OpenGL.
* Gaming hardware: Wiimote, Kinect.
* Physics, including bindings to external work like Chipmunk.
* Game programming abstractions and common patterns, including asset handling,
preferences, menus, data structures.
* Functional Reactive Programming.

If this is your first time
doing game programming in Haskell, I would recommend this sequence:

* [SDL 1.2](SDL » SDL1)
* [Input hardware: wiimotes](Hardware » Wiimote)
* [[Game Programming Concepts]]

After that, or if you are an experienced game programmer, or just a rebel
spirit dressed like a mere mortal, feel free to jump directly to any section you
want.

## Low-level Game Programming in Haskell

Low-level game programming in Haskell is done via bindings (Haskell
libraries that provide access to other libraries, often written in
C or C++). Libraries commonly used in games implement abstractions
for graphics and interactive multimedia, provide access to specific hardware
and simulate physics.

* Multimedia
  * [SDL 1.2](SDL » SDL1)
  * [[SDL2]]
  * [[OpenGL]]
* Hardware
  * [Input hardware: wiimotes](Hardware » Wiimote)
  * [[Input hardware: kinect]]
* Physics engines
  * (to be completed)

## Game Programming concepts

There are recurrent problems in game development, even between seemingly
very different games. This section explores concepts and patterns in game
programming at different levels, from high-level architectural patterns
to lower-level techniques like parallax scrolling and game saving.

Main article: [[Game Programming Concepts]]

* [[Architectural patterns]]: game loops, state machines, timelines.
* [Connecting external systems](Multimedia): views, video and audio, cameras, etc.
* [[Physics]]
* [[Robustness]]: testing and debugging, profiling.
* [Usability features](Usability): saving games, preferences.
* [[Data Structures]]

See also our list of [[examples]], which uses the ideas listed above to implement
either small games or specific features.

# Functional Reactive Programming

[[Functional Reactive Programming]] (FRP) is an functional abstraction for
values that change over time. Values in FRP are defined for the execution of
the program, and they are called *signals* or *behaviors*. Signals can be
defined in terms of other signals, making them also change as their dependecies
are updated.

This approach works well for interactive applications, and we will explore both
how to express ideas using different FRP flavors and how avoid some of the
pitfalls.

# Haskell specifics

*To be completed*
