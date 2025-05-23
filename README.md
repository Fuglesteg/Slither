# Slither

This is a work in progress game engine written in Common Lisp. It uses OpenGL
and GLFW.

It includes a renderer with easy to use macros that abstract certain parts of OpenGL. Like `define-texture`, `define-array-texture`, `define-shader-program`, `draw-texture`, `draw-array-texture` etc.

Currently you write game logic in entities and behaviors. A behavior is something that contains data and can be called by the engine every tick, including when it is initialized. An entity is something with a position in the game that can contain behaviors. Together with these abstractions you can compose game logic that can easily be reused. They are defined using the macros: `defentity` and `defbehavior`.

Currently entities and behaviors are implemented using CLOS, but it would be interesting to see if these could be implemented as something similar to ECS, such that a development build could use CLOS for easier debugging and better interactivity, while release builds could use ECS for performance.