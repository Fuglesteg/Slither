# Slither

This is a work in progress game engine written in Common Lisp. It uses OpenGL
and GLFW.

It includes a renderer with easy to use macros that abstract certain parts of OpenGL. Like `define-texture`, `define-array-texture`, `define-shader-program`, `draw-texture`, `draw-array-texture` etc.

Game logic is defined using entities and behaviors. A behavior is something that contains data and can be called by the engine every tick, including when it is initialized. An entity is used to compose and use behaviors, while it can also contain it's own logic. Together with these abstractions you can compose game logic that can easily be reused. They are defined using the macros: `defentity` and `defbehavior`.

## Features

- Built in behaviors for rapid prototyping
    - Sprite
    - Speaker & Listener
    - Move & Camera
    - etc...
- Physics
    - Simple 2D rigidbody physics
    - Currently only supports circle colliders
- Networking
    - Authoritative server model
    - Client prediction
- Sound system based on [Harmony](https://shirakumo.org/projects/harmony)
    - Location based sound