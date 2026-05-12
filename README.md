# Slither

Slither is a work in progress game engine written in Common Lisp.

The goal of Slither is to make game development easy, interactive and fun. While still being capable of building production grade games. The use of Common Lisp is a natural fit for this goal. Common Lisp offers us both interactive development, for rapid testing, prototyping and debugging, and high performance in a battle tested programming environment.

Game logic is defined using entities and behaviors. A behavior is something that contains data and can be called by the engine every tick, including when it is initialized. An entity is used to compose and use behaviors, while it can also contain it's own logic. Together with these abstractions you can compose game logic that can easily be reused. They are defined using the macros: `defentity` and `defbehavior`.

## Features

- Declarative renderer
    - Ease of use macros
        - `defshader`
        - `define-texture`
        - etc...
    - Procedural draw calls
        - `draw-texture`
        - `draw-rectangle`
        - etc...
- DSL for defining game logic in entities and behaviors
- Built-in behaviors for rapid prototyping
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
    - `networked` behavior for declaratively defining networked entities
- Sound system based on [Harmony](https://shirakumo.org/projects/harmony)
    - Location based sound