module Math.Vector2 exposing (..)

{-|
@docs Vec2, fromTuple
-}

import Helpers exposing (..)
import Native.Float32Array


{-|
-}
type Vec2
    = Vec2


{-|
-}
fromTuple : Float2 -> Vec2
fromTuple =
    Native.Float32Array.fromTuple2
