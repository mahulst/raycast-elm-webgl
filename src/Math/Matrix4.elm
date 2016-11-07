module Math.Matrix4 exposing (..)

{-|
@docs Mat4, fromTuple
-}

import Helpers exposing (..)
import Native.Float32Array


{-| 4x4 matrix type
-}
type Mat4
    = Mat4


{-|
-}
fromTuple : Float4x4 -> Mat4
fromTuple =
    Native.Float32Array.fromTuple4x4
