module Math.Vector4 exposing (..)

{-|
@docs Vec4, fromTuple
-}

import Helpers exposing (..)
import Native.Float32Array


{-|
-}
type Vec4
    = Vec4


{-|
-}
fromTuple : Float4 -> Vec4
fromTuple =
    Native.Float32Array.fromTuple4
