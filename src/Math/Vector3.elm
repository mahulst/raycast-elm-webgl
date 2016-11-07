module Math.Vector3 exposing (..)

{-|
@docs Vec3, fromTuple
-}

import Helpers exposing (..)
import Native.Float32Array


{-|
-}
type Vec3
    = Vec3


{-|
-}
fromTuple : Float3 -> Vec3
fromTuple =
    Native.Float32Array.fromTuple3
