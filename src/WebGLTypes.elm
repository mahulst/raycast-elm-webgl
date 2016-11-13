module WebGLTypes exposing (..)

{-|
The types in this library correspond to a `Float32Array` in JavaScript.
This library is meant to be used with WebGL.

## Types
@docs Vec2, Vec3, Vec4, Mat4

## Constructors
@docs fromFloat2, fromFloat3, fromFloat4, fromFloat4x4

--
Type aliases to simplify type annotations
@docs Float2, Float3, Float4, Float4x4
-}

import Math.Vector2 as V2
import Math.Vector3 as V3
import Math.Vector4 as V4
import Math.Matrix4 as M4
import Native.Float32Array


{-| Corresponds to vec2 in GLSL
-}
type alias Vec2 =
    V2.Vec2


{-| vec3
-}
type alias Vec3 =
    V3.Vec3


{-| vec4
-}
type alias Vec4 =
    V4.Vec4


{-| mat4
-}
type alias Mat4 =
    M4.Mat4


{-| -}
fromFloat2 : Float2 -> Vec2
fromFloat2 =
    Native.Float32Array.fromTuple2


{-| -}
fromFloat3 : Float3 -> Vec3
fromFloat3 =
    Native.Float32Array.fromTuple3


{-| -}
fromFloat4 : Float4 -> Vec4
fromFloat4 =
    Native.Float32Array.fromTuple4


{-| -}
fromFloat4x4 : Float4x4 -> Mat4
fromFloat4x4 =
    Native.Float32Array.fromTuple4x4


{-| -}
type alias Float2 =
    ( Float, Float )


{-| -}
type alias Float3 =
    ( Float, Float, Float )


{-| -}
type alias Float4 =
    ( Float, Float, Float, Float )


{-| -}
type alias Float4x4 =
    ( Float4, Float4, Float4, Float4 )
