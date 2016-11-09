module WebGLTypes exposing (..)

import Math.Vector2 as V2
import Math.Vector3 as V3
import Math.Vector4 as V4
import Math.Matrix4 as M4


type alias GLVec2 =
    V2.Vec2


type alias GLVec3 =
    V3.Vec3


type alias GLVec4 =
    V4.Vec4


type alias GLMat4 =
    M4.Mat4


fromFloat2 =
    V2.fromTuple


fromFloat3 =
    V3.fromTuple


fromFloat4 =
    V4.fromTuple


fromFloat4x4 =
    M4.fromTuple
