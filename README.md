# webgl-types

Constructors for the GLSL vec and mat types for use with WebGL.

This library is intended to be used as a helper library for [elm-webgl-math](http://package.elm-lang.org/packages/Zinggi/elm-webgl-math/latest).

## Complete example
This is a modified version of the `triangle.elm` example from the webgl library.

```elm

module Main exposing (..)

import Matrix4 as M4 exposing (Float4x4)
import WebGLTypes as GL exposing (fromFloat3, fromFloat4x4)
import WebGL exposing (..)
import Html exposing (Html)
import Html.App as Html
import Html.Attributes exposing (width, height)
import AnimationFrame


-- Create a mesh with two triangles


type alias Vertex =
    { position : GL.Vec3, color : GL.Vec3 }


mesh : Drawable Vertex
mesh =
    Triangle
        [ ( Vertex (fromFloat3 ( 0, 0, 0 )) (fromFloat3 ( 1, 0, 0 ))
          , Vertex (fromFloat3 ( 1, 1, 0 )) (fromFloat3 ( 0, 1, 0 ))
          , Vertex (fromFloat3 ( 1, -1, 0 )) (fromFloat3 ( 0, 0, 1 ))
          )
        ]


main : Program Never
main =
    Html.program
        { init = ( 0, Cmd.none )
        , view = view
        , subscriptions = (\model -> AnimationFrame.diffs Basics.identity)
        , update = (\elapsed currentTime -> ( elapsed + currentTime, Cmd.none ))
        }


view : Float -> Html msg
view t =
    WebGL.toHtml
        [ width 400, height 400 ]
        [ render vertexShader
            fragmentShader
            mesh
            { perspective = fromFloat4x4 <| perspective (t / 1000) }
        ]


perspective : Float -> Float4x4
perspective t =
    M4.mul (M4.makePerspective 45 1 0.01 100)
        (M4.makeLookAt ( 4 * cos t, 0, 4 * sin t ) ( 0, 0, 0 ) ( 0, 1, 0 ))



-- Shaders


vertexShader : Shader { attr | position : GL.Vec3, color : GL.Vec3 } { unif | perspective : GL.Mat4 } { vcolor : GL.Vec3 }
vertexShader =
    [glsl|

attribute vec3 position;
attribute vec3 color;
uniform mat4 perspective;
varying vec3 vcolor;

void main () {
    gl_Position = perspective * vec4(position, 1.0);
    vcolor = color;
}

|]


fragmentShader : Shader {} u { vcolor : GL.Vec3 }
fragmentShader =
    [glsl|

precision mediump float;
varying vec3 vcolor;

void main () {
    gl_FragColor = vec4(vcolor, 1.0);
}

|]


```

