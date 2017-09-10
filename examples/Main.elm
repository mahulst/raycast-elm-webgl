module Main exposing (main)

{-
   Rotating cube with colored sides.
-}

import AnimationFrame
import Color exposing (Color)
import Html exposing (Html)
import WebGLTypes as GL exposing (fromFloat3, fromFloat4x4)
import Vector3 as V3 exposing (Vec3, Float3)
import Html.Attributes exposing (width, height, style)
import Matrix4 exposing (Mat4, Float4x4)
import Math.Vector4 as Vec4
import Time exposing (Time)
import WebGL exposing (Mesh, Shader)
import Mouse exposing (..)
import Debug


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }


type alias Model =
    { cameraPos : Float3
    , rays : List ( GL.Vec3, GL.Vec3 )
    }


type Msg
    = MoveMouse Mouse.Position
    | MouseClick Mouse.Position


init : ( Model, Cmd Msg )
init =
    ( Model ( 3, 3, 3 ) [], Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MoveMouse pos ->
            let
                angle =
                    angleOfCamera pos

                cameraPos =
                    getCameraPosFromAngle angle (V3.getY model.cameraPos)
            in
                ( { model | cameraPos = cameraPos }, Cmd.none )

        MouseClick pos ->
            let
                destination =
                    fromFloat3 ( 0, 1, 0 )

                origin =
                    model.cameraPos

                newRays =
                    model.rays ++ [ ( destination, (fromFloat3 origin) ) ]
            in
                ( { model | rays = newRays }, Cmd.none )


getCameraPosFromAngle : Float -> Float -> Float3
getCameraPosFromAngle angle heightOfCamera =
    let
        cosine =
            cos angle

        sinus =
            sin angle

        hypotenuse =
            3

        adjacent =
            cosine * hypotenuse

        opposite =
            sinus * hypotenuse
    in
        ( adjacent, heightOfCamera, opposite )


angleOfCamera : Mouse.Position -> Float
angleOfCamera { x, y } =
    (toFloat x) / 1000 * 7.2


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Mouse.moves MoveMouse, Mouse.clicks MouseClick ]


view : Model -> Html Msg
view model =
    WebGL.toHtml
        [ width 1000
        , height 1000
        , style [ ( "display", "block" ) ]
        ]
        [ WebGL.entity
            vertexShader
            fragmentShader
            cubeMesh
            (uniforms model)
        , WebGL.entity
            vertexShader
            fragmentShader
            floor
            (uniforms2 model)
        , WebGL.entity
            vertexShader
            fragmentShader
            (lines model)
            (uniforms2 model)
        ]


type alias Position =
    { x : Float
    , y : Float
    }


undefined : () -> a
undefined _ =
    Debug.crash "Undefined!"



--getClickPosition : Model -> Position -> ( Vertex, Vertex )
--getClickPosition model { x, y } =
--    let
--        normalizedPosition =
--            ( (x * 2) / 1000 - 1,  (1 - (2 * y) / 1000))
--
--        homogeneousClipCoordinates = Vec4.vec4 (Tuple.first normalizedPosition) (Tuple.second normalizedPosition) -1  1
--
--        inversedProjectionMatrix =
--            Matrix4.inverseOrthonormal (camera model)
--
--        vec4AppliedInversedProjectionMatrix =
--            undefined -- Matrix4.transform inversedProjectionMatrix homogeneousClipCoordinates
--
--        inversedViewMatrix =
--            undefined -- Matrix4.inverseOrthonormal <something>
--
--        vec4AppliedInversedViewMatrix =
--            undefined
--
--        vec3NormalizedRay =
--            undefined -- Vec3.normalize (fromFloat3 <x y z from vec4AppliedInversedViewMatrix> )
--
--        fromVec3 =
--            model.cameraPos
--
--        toVec3 =
--            undefined
--    in
--            ( Vertex (fromFloat3 (0, 0, 0)) fromVec3, Vertex (fromFloat3 (0, 0, 0)) (fromFloat3  0 0 0 )) --toVec3)


createLineFromVec3 : ( GL.Vec3, GL.Vec3 ) -> ( Vertex, Vertex )
createLineFromVec3 ( a, b ) =
    ( Vertex (fromFloat3 ( 0, 0, 0 )) a, Vertex (fromFloat3 ( 0, 0, 0 )) b )


lines : Model -> Mesh Vertex
lines model =
    let
        rays =
            List.map createLineFromVec3 model.rays
    in
        WebGL.lines rays


type alias Uniforms =
    { rotation : GL.Mat4
    , perspective : GL.Mat4
    , camera : GL.Mat4
    , shade : Float
    }


perspective : Float4x4
perspective =
    Matrix4.makePerspective 45 1 0.01 100


camera : Model -> Float4x4
camera model =
    Matrix4.makeLookAt model.cameraPos ( 0, 0, 0 ) ( 0, 1, 0 )


uniforms : Model -> Uniforms
uniforms model =
    { rotation =
        (fromFloat4x4 Matrix4.identity)
    , perspective = (fromFloat4x4 perspective)
    , camera = (fromFloat4x4 (camera model))
    , shade = 0.8
    }


uniforms2 : Model -> Uniforms
uniforms2 model =
    { rotation =
        (fromFloat4x4 Matrix4.identity)
    , perspective = (fromFloat4x4 perspective)
    , camera = (fromFloat4x4 (camera model))
    , shade = 0.8
    }


floor : Mesh Vertex
floor =
    WebGL.lines
        [ ( Vertex (fromFloat3 ( 1, 0, 0 )) (fromFloat3 ( 0, -1, 1 )), Vertex (fromFloat3 ( 1, 0, 0 )) (fromFloat3 ( 0, -1, -1 )) )
        , ( Vertex (fromFloat3 ( 0, 1, 0 )) (fromFloat3 ( -1, -1, 0 )), Vertex (fromFloat3 ( 0, 1, 0 )) (fromFloat3 ( 1, -1, 0 )) )
        , ( Vertex (fromFloat3 ( 0, 0, 1 )) (fromFloat3 ( 0, -1, 0 )), Vertex (fromFloat3 ( 0, 0, 1 )) (fromFloat3 ( 0, 1, 0 )) )

        --        ,  ( Vertex (fromFloat3 (0, 1, 0)) (fromFloat3 (0, 1, 0)), Vertex (fromFloat3 (0, 1, 0)) (fromFloat3 (0, -1, 0)))
        --        , ( Vertex (fromFloat3 (0, 0, 1)) (fromFloat3 (-1, -1, 1)), Vertex (fromFloat3 (0, 0, 1)) (fromFloat3 (-1, -1, -1)))
        --        , ( Vertex (fromFloat3 (1, 0, 0)) (fromFloat3 (0, 0, 0)), Vertex (fromFloat3 (1, 0, 0)) (fromFloat3 (0, 0, 0)))
        ]



-- Mesh


type alias Vertex =
    { color : GL.Vec3
    , position : GL.Vec3
    }


cubeMesh : Mesh Vertex
cubeMesh =
    let
        rft =
            fromFloat3 ( 0.5, 0.5, 0.5 )

        lft =
            fromFloat3 ( -0.5, 0.5, 0.5 )

        lbt =
            fromFloat3 ( -0.5, -0.5, 0.5 )

        rbt =
            fromFloat3 ( 0.5, -0.5, 0.5 )

        rbb =
            fromFloat3 ( 0.5, -0.5, -0.5 )

        rfb =
            fromFloat3 ( 0.5, 0.5, -0.5 )

        lfb =
            fromFloat3 ( -0.5, 0.5, -0.5 )

        lbb =
            fromFloat3 ( -0.5, -0.5, -0.5 )
    in
        [ face Color.green rft rfb rbb rbt
        , face Color.blue rft rfb lfb lft
        , face Color.yellow rft lft lbt rbt
        , face Color.red rfb lfb lbb rbb
        , face Color.purple lft lfb lbb lbt
        , face Color.orange rbt rbb lbb lbt
        ]
            |> List.concat
            |> WebGL.triangles


face : Color -> GL.Vec3 -> GL.Vec3 -> GL.Vec3 -> GL.Vec3 -> List ( Vertex, Vertex, Vertex )
face rawColor a b c d =
    let
        color =
            let
                c =
                    Color.toRgb rawColor
            in
                fromFloat3
                    ( (toFloat c.red / 255)
                    , (toFloat c.green / 255)
                    , (toFloat c.blue / 255)
                    )

        vertex position =
            Vertex color position
    in
        [ ( vertex a, vertex b, vertex c )
        , ( vertex c, vertex d, vertex a )
        ]



-- Shaders


vertexShader : Shader Vertex Uniforms { vcolor : GL.Vec3 }
vertexShader =
    [glsl|

        attribute vec3 position;
        attribute vec3 color;
        uniform mat4 perspective;
        uniform mat4 camera;
        uniform mat4 rotation;
        varying vec3 vcolor;
        void main () {
            gl_Position = perspective * camera * rotation * vec4(position, 1.0);
            vcolor = color;
        }

    |]


fragmentShader : Shader {} Uniforms { vcolor : GL.Vec3 }
fragmentShader =
    [glsl|

        precision mediump float;
        uniform float shade;
        varying vec3 vcolor;
        void main () {
            gl_FragColor = shade * vec4(vcolor, 1.0);
        }

    |]
