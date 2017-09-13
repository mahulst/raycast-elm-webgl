module Main exposing (main)

import Color exposing (Color)
import Html exposing (Html)
import Html.Attributes exposing (width, height, style)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (vec3, Vec3, toTuple)
import Math.Vector4 as Vec4
import WebGL exposing (Mesh, Shader)
import Mouse exposing (..)
import Keyboard exposing (..)
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
    { cameraPos : Vec3
    , cameraAngle : Float
    , rays : List ( Vec3, Vec3 )
    }


type alias Vertex =
    { color : Vec3
    , position : Vec3
    }


type alias Uniforms =
    { rotation : Mat4
    , perspective : Mat4
    , camera : Mat4
    , shade : Float
    }


type Msg
    = KeyMsg Keyboard.KeyCode
    | MouseClick Mouse.Position


init : ( Model, Cmd Msg )
init =
    ( Model (getCameraPosFromAngle 0 3) 0 [], Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyMsg code ->
            case code of
                -- Right
                37 ->
                    let
                        angle =
                            model.cameraAngle + 0.2

                        cameraPos =
                            getCameraPosFromAngle angle (Vec3.getY model.cameraPos)
                    in
                        ( { model | cameraPos = cameraPos, cameraAngle = angle }, Cmd.none )

                39 ->
                    let
                        angle =
                            model.cameraAngle - 0.2

                        cameraPos =
                            getCameraPosFromAngle angle (Vec3.getY model.cameraPos)
                    in
                        ( { model | cameraPos = cameraPos, cameraAngle = angle }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        MouseClick pos ->
            let
                destination =
                    getClickPosition model pos

                origin =
                    model.cameraPos

                newRays =
                    model.rays ++ [ ( origin, destination ) ]
            in
                ( { model | rays = newRays }, Cmd.none )


getCameraPosFromAngle : Float -> Float -> Vec3
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
        vec3 adjacent heightOfCamera opposite


angleOfCamera : Mouse.Position -> Float
angleOfCamera { x, y } =
    (toFloat x) / 1000 * 7.2


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.downs KeyMsg
        , Mouse.clicks MouseClick
        ]


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
            (uniforms model)
        , WebGL.entity
            vertexShader
            fragmentShader
            (lines model)
            (uniforms model)
        ]

multi : Vec3 -> Float -> Vec3
multi v1 f =
  vec3
    ((Vec3.getX v1) * f)
    ((Vec3.getY v1) * f)
    ((Vec3.getZ v1) * f)


getClickPosition : Model -> Mouse.Position -> Vec3
getClickPosition model pos =
    let
        x =
            toFloat pos.x

        y =
            toFloat pos.y

        normalizedPosition =
            ( (x * 2) / 1000 - 1, (1 - y / 1000 * 2) )

        homogeneousClipCoordinates =
            Vec4.vec4
                (Tuple.first normalizedPosition)
                (Tuple.second normalizedPosition)
                -1
                1

        inversedViewMatrix =
            Maybe.withDefault Mat4.identity (Mat4.inverse (camera model))

        inversedProjectionMatrix =
            Maybe.withDefault Mat4.identity (Mat4.inverse perspective)

        test = mulVector inversedProjectionMatrix homogeneousClipCoordinates

        test2 = Vec4.vec4 (Vec4.getX test) (Vec4.getY test) -1 0

        test25 = mulVector inversedViewMatrix test2

        test3 = vec3 (Vec4.getX test25) (Vec4.getY test25) (Vec4.getZ test25)

        test4 = Vec3.normalize test3

        test5 = model.cameraPos

        test6 = multi test4 20

        test7 = Vec3.add test5 test6


        toVec3 =
            test7
    in
        toVec3


mulVector : Mat4 -> Vec4.Vec4 -> Vec4.Vec4
mulVector mat v =
    let
        rec =
            Mat4.toRecord mat

        r1 =
            Vec4.vec4 rec.m11 rec.m12 rec.m13 rec.m14

        r2 =
            Vec4.vec4 rec.m21 rec.m22 rec.m23 rec.m24

        r3 =
            Vec4.vec4 rec.m31 rec.m32 rec.m33 rec.m34

        r4 =
            Vec4.vec4 rec.m41 rec.m42 rec.m43 rec.m44
    in
        Vec4.vec4 (Vec4.dot r1 v) (Vec4.dot r2 v) (Vec4.dot r3 v) (Vec4.dot r4 v)


createLineFromVec3 : ( Vec3, Vec3 ) -> List ( Vertex, Vertex )
createLineFromVec3 ( a, b ) =
    let
        a1 =
            Vertex (vec3 0 0 0) a

        a2 =
            Vertex (vec3 0 0 0) b

        a3 =
            Vertex (vec3 0 0 0) (vec3 0 0 0)
    in
        [ ( a1, a2 ) ]


lines : Model -> Mesh Vertex
lines model =
    let
        rays =
            List.map createLineFromVec3 model.rays

        flattenedRays =
            List.concat rays
    in
        WebGL.lines flattenedRays


perspective : Mat4
perspective =
    Mat4.makePerspective 45 1 0.01 50


camera : Model -> Mat4
camera model =
    Mat4.makeLookAt model.cameraPos (vec3 0 0 0) (vec3 0 1 0)


uniforms : Model -> Uniforms
uniforms model =
    { rotation = Mat4.identity
    , perspective = perspective
    , camera = camera model
    , shade = 0.8
    }


floor : Mesh Vertex
floor =
    WebGL.lines
        [ ( Vertex (vec3 1 0 0) (vec3 0 -1 1), Vertex (vec3 1 0 0) (vec3 0 -1 -1) )
        , ( Vertex (vec3 0 1 0) (vec3 -1 -1 0), Vertex (vec3 0 1 0) (vec3 1 -1 0) )
        , ( Vertex (vec3 0 0 1) (vec3 0 -1 0), Vertex (vec3 0 0 1) (vec3 0 1 0) )
        ]



-- Mesh


cubeMesh : Mesh Vertex
cubeMesh =
    let
        rft =
            vec3 0.5 0.5 0.5

        lft =
            vec3 -0.5 0.5 0.5

        lbt =
            vec3 -0.5 -0.5 0.5

        rbt =
            vec3 0.5 -0.5 0.5

        rbb =
            vec3 0.5 -0.5 -0.5

        rfb =
            vec3 0.5 0.5 -0.5

        lfb =
            vec3 -0.5 0.5 -0.5

        lbb =
            vec3 -0.5 -0.5 -0.5
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


face : Color -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> List ( Vertex, Vertex, Vertex )
face rawColor a b c d =
    let
        color =
            let
                c =
                    Color.toRgb rawColor
            in
                vec3
                    (toFloat c.red / 255)
                    (toFloat c.green / 255)
                    (toFloat c.blue / 255)

        vertex position =
            Vertex color position
    in
        [ ( vertex a, vertex b, vertex c )
        , ( vertex c, vertex d, vertex a )
        ]



-- Shaders


vertexShader : Shader Vertex Uniforms { vcolor : Vec3 }
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


fragmentShader : Shader {} Uniforms { vcolor : Vec3 }
fragmentShader =
    [glsl|

        precision mediump float;
        uniform float shade;
        varying vec3 vcolor;
        void main () {
            gl_FragColor = shade * vec4(vcolor, 1.0);
        }

    |]
