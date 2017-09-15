module Main exposing (main, rayTriangleIntersect)

import AnimationFrame
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
import Time exposing (Time)


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
    , cubeSelected : Bool
    , theta : Float
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
    | DeltaTime Time


init : ( Model, Cmd Msg )
init =
    ( Model (getCameraPosFromAngle 0 3) 0 False 0, Cmd.none )


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
                origin =
                    model.cameraPos

                destination =
                    getClickPosition model pos

                direction =
                    Vec3.direction destination origin

                triangleList =
                    List.map (\( v0, v1, v2 ) -> ( v0.position, v1.position, v2.position )) cubeTriangles

                cam =
                    camera model

                isClicked =
                    isCubeClicked origin direction triangleList
            in
                ( { model | cubeSelected = isClicked }, Cmd.none )

        DeltaTime time ->
            let
                newTheta =
                    model.theta + (Time.inMilliseconds time) / 5000
            in
                ( { model | theta = newTheta }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.downs KeyMsg
        , Mouse.clicks MouseClick
        , AnimationFrame.diffs DeltaTime
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
        ]


perspective : Mat4
perspective =
    Mat4.makePerspective 45 1 0.01 50


camera : Model -> Mat4
camera model =
    Mat4.makeLookAt model.cameraPos (vec3 0 0 0) (vec3 0 1 0)


uniforms : Model -> Uniforms
uniforms model =
    { rotation =
        Mat4.mul
            (Mat4.makeRotate (3 * 40) (vec3 0 1 0))
            (Mat4.makeRotate (2 * 40) (vec3 1 0 0))
    , perspective = perspective
    , camera = camera model
    , shade =
        if model.cubeSelected then
            0.4
        else
            1
    }



-- Mesh


cubeMesh : Mesh Vertex
cubeMesh =
    cubeTriangles
        |> WebGL.triangles


cubeTriangles : List ( Vertex, Vertex, Vertex )
cubeTriangles =
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
        [ face Color.green rbb rfb rft rbt

        --        , face Color.blue rft rfb lfb lft
        --        , face Color.yellow rft lft lbt rbt
        --        , face Color.red lbb lfb rfb rbb
        --        , face Color.purple lft lfb lbb lbt
        --        , face Color.orange rbt rbb lbb lbt
        ]
            |> List.concat


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
            gl_Position = perspective * camera *  rotation * vec4(position, 1.0);
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



-- Utility methods


isCubeClicked : Vec3 -> Vec3 -> List ( Vec3, Vec3, Vec3 ) -> Bool
isCubeClicked origin destination list =
    let
        intersect =
            rayTriangleIntersect origin destination
    in
        List.any
            (\triangle ->
                intersect triangle
                    |> (\m ->
                            case m of
                                Nothing ->
                                    False

                                Just _ ->
                                    True
                       )
            )
            list


rayTriangleIntersect : Vec3 -> Vec3 -> ( Vec3, Vec3, Vec3 ) -> Maybe Vec3
rayTriangleIntersect rayOrigin rayDirection ( triangle0, triangle1, triangle2 ) =
    let
        epsilon =
            0.000001

        edge1 =
            Vec3.sub triangle1 triangle0

        edge2 =
            Vec3.sub triangle2 triangle0

        pvec =
            Vec3.cross rayDirection edge2

        det =
            Vec3.dot edge1 pvec
    in
        if det < epsilon then
            Nothing
        else
            let
                tvec =
                    Vec3.sub rayOrigin triangle0

                u =
                    Vec3.dot tvec pvec
            in
                if u < 0 || u > det then
                    Nothing
                else
                    let
                        qvec =
                            Vec3.cross tvec edge1

                        v =
                            Vec3.dot rayDirection qvec
                    in
                        if v < 0 || u + v > det then
                            Nothing
                        else
                            let
                                t =
                                    (Vec3.dot edge2 qvec) / det

                                v0 =
                                    (Vec3.getX rayOrigin) + t * (Vec3.getX rayDirection)

                                v1 =
                                    (Vec3.getY rayOrigin) + t * (Vec3.getY rayDirection)

                                v2 =
                                    (Vec3.getZ rayOrigin) + t * (Vec3.getZ rayDirection)
                            in
                                Just (vec3 v0 v1 v2)


getCameraPosFromAngle : Float -> Float -> Vec3
getCameraPosFromAngle angle heightOfCamera =
    let
        cosine =
            cos angle

        sinus =
            sin angle

        hypotenuse =
            8

        adjacent =
            cosine * hypotenuse

        opposite =
            sinus * hypotenuse
    in
        vec3 adjacent heightOfCamera opposite


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

        invertedViewMatrix =
            Mat4.inverseOrthonormal (camera model)

        invertedProjectionMatrix =
            Maybe.withDefault Mat4.identity (Mat4.inverse perspective)

        vec4CameraCoordinates =
            mulVector invertedProjectionMatrix homogeneousClipCoordinates

        direction =
            Vec4.vec4 (Vec4.getX vec4CameraCoordinates) (Vec4.getY vec4CameraCoordinates) -1 0

        vec4WorldCoordinates =
            mulVector invertedViewMatrix direction

        vec3WorldCoordinates =
            vec3 (Vec4.getX vec4WorldCoordinates) (Vec4.getY vec4WorldCoordinates) (Vec4.getZ vec4WorldCoordinates)

        normalizedVec3WorldCoordinates =
            Vec3.normalize vec3WorldCoordinates

        origin =
            model.cameraPos

        scaledDirection =
            Vec3.scale 20 normalizedVec3WorldCoordinates

        destination =
            Vec3.add origin scaledDirection
    in
        destination


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
