module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Math.Vector3 exposing (vec3)
import Main exposing (rayTriangleIntersect)


suite : Test
suite =
    describe "Triangle / Ray intersection"
        [ test "succesfull intersect" <|
            \_ ->
                let
                    triangle =
                        ( (vec3 5 5 5), (vec3 10 15 4), (vec3 15 5 3) )

                    rayOrigin =
                        vec3 10 5 -20

                    rayDirection =
                        vec3 0 0 1
                in
                    Expect.equal (rayTriangleIntersect rayOrigin rayDirection triangle) (Just (vec3 10 5 4))
        , test "succesfull intersect 2" <|
            \_ ->
                let
                    triangle =
                        ( (vec3 0 -0.25412774340769434 0.9981512082033992), (vec3 0.28256232491660405 -1.2548287960561138e-16 0.9980587888138401), (vec3 0 0.2759101214140679 0.9979584477623187) )

                    rayOrigin =
                        vec3 8 3 0

                    rayDirection =
                        vec3 -0.9313842058181763 -0.36431002616882324 -0.04473506659269333
                in
                    Expect.equal (rayTriangleIntersect rayOrigin rayDirection triangle) (Just (vec3 10 5 4))
        , test "unsuccesfull intersect" <|
            \_ ->
                let
                    triangle =
                        ( (vec3 5 5 5), (vec3 10 15 4), (vec3 15 5 3) )

                    rayOrigin =
                        vec3 9 5 -5

                    rayDirection =
                        vec3 0 0 -1
                in
                    Expect.equal (rayTriangleIntersect rayOrigin rayDirection triangle) Nothing
        ]
