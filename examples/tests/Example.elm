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
