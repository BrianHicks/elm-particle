module Confetti exposing (main)

-- TODO: make these confetti particles much nicer, and document this whole thing
-- TODO: oh, and clean up the imports

import Browser exposing (Document)
import Browser.Events
import Html exposing (Html)
import Html.Attributes as Attrs exposing (style)
import Json.Decode as Decode
import Particle exposing (Particle)
import Particle.System as System exposing (System)
import Random exposing (Generator)
import Random.Extra
import Random.Float exposing (normal)
import Svg exposing (Svg)
import Svg.Attributes as SAttrs
import Task
import Time exposing (Posix)


type alias Model =
    { system : System Confetti }


type Msg
    = Burst Float Float
    | ParticleMsg (System.Msg Confetti)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Burst x y ->
            ( { model | system = System.burst (Random.list 25 (particleAt x y)) model.system }
            , Cmd.none
            )

        ParticleMsg particleMsg ->
            ( { model | system = System.update particleMsg model.system }
            , Cmd.none
            )


view : Model -> Document Msg
view model =
    { title = "Confetti!"
    , body =
        [ System.view viewConfetti
            [ style "width" "100%"
            , style "height" "100vh"
            ]
            model.system
        ]
    }


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> ( { system = System.init (Random.initialSeed 0) }, Cmd.none )
        , view = view
        , update = update
        , subscriptions =
            \model ->
                Sub.batch
                    [ System.sub [] ParticleMsg model.system
                    , Browser.Events.onClick
                        (Decode.map2 Burst
                            (Decode.field "clientX" Decode.float)
                            (Decode.field "clientY" Decode.float)
                        )
                    ]
        }



-- Generators!


{-| We're going to emulate the Apple Color Emoji party popper emoji: 🎉
([emojipedia](https://emojipedia.org/party-popper/))

What's it got? Well, we've got the cone, which we'll render statically and maybe
have a little animation elsewhere. But then we've got streamers and confetti,
streaming out towards the upper right.

-}
type Confetti
    = Square
        { color : Color
        , rotationOffset : Float
        , xRotations : Float
        , yRotations : Float
        , zRotations : Float
        }


{-| What color make our little celebration pieces? We'll use a custom type here
to represent which colors we want, as they have a slightly off-color border.
-}
type Color
    = Red
    | Pink
    | Orange
    | Yellow
    | Blue


{-| Generate a confetti square, using the color ratios seen in the Apple Color
Emoji.
-}
genSquare : Generator Confetti
genSquare =
    Random.map5
        (\color rotationOffset xRotations yRotations zRotations ->
            Square
                { color = color
                , rotationOffset = rotationOffset
                , xRotations = xRotations
                , yRotations = yRotations
                , zRotations = zRotations
                }
        )
        (Random.weighted
            ( 2 / 11, Red )
            [ ( 2 / 11, Pink )
            , ( 2 / 11, Orange )
            , ( 2 / 11, Yellow )
            , ( 3 / 11, Blue )
            ]
        )
        (normal 0 1)
        (normal 0 1)
        (normal 0 1)
        (normal 0 1)


{-| Generate confetti according to the ratios seen in the Apple Color Emoji.
-}
genConfetti : Generator Confetti
genConfetti =
    Random.Extra.frequency
        ( 11 / 15, genSquare )
        []


particleAt : Float -> Float -> Generator (Particle Confetti)
particleAt x y =
    Particle.generate genConfetti
        |> Particle.withLifetime (Random.constant 1.5)
        |> Particle.at (Random.constant { x = x, y = y })
        |> Particle.heading (genHeading 0 400)
        |> Particle.withGravity 980


genRadius : Generator Float
genRadius =
    normal 20 5


genHeading : Float -> Float -> Generator { angle : Float, speed : Float }
genHeading angleCenter powerCenter =
    Random.map2 (\angle speed -> { angle = degrees angle, speed = speed })
        (normal angleCenter 15)
        (normal powerCenter 100)



-- views


viewConfetti : Confetti -> Float -> Svg msg
viewConfetti confetti lifetime =
    case confetti of
        Square { color, rotationOffset, xRotations, yRotations, zRotations } ->
            Svg.rect
                [ SAttrs.width "20px"
                , SAttrs.height "20px"
                , SAttrs.x "-10px"
                , SAttrs.y "-10px"
                , SAttrs.fill (fill color)
                , SAttrs.stroke (stroke color)
                , SAttrs.strokeWidth "1px"
                , SAttrs.opacity <| String.fromFloat <| 1 - cubicBezier 1 0 1 -0.5 lifetime
                , SAttrs.style <|
                    "transform: rotateX("
                        ++ String.fromFloat (xRotations * lifetime + rotationOffset)
                        ++ "turn) rotateY("
                        ++ String.fromFloat (yRotations * lifetime + rotationOffset)
                        ++ "turn) rotateZ("
                        ++ String.fromFloat (zRotations * lifetime + rotationOffset)
                        ++ "turn);"
                ]
                []


fill : Color -> String
fill color =
    case color of
        Red ->
            "#D93E61"

        Pink ->
            "#F884B2"

        Orange ->
            "#FEA849"

        Yellow ->
            "#FEFD34"

        Blue ->
            "#4A92FF"


stroke : Color -> String
stroke color =
    case color of
        Red ->
            "#E4A5B7"

        Pink ->
            "#D28194"

        Orange ->
            "#D9AF87"

        Yellow ->
            "#C9B975"

        Blue ->
            "#93A7D8"



-- utils


cubicBezier : Float -> Float -> Float -> Float -> Float -> Float
cubicBezier p0 p1 p2 p3 t =
    let
        oneMinusT =
            1 - t
    in
    oneMinusT ^ 3 * p0 + (3 * oneMinusT ^ 2 * t * p1) + (3 * oneMinusT * t ^ 2 * p2) + (t ^ 3 * p3)
