module Confetti exposing (main)

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



-- Generators!


{-| We're going to make confetti come out of the party popper emoji: 🎉
([emojipedia](https://emojipedia.org/party-popper/))

What's it got? Well, at least in Apple Color Emoji, we've got the cone, which
we'll render statically elsewhere. But then we've got streamers and confetti,
streaming out towards the upper right.

-}
type Confetti
    = Square
        { color : Color
        , rotationOffset : Float
        , rotations : Float
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
    Random.map3
        (\color rotationOffset rotations ->
            Square
                { color = color
                , rotationOffset = rotationOffset
                , rotations = rotations
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
        (normal 2 1)


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
        |> Particle.withLifetime (normal 1.5 0.25)
        |> Particle.at (Random.constant { x = x, y = y })
        |> Particle.heading
            (Random.map2 (\angle speed -> { angle = angle, speed = speed })
                (normal (degrees 47) (degrees 15))
                (normal 500 100)
            )
        |> Particle.withGravity 780


type alias Model =
    { system : System Confetti
    , mouse : ( Float, Float )
    }


type Msg
    = Burst Float Float
    | MouseMove Float Float
    | ParticleMsg (System.Msg Confetti)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Burst x y ->
            ( { model | system = System.burst (Random.list 1 (particleAt x y)) model.system }
            , Cmd.none
            )

        MouseMove x y ->
            ( { model | mouse = ( x, y ) }
            , Cmd.none
            )

        ParticleMsg particleMsg ->
            ( { model | system = System.update particleMsg model.system }
            , Cmd.none
            )


view : Model -> Document Msg
view model =
    let
        ( mouseX, mouseY ) =
            model.mouse
    in
    { title = "Confetti!"
    , body =
        [ Html.span
            [ style "position" "absolute"
            , style "left" (String.fromFloat (mouseX - 30) ++ "px")
            , style "top" (String.fromFloat (mouseY - 40) ++ "px")
            , style "cursor" "none"
            , style "font-size" "80px"
            , style "user-select" "none"
            ]
            [ Html.text "🎉" ]
        , System.view viewConfetti
            [ style "width" "100%"
            , style "height" "100vh"
            ]
            model.system
        ]
    }


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> ( { system = System.init (Random.initialSeed 0), mouse = ( 0, 0 ) }, Cmd.none )
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
                    , Browser.Events.onMouseMove
                        (Decode.map2 MouseMove
                            (Decode.field "clientX" Decode.float)
                            (Decode.field "clientY" Decode.float)
                        )
                    ]
        }



-- views


viewConfetti : Confetti -> Float -> Svg msg
viewConfetti confetti lifetime =
    let
        opacity =
            if Debug.log "lifetime" lifetime < 0.1 then
                lifetime * 10

            else
                1
    in
    case confetti of
        Square { color, rotationOffset, rotations } ->
            Svg.rect
                [ SAttrs.width "8px"
                , SAttrs.height "8px"
                , SAttrs.x "-4px"
                , SAttrs.y "-4px"
                , SAttrs.fill (fill color)
                , SAttrs.stroke (stroke color)
                , SAttrs.strokeWidth "1px"
                , SAttrs.opacity <| String.fromFloat opacity
                , SAttrs.transform <| "rotate(" ++ String.fromFloat ((rotations * lifetime + rotationOffset) * 360) ++ ")"
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
