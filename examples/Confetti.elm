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

What's it got? Well, in the Mutant Standard Emoji, we've got the cone–which
we'll render statically–bursting streamers and confetti towards the upper right.

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
    | Yellow
    | Green


{-| Generate a confetti square, using the color ratios seen in the Mutant
Standard Emoji.
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
            ( 1 / 5, Red )
            [ ( 1 / 5, Pink )
            , ( 1 / 5, Yellow )
            , ( 2 / 5, Green )
            ]
        )
        (normal 0 1)
        (normal 1 1)


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
        |> Particle.withLocation (Random.constant { x = x, y = y })
        |> Particle.withHeading
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
            ( { model | system = System.burst (Random.list 50 (particleAt x y)) model.system }
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
        [ System.view viewConfetti
            [ style "width" "100%"
            , style "height" "100vh"
            , style "z-index" "1"
            , style "position" "relative"
            , style "cursor" "none"
            ]
            model.system
        , Html.img
            [ Attrs.src "tada.png"
            , Attrs.width 64
            , Attrs.height 64
            , Attrs.alt "\"tada\" emoji from Mutant Standard"
            , style "position" "absolute"
            , style "left" (String.fromFloat (mouseX - 20) ++ "px")
            , style "top" (String.fromFloat (mouseY - 30) ++ "px")
            , style "user-select" "none"
            , style "cursor" "none"
            , style "z-index" "0"
            ]
            []
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
                [ SAttrs.width "10px"
                , SAttrs.height "10px"
                , SAttrs.x "-5px"
                , SAttrs.y "-5px"
                , SAttrs.rx "2px"
                , SAttrs.rx "2px"
                , SAttrs.fill (fill color)
                , SAttrs.stroke "black"
                , SAttrs.strokeWidth "4px"
                , SAttrs.opacity <| String.fromFloat opacity
                , SAttrs.transform <| "rotate(" ++ String.fromFloat ((rotations * lifetime + rotationOffset) * 360) ++ ")"
                ]
                []


fill : Color -> String
fill color =
    case color of
        Red ->
            "#D72D35"

        Pink ->
            "#F2298A"

        Yellow ->
            "#F2C618"

        Green ->
            "#2ACC42"
