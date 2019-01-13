module Main exposing (main)

{-| HEADS UP! You can view this example alongside the running code at
<https://brianhicks.github.io/elm-particle/Firework.html>

We're going to make a firework, specifically one that looks like this stock
video: <https://videos.pexels.com/videos/fireworks-display-854341>

[ms]: https://mutant.tech/

-}

import Browser exposing (Document)
import Html exposing (Html)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Particle exposing (Particle)
import Particle.System as System exposing (System)
import Random exposing (Generator)
import Random.Extra
import Random.Float exposing (normal)
import Svg exposing (Svg)
import Svg.Attributes as SAttrs


type Firework
    = Fizzler Color


type Color
    = Red
    | Green
    | Blue


fizzler : Color -> Generator (Particle Firework)
fizzler color =
    Particle.init (Random.constant (Fizzler color))
        |> Particle.withDirection (Random.map degrees (Random.float 0 360))
        |> Particle.withSpeed (Random.map (clamp 0 200) (normal 100 100))
        |> Particle.withLifetime (normal 3 0.25)


fireworkAt : Color -> Float -> Float -> Generator (List (Particle Firework))
fireworkAt color x y =
    fizzler color
        |> Particle.withLocation (Random.constant { x = x, y = y })
        |> Particle.withGravity 50
        |> Particle.withDrag
            (\_ ->
                { coefficient = 1
                , density = 0.015
                , area = 3
                }
            )
        |> Random.list 150


type alias Model =
    System Firework


type Msg
    = ParticleMsg (System.Msg Firework)
    | Detonate


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ParticleMsg inner ->
            ( System.update inner model, Cmd.none )

        Detonate ->
            ( System.burst
                (Random.Extra.andThen3 fireworkAt
                    (Random.uniform Red [ Green, Blue ])
                    (normal 300 100)
                    (normal 300 100)
                )
                model
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    Html.main_ []
        [ Html.button
            [ onClick Detonate
            , style "display" "block"
            ]
            [ Html.text "Detonate!" ]
        , System.view fireworkView
            [ style "width" "600px"
            , style "height" "600px"
            , style "background-color" "#0F0F0F"
            ]
            model
        ]


fireworkView : Particle Firework -> Svg msg
fireworkView particle =
    case Particle.data particle of
        Fizzler color ->
            let
                length =
                    max 2 (Particle.speed particle / 15)

                ( hue, saturation, luminance ) =
                    toHsl color

                maxLuminance =
                    100

                luminanceDelta =
                    maxLuminance - luminance
            in
            Svg.ellipse
                [ -- location within the burst
                  SAttrs.cx (String.fromFloat (length / 2))
                , SAttrs.cy "0"

                -- size, smeared by motion
                , SAttrs.rx (String.fromFloat length)
                , SAttrs.ry "2"
                , SAttrs.transform ("rotate(" ++ String.fromFloat (Particle.directionDegrees particle) ++ ")")

                -- color!
                , SAttrs.fill
                    (hslString
                        hue
                        saturation
                        (maxLuminance - luminanceDelta * (1 - Particle.lifetimePercent particle))
                    )
                ]
                []


{-| Using the tango palette:
<http://tango.freedesktop.org/Tango_Icon_Theme_Guidelines>
-}
toHsl : Color -> ( Float, Float, Float )
toHsl color =
    case color of
        Red ->
            -- scarlet red
            ( 0, 86, 55 )

        Green ->
            -- chameleon
            ( 90, 75, 55 )

        Blue ->
            -- sky blue
            ( 211, 49, 63 )


hslString : Float -> Float -> Float -> String
hslString hue saturation luminance =
    "hsl("
        ++ String.fromFloat hue
        ++ ","
        ++ String.fromFloat saturation
        ++ "%,"
        ++ String.fromFloat luminance
        ++ "%)"


main : Program () (System Firework) Msg
main =
    Browser.element
        { init = \_ -> ( System.init (Random.initialSeed 0), Cmd.none )
        , update = update
        , view = view
        , subscriptions = \model -> System.sub [] ParticleMsg model
        }
