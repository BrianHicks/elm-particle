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
    = Fizzler
    | Streamer


fizzler : Generator (Particle Firework)
fizzler =
    Particle.init (Random.constant Fizzler)
        |> Particle.withDirection (Random.map degrees (Random.float 0 360))
        |> Particle.withSpeed (Random.map (clamp 0 400) (normal 200 200))
        |> Particle.withLifetime (Random.constant 1)


streamer : Generator (Particle Firework)
streamer =
    Particle.init (Random.constant Streamer)
        |> Particle.withDirection (Random.map degrees (Random.float 0 360))
        |> Particle.withSpeed (Random.map (clamp 0 800) (normal 400 400))
        |> Particle.withLifetime (Random.constant 1.5)


firework : Generator (Particle Firework)
firework =
    Random.Extra.frequency ( 1, fizzler ) [ ( 1, streamer ) ]


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
                (firework
                    |> Particle.withLocation (Random.constant { x = 300, y = 300 })
                    |> Particle.withGravity 50
                    |> Particle.withDrag
                        (\_ ->
                            { coefficient = 1
                            , density = 0.015
                            , area = 3
                            }
                        )
                    |> Random.list 300
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
        Fizzler ->
            Svg.circle
                [ SAttrs.r "3"
                , SAttrs.fill "#DCF1FF"
                ]
                []

        Streamer ->
            Svg.rect
                [ SAttrs.height "3"
                , SAttrs.width "15"
                , SAttrs.fill "#DCF1FF"
                , SAttrs.transform <|
                    "rotate("
                        ++ String.fromFloat (Particle.directionDegrees particle)
                        ++ ")"
                ]
                []


main : Program () (System Firework) Msg
main =
    Browser.element
        { init = \_ -> ( System.init (Random.initialSeed 0), Cmd.none )
        , update = update
        , view = view
        , subscriptions = \model -> System.sub [] ParticleMsg model
        }
