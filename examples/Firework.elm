module Main exposing (main)

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
    = Dot
    | Line Int


firework : Generator Firework
firework =
    Random.Extra.frequency
        ( 1 / 8, Random.map (Line << max 3 << floor) (normal 15 6) )
        [ ( 7 / 8, Random.constant Dot ) ]


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
                (Particle.init firework
                    |> Particle.withLocation (Random.constant { x = 150, y = 300 })
                    |> Particle.withDirection (normal 0 (degrees 15))
                    |> Particle.withSpeed (normal 500 100)
                    |> Particle.withLifetime (Random.constant 1)
                    |> Particle.withGravity 100
                    |> Particle.withDrag
                        (\_ ->
                            { coefficient = 1
                            , density = 0.001226
                            , area = 3
                            }
                        )
                    |> Random.list 100
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
            [ style "width" "300px"
            , style "height" "300px"
            , style "background-color" "#000000"
            ]
            model
        ]


fireworkView : Particle Firework -> Svg msg
fireworkView particle =
    case Particle.data particle of
        Dot ->
            Svg.circle
                [ SAttrs.r "1"
                , SAttrs.fill "#FFF176"
                ]
                []

        Line length ->
            Svg.rect
                [ SAttrs.height "2"
                , SAttrs.width (String.fromInt length)
                , SAttrs.fill "#FFF176"
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
