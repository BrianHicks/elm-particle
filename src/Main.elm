module Main exposing (Model, Msg(..), main, update, view)

import Browser exposing (Document)
import Browser.Events
import Html exposing (Html)
import Html.Attributes as Attrs exposing (style)
import Particle exposing (Particle)
import Random exposing (Generator)
import Random.Float exposing (normal)
import Svg exposing (Svg)
import Svg.Attributes as SAttrs
import Task
import Time exposing (Posix)


type alias Model =
    { timeNow : Maybe Posix
    , particles : List (Particle ( String, Float ))
    }


type Msg
    = NewParticle (List (Particle ( String, Float )))
    | TimeNow Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewParticle particles ->
            ( { model | particles = particles ++ model.particles }, Cmd.none )

        TimeNow timeNow ->
            case model.timeNow of
                Just last ->
                    ( { model
                        | timeNow = Just timeNow
                        , particles =
                            List.filterMap
                                (Particle.update (toFloat (Time.posixToMillis timeNow - Time.posixToMillis last) / 1000))
                                model.particles
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( { model | timeNow = Just timeNow }
                    , Cmd.none
                    )


view : Model -> Document Msg
view model =
    { title = "Particles!"
    , body =
        [ Svg.svg
            [ style "width" "1024px"
            , style "height" "768px"
            ]
            (List.map (Particle.view viewColoredCircleParticle) model.particles)
        ]
    }


viewColoredCircleParticle : ( String, Float ) -> Float -> Svg msg
viewColoredCircleParticle ( color, radius ) _ =
    Svg.circle
        [ SAttrs.r (String.fromFloat radius)
        , SAttrs.fill color
        ]
        []


viewTextParticle : () -> Float -> Svg msg
viewTextParticle _ remaining =
    Svg.text_
        [ SAttrs.dx "0"
        , SAttrs.dy "20"
        ]
        [ Svg.text (String.fromFloat remaining) ]


main : Program () Model Msg
main =
    Browser.document
        { init =
            \_ ->
                ( { timeNow = Nothing, particles = [] }
                , Cmd.batch
                    [ Task.perform TimeNow Time.now
                    , Random.generate NewParticle <|
                        Random.list 100 <|
                            Random.map3
                                (\heading color radius ->
                                    Particle.init ( color, radius ) 1.5
                                        |> Particle.at { x = 1024 / 2, y = 768 / 8 }
                                        |> Particle.heading heading
                                        |> Particle.withGravity 980
                                )
                                genHeading
                                genColor
                                genRadius
                    ]
                )
        , view = view
        , update = update
        , subscriptions =
            \model ->
                if List.isEmpty model.particles then
                    Sub.none

                else
                    Browser.Events.onAnimationFrame TimeNow
        }



-- generators


genColor : Generator String
genColor =
    Random.uniform "#E3F2FD" [ "#BBDEFB", "#90CAF9", "#64B5F6", "#42A5F5", "#2196F3", "#1E88E5", "#1976D2", "#1565C0", "#0D47A1" ]


genRadius : Generator Float
genRadius =
    normal 20 5


genHeading : Generator { angle : Float, speed : Float }
genHeading =
    Random.map2 (\angle speed -> { angle = degrees angle, speed = speed })
        (normal 270 30)
        (Random.float 300 500)
