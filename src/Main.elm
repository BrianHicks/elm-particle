module Main exposing (Model, Msg(..), main, update, view)

import Browser exposing (Document)
import Browser.Events
import Html exposing (Html)
import Html.Attributes as Attrs exposing (style)
import Particle exposing (Particle)
import Random exposing (Generator)
import Svg exposing (Svg)
import Svg.Attributes as SAttrs
import Task
import Time exposing (Posix)


type alias Model =
    { timeNow : Maybe Posix
    , particles : List (Particle String)
    }


type Msg
    = NewParticle (List (Particle String))
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


viewColoredCircleParticle : String -> Float -> Svg msg
viewColoredCircleParticle color _ =
    Svg.circle
        [ SAttrs.r "20"
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
                            Random.map2
                                (\heading color ->
                                    Particle.init color 1.5
                                        |> Particle.at { x = 1024 / 2, y = 768 / 8 }
                                        |> Particle.heading heading
                                        |> Particle.withGravity 980
                                )
                                genHeading
                                genColor
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
    Random.uniform "red" [ "green", "blue" ]


genHeading : Generator { angle : Float, speed : Float }
genHeading =
    Random.map2 (\angle speed -> { angle = degrees angle, speed = speed })
        (Random.float (180 + 45) (360 - 45))
        (Random.float 300 500)
