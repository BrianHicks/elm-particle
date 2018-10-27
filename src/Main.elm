module Main exposing (Model, Msg(..), main, update, view)

import Browser exposing (Document)
import Browser.Events
import Html exposing (Html)
import Html.Attributes exposing (style)
import Particle exposing (Particle)
import Svg
import Task
import Time exposing (Posix)


type alias Model =
    { timeNow : Posix
    , particles : List Particle
    }


type Msg
    = TimeNow Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TimeNow timeNow ->
            let
                delta =
                    -- deal with the initial 0 state
                    if Time.posixToMillis model.timeNow == 0 then
                        0

                    else
                        Time.posixToMillis timeNow - Time.posixToMillis model.timeNow
            in
            ( { model
                | timeNow = timeNow
                , particles = List.filterMap (Particle.update (toFloat delta / 1000)) model.particles
              }
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
            (List.map Particle.view model.particles)
        ]
    }


main : Program () Model Msg
main =
    Browser.document
        { init =
            \_ ->
                ( { timeNow = Time.millisToPosix 0
                  , particles =
                        [ Particle.init 1
                            |> Particle.at { x = 50, y = 50 }
                            |> Particle.heading { x = 100, y = -200 }
                            |> Particle.withGravity 980
                        ]
                  }
                , Task.perform TimeNow Time.now
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
