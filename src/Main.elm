module Main exposing (..)

import Browser exposing (Document)
import Browser.Events
import Html exposing (Html)
import Html.Attributes exposing (style)
import Task
import Time exposing (Posix)


type alias Model =
    { timeNow : Posix
    , particles : List Particle
    }


type alias Position =
    { x : Float, y : Float }


type alias Velocity =
    { x : Float, y : Float }


type alias Acceleration =
    { x : Float, y : Float }


type alias Particle =
    { position : Position
    , velocity : Velocity
    , acceleration : Acceleration
    , lifetime : Float
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
                , particles = List.filterMap (updateParticle (toFloat delta / 1000)) model.particles
              }
            , Cmd.none
            )


updateParticle : Float -> Particle -> Maybe Particle
updateParticle deltaSeconds { position, velocity, acceleration, lifetime } =
    let
        newLifetime =
            lifetime - deltaSeconds
    in
    if newLifetime < 0 then
        Nothing
    else
        Just
            { position =
                { x = position.x + velocity.x * deltaSeconds + acceleration.x * deltaSeconds * deltaSeconds / 2
                , y = position.y + velocity.y * deltaSeconds + acceleration.y * deltaSeconds * deltaSeconds / 2
                }
            , velocity =
                { x = velocity.x + acceleration.x * deltaSeconds
                , y = velocity.y + acceleration.y * deltaSeconds
                }
            , acceleration = acceleration
            , lifetime = newLifetime
            }


view : Model -> Document Msg
view model =
    { title = "Particles!"
    , body = List.map viewParticle model.particles
    }


viewParticle : Particle -> Html msg
viewParticle { position, lifetime } =
    Html.div
        [ style "position" "absolute"
        , style "left" (String.fromFloat position.x ++ "px")
        , style "top" (String.fromFloat position.y ++ "px")
        ]
        [ Html.text (String.fromFloat lifetime) ]


main : Program () Model Msg
main =
    Browser.document
        { init =
            \_ ->
                ( { timeNow = Time.millisToPosix 0
                  , particles =
                        [ { position = Position 0 0
                          , velocity = Velocity 100 0
                          , acceleration = Acceleration 0 980
                          , lifetime = 1
                          }
                        ]
                  }
                , Task.perform TimeNow Time.now
                )
        , view = view
        , update = update
        , subscriptions = \_ -> Browser.Events.onAnimationFrame TimeNow
        }
