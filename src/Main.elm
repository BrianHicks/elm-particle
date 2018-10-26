module Main exposing (..)

import Browser exposing (Document)
import Browser.Events
import Html
import Task
import Time exposing (Posix)


type alias Model =
    { timeNow : Posix }


type Msg
    = TimeNow Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TimeNow timeNow ->
            ( { model | timeNow = timeNow }, Cmd.none )


view : Model -> Document Msg
view model =
    { title = "Particles!"
    , body = [ model.timeNow |> Time.posixToMillis |> String.fromInt |> Html.text ]
    }


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> ( { timeNow = Time.millisToPosix 0 }, Task.perform TimeNow Time.now )
        , view = view
        , update = update
        , subscriptions = \_ -> Browser.Events.onAnimationFrame TimeNow
        }
