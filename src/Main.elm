module Main exposing (Model, Msg(..), main, update, view)

import Browser exposing (Document)
import Browser.Events
import Html exposing (Html)
import Html.Attributes as Attrs exposing (style)
import Json.Decode as Decode
import Particle exposing (Particle)
import Particle.System as System exposing (System)
import Random exposing (Generator)
import Random.Float exposing (normal)
import Svg exposing (Svg)
import Svg.Attributes as SAttrs
import Task
import Time exposing (Posix)


type alias Model =
    { system : System ParticleInfo
    , previousTime : Maybe Time.Posix
    }


type alias ParticleInfo =
    { color : String, radius : Float }


type Msg
    = NewParticle (List (Particle ParticleInfo))
    | Burst Float Float
    | TimeNow Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewParticle particles ->
            ( { model | system = System.add particles model.system }, Cmd.none )

        Burst x y ->
            ( model
            , Random.generate NewParticle <|
                Random.list 100 <|
                    Random.map3
                        (\heading color radius ->
                            Particle.init (ParticleInfo color radius) 1.5
                                |> Particle.at { x = x, y = y }
                                |> Particle.heading heading
                                |> Particle.withGravity 980
                        )
                        (genHeading 0 300)
                        genColor
                        genRadius
            )

        TimeNow timeNow ->
            ( { model
                | system = System.update timeNow model.system
                , previousTime = Just timeNow
              }
            , case model.previousTime of
                Nothing ->
                    Cmd.none

                Just previousTime ->
                    Random.generate NewParticle <|
                        Random.list (round ((100.0 / 1000.0) * toFloat (Time.posixToMillis timeNow - Time.posixToMillis previousTime))) <|
                            Random.map3
                                (\heading color radius ->
                                    Particle.init (ParticleInfo color radius) 1.5
                                        |> Particle.at { x = 500, y = 500 }
                                        |> Particle.heading heading
                                        |> Particle.withGravity 980
                                )
                                (genHeading 45 600)
                                genColor
                                genRadius
            )


view : Model -> Document Msg
view model =
    { title = "Particles!"
    , body =
        [ System.view viewColoredCircleParticle
            [ style "width" "100%"
            , style "height" "100vh"
            ]
            model.system
        ]
    }


viewColoredCircleParticle : ParticleInfo -> Float -> Svg msg
viewColoredCircleParticle { color, radius } lifetime =
    let
        control =
            2
    in
    Svg.circle
        [ SAttrs.r (String.fromFloat radius)
        , SAttrs.fill color
        , SAttrs.opacity <| String.fromFloat <| 1 - cubicBezier 1 0.01 0.92 -0.5 lifetime
        ]
        []


cubicBezier : Float -> Float -> Float -> Float -> Float -> Float
cubicBezier p0 p1 p2 p3 t =
    let
        oneMinusT =
            1 - t
    in
    oneMinusT ^ 3 * p0 + (3 * oneMinusT ^ 2 * t * p1) + (3 * oneMinusT * t ^ 2 * p2) + (t ^ 3 * p3)


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
                ( { system = System.init, previousTime = Nothing }
                , Task.perform TimeNow Time.now
                )
        , view = view
        , update = update
        , subscriptions =
            \model ->
                Sub.batch
                    [ System.sub TimeNow model.system
                    , Browser.Events.onAnimationFrame TimeNow
                    , Browser.Events.onClick
                        (Decode.map2 Burst
                            (Decode.field "clientX" Decode.float)
                            (Decode.field "clientY" Decode.float)
                        )
                    ]
        }



-- generators


genColor : Generator String
genColor =
    Random.uniform "#E3F2FD" [ "#BBDEFB", "#90CAF9", "#64B5F6", "#42A5F5", "#2196F3", "#1E88E5", "#1976D2", "#1565C0", "#0D47A1" ]


genRadius : Generator Float
genRadius =
    normal 20 5


genHeading : Float -> Float -> Generator { angle : Float, speed : Float }
genHeading angleCenter powerCenter =
    Random.map2 (\angle speed -> { angle = degrees angle, speed = speed })
        (normal angleCenter 10)
        (normal powerCenter 100)
