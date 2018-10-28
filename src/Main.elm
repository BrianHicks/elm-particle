module Main exposing (Model, Msg(..), main, update, view)

import Browser exposing (Document)
import Browser.Events
import Html exposing (Html)
import Html.Attributes as Attrs exposing (style)
import Json.Decode as Decode
import Particle exposing (Particle)
import Random exposing (Generator)
import Random.Float exposing (normal)
import Svg exposing (Svg)
import Svg.Attributes as SAttrs
import System exposing (System)
import Task
import Time exposing (Posix)


type alias Model =
    { system : System ParticleInfo }


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
                        genHeading
                        genColor
                        genRadius
            )

        TimeNow timeNow ->
            ( { model | system = System.update timeNow model.system }, Cmd.none )


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
viewColoredCircleParticle { color, radius } _ =
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
        { init = \_ -> ( { system = System.init }, Cmd.none )
        , view = view
        , update = update
        , subscriptions =
            \model ->
                Sub.batch
                    [ System.sub TimeNow model.system
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


genHeading : Generator { angle : Float, speed : Float }
genHeading =
    Random.map2 (\angle speed -> { angle = degrees angle, speed = speed })
        (normal 0 30)
        (Random.float 300 500)
