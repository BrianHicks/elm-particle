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
    { system : System ParticleInfo }


type alias ParticleInfo =
    { color : String
    , radius : Float
    }


type Msg
    = Burst Float Float
    | ParticleMsg (System.Msg ParticleInfo)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Burst x y ->
            ( { model | system = System.burst 100 (particleAt x y) model.system }
            , Cmd.none
            )

        ParticleMsg particleMsg ->
            ( { model | system = System.update particleMsg model.system }
            , Cmd.none
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


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> ( { system = System.init (Random.initialSeed 0) }, Cmd.none )
        , view = view
        , update = update
        , subscriptions =
            \model ->
                Sub.batch
                    [ System.sub [ waterEmitter ] ParticleMsg model.system
                    , Browser.Events.onClick
                        (Decode.map2 Burst
                            (Decode.field "clientX" Decode.float)
                            (Decode.field "clientY" Decode.float)
                        )
                    ]
        }



-- emitters


waterEmitter : Int -> Generator (List (Particle ParticleInfo))
waterEmitter delta =
    Random.list (ceiling (toFloat delta / 1000))
        (Random.map3
            (\heading color radius ->
                Particle.init (ParticleInfo color radius) 1
                    |> Particle.at { x = 500, y = 500 }
                    |> Particle.heading heading
                    |> Particle.withGravity 980
            )
            (genHeading 45 600)
            genColor
            genRadius
        )



-- generators


particleAt : Float -> Float -> Generator (Particle ParticleInfo)
particleAt x y =
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



-- views


viewColoredCircleParticle : ParticleInfo -> Float -> Svg msg
viewColoredCircleParticle { color, radius } lifetime =
    Svg.circle
        [ SAttrs.r (String.fromFloat radius)
        , SAttrs.fill color
        , SAttrs.opacity <| String.fromFloat <| 1 - cubicBezier 1 0.01 0.92 -0.5 lifetime
        ]
        []



-- utils


cubicBezier : Float -> Float -> Float -> Float -> Float -> Float
cubicBezier p0 p1 p2 p3 t =
    let
        oneMinusT =
            1 - t
    in
    oneMinusT ^ 3 * p0 + (3 * oneMinusT ^ 2 * t * p1) + (3 * oneMinusT * t ^ 2 * p2) + (t ^ 3 * p3)
