module Main exposing (Model, Msg(..), main, update, view)

-- TODO: make these water droplets a bit nicer, and document this whole thing
-- TODO: oh, and clean up the imports

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
    { system : System Droplet }


type alias Droplet =
    { color : String
    , radius : Float
    }


type Msg
    = ParticleMsg (System.Msg Droplet)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ParticleMsg particleMsg ->
            ( { model | system = System.update particleMsg model.system }
            , Cmd.none
            )


view : Model -> Document Msg
view model =
    { title = "Water!"
    , body =
        [ System.view viewDroplet
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
            \model -> Sub.batch [ System.sub [ waterEmitter ] ParticleMsg model.system ]
        }



-- emitters


waterEmitter : Float -> Generator (List (Particle Droplet))
waterEmitter delta =
    Random.list (ceiling (delta / 1000))
        (Random.map3
            (\heading color radius ->
                Particle.init (Droplet color radius) 1
                    |> Particle.at { x = 500, y = 500 }
                    |> Particle.heading heading
                    |> Particle.withGravity 980
            )
            (genHeading 45 600)
            genColor
            genRadius
        )



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



-- views


viewDroplet : Droplet -> Float -> Svg msg
viewDroplet { color, radius } lifetime =
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
