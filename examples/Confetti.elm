module Confetti exposing (main)

-- TODO: make these confetti particles much nicer, and document this whole thing
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
    { system : System Confetti }


type alias Confetti =
    { color : String
    , radius : Float
    }


type Msg
    = Burst Float Float
    | ParticleMsg (System.Msg Confetti)


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
    { title = "Confetti!"
    , body =
        [ System.view viewConfetti
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
                    [ System.sub [] ParticleMsg model.system
                    , Browser.Events.onClick
                        (Decode.map2 Burst
                            (Decode.field "clientX" Decode.float)
                            (Decode.field "clientY" Decode.float)
                        )
                    ]
        }



-- generators


particleAt : Float -> Float -> Generator (Particle Confetti)
particleAt x y =
    Random.map3
        (\heading color radius ->
            Particle.init (Confetti color radius) 1.5
                |> Particle.at { x = x, y = y }
                |> Particle.heading heading
                |> Particle.withGravity 980
        )
        (genHeading 0 400)
        genColor
        genRadius


genColor : Generator String
genColor =
    Random.uniform "red" [ "green", "blue", "yellow" ]


genRadius : Generator Float
genRadius =
    normal 20 5


genHeading : Float -> Float -> Generator { angle : Float, speed : Float }
genHeading angleCenter powerCenter =
    Random.map2 (\angle speed -> { angle = degrees angle, speed = speed })
        (normal angleCenter 15)
        (normal powerCenter 100)



-- views


viewConfetti : Confetti -> Float -> Svg msg
viewConfetti { color, radius } lifetime =
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
