module Water exposing (main)

{-| HEADS UP! You can view this example alongside the running code at
<https://brianhicks.github.io/elm-particle/Water.html>

Generate some water coming from a hose or another source. This example
mostly demonstrates emitters, and does just enough generation to get something
nice looking.

-}

import Browser
import Html exposing (Html)
import Html.Attributes as Attrs exposing (style)
import Particle exposing (Particle)
import Particle.System as System exposing (System)
import Random exposing (Generator)
import Random.Float exposing (normal)
import Svg exposing (Svg)
import Svg.Attributes as SAttrs


{-| This `main` is as minimal as possible. The thing to pay attention to is the
call to `System.sub` below, which contains our emitter.
-}
main : Program () (System Droplet) (System.Msg Droplet)
main =
    Browser.element
        { init = \_ -> ( System.init (Random.initialSeed 0), Cmd.none )
        , view = view
        , update = \msg system -> ( System.update msg system, Cmd.none )
        , subscriptions =
            \system -> System.sub [ waterEmitter ] identity system
        }



-- emitters


type alias Droplet =
    { color : String
    , radius : Float
    }


droplet : Generator Droplet
droplet =
    Random.map2 Droplet
        (Random.uniform "#E3F2FD"
            [ "#BBDEFB"
            , "#90CAF9"
            , "#64B5F6"
            , "#42A5F5"
            , "#2196F3"
            , "#1E88E5"
            , "#1976D2"
            , "#1565C0"
            , "#0D47A1"
            ]
        )
        (normal 20 5)


{-| Emitters take the delta (in milliseconds )since the last update. This is so
you can emit the right number of particles. This emitter emits about 60
particles per second.
-}
waterEmitter : Float -> Generator (List (Particle Droplet))
waterEmitter delta =
    Particle.init droplet
        |> Particle.withLifetime (Random.constant 1)
        |> Particle.withLocation (Random.constant { x = 50, y = 500 })
        |> Particle.withDirection (normal (degrees 45) (degrees 10))
        |> Particle.withSpeed (normal 600 100)
        |> Particle.withGravity 980
        |> Random.list (ceiling (delta * (60 / 1000)))



-- views


view : System Droplet -> Html msg
view system =
    Html.main_ []
        [ System.view viewDroplet
            [ style "width" "100%"
            , style "height" "98vh"
            ]
            system
        ]


viewDroplet : Particle Droplet -> Svg msg
viewDroplet particle =
    let
        { color, radius } =
            Particle.data particle
    in
    Svg.circle
        [ SAttrs.r (String.fromFloat radius)
        , SAttrs.fill color
        ]
        []
