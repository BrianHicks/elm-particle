module Particle.System exposing (Msg, System, burst, init, stream, sub, update, view)

{-| -}

import Browser.Events
import Html exposing (Html)
import Particle exposing (Particle)
import Random exposing (Generator)
import Svg exposing (Svg)
import Time


type System a
    = System
        { lastFrame : Maybe Time.Posix
        , seed : Random.Seed
        , particles : List (Particle a)
        }


init : Random.Seed -> System a
init seed =
    System
        { lastFrame = Nothing
        , seed = seed
        , particles = []
        }


burst : Int -> Generator (Particle a) -> System a -> System a
burst amount generator (System system) =
    let
        ( particles, nextSeed ) =
            Random.step (Random.list amount generator) system.seed
    in
    System { system | particles = particles ++ system.particles, seed = nextSeed }


stream : Float -> Generator (Particle a) -> System a -> System a
stream perSecond generator (System system) =
    System system


type Msg
    = NewFrame Time.Posix


update : Msg -> System a -> System a
update msg (System system) =
    case msg of
        NewFrame frameTime ->
            updateNewFrame frameTime (System system)


updateNewFrame : Time.Posix -> System a -> System a
updateNewFrame frameTime (System system) =
    case system.lastFrame of
        Nothing ->
            System { system | lastFrame = Just frameTime }

        Just oldTime ->
            let
                -- TODO: this should check if the delta is greater than some
                -- value--a second seems fine--and wait for the next frame to
                -- update. This *should* take care of hanging when the browser
                -- tab is unfocused, and it will prevent churn on really slow
                -- computers as well.
                newParticles =
                    List.filterMap
                        (Particle.update (toFloat (Time.posixToMillis frameTime - Time.posixToMillis oldTime) / 1000))
                        system.particles
            in
            System
                { system
                    | lastFrame =
                        if List.isEmpty newParticles then
                            Nothing

                        else
                            Just frameTime
                    , particles = newParticles
                }


view : (a -> Float -> Svg msg) -> List (Html.Attribute msg) -> System a -> Html msg
view viewParticle attrs (System { particles }) =
    Svg.svg attrs (List.map (Particle.view viewParticle) particles)


sub : (Msg -> msg) -> System a -> Sub msg
sub msg (System system) =
    case system.particles of
        [] ->
            Sub.none

        _ ->
            Browser.Events.onAnimationFrame (msg << NewFrame)
