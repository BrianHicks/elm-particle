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
        { frame : Maybe Int
        , lastDelta : Maybe Int
        , kickstarting : Bool
        , seed : Random.Seed
        , particles : List (Particle a)
        }


init : Random.Seed -> System a
init seed =
    System
        { frame = Nothing
        , lastDelta = Nothing
        , kickstarting = True
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
    case Debug.log "delta" system.lastDelta of
        Just delta ->
            let
                ( particles, nextSeed ) =
                    Random.step
                        (Random.list
                            (round ((perSecond / 1000) * toFloat delta))
                            generator
                        )
                        system.seed
            in
            System
                { system
                    | particles = particles ++ system.particles
                    , seed = nextSeed
                    , kickstarting = True
                }

        _ ->
            System { system | kickstarting = True }


type Msg
    = NewFrame Time.Posix


update : Msg -> System a -> System a
update msg (System system) =
    case msg of
        NewFrame frameTime ->
            updateNewFrame frameTime (System system)


updateNewFrame : Time.Posix -> System a -> System a
updateNewFrame frameTime (System system) =
    let
        newFrame =
            Time.posixToMillis frameTime
    in
    case system.frame of
        Nothing ->
            System { system | frame = Just newFrame }

        Just oldFrame ->
            let
                delta =
                    newFrame - oldFrame

                -- TODO: this should check if the delta is greater than some
                -- value--a second seems fine--and wait for the next frame to
                -- update. This *should* take care of hanging when the browser
                -- tab is unfocused, and it will prevent churn on really slow
                -- computers as well.
                newParticles =
                    List.filterMap
                        (Particle.update (toFloat (newFrame - oldFrame) / 1000))
                        system.particles

                emptyParticles =
                    List.isEmpty newParticles
            in
            System
                { system
                    | frame =
                        if emptyParticles && not system.kickstarting then
                            Nothing

                        else
                            Just newFrame
                    , lastDelta = Just delta
                    , particles = newParticles
                    , kickstarting = False
                }


view : (a -> Float -> Svg msg) -> List (Html.Attribute msg) -> System a -> Html msg
view viewParticle attrs (System { particles }) =
    Svg.svg attrs (List.map (Particle.view viewParticle) particles)


sub : (Msg -> msg) -> System a -> Sub msg
sub msg (System system) =
    if system.particles /= [] || system.kickstarting then
        Browser.Events.onAnimationFrame (msg << NewFrame)

    else
        Sub.none
