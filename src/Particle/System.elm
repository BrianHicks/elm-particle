module Particle.System exposing (Msg, System, burst, init, sub, update, view)

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
        , seed : Random.Seed
        , particles : List (Particle a)
        }


init : Random.Seed -> System a
init seed =
    System
        { frame = Nothing
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


type Msg a
    = NewFrame Int (List (Particle a)) Random.Seed


update : Msg a -> System a -> System a
update (NewFrame frameTime particles seed) (System system) =
    case frameDelta frameTime system.frame of
        Nothing ->
            System
                { system
                    | frame = Just frameTime
                    , particles = particles ++ system.particles
                    , seed = seed
                }

        Just delta ->
            let
                -- TODO: this should check if the delta is greater than some
                -- value--a second seems fine--and wait for the next frame to
                -- update. This *should* take care of hanging when the browser
                -- tab is unfocused, and it will prevent churn on really slow
                -- computers as well.
                newParticles =
                    List.filterMap
                        (Particle.update (toFloat delta / 1000))
                        (particles ++ system.particles)
            in
            System
                { system
                    | frame =
                        if List.isEmpty newParticles then
                            Nothing

                        else
                            Just frameTime
                    , particles = newParticles
                    , seed = seed
                }


frameDelta : Int -> Maybe Int -> Maybe Int
frameDelta current maybePrevious =
    Maybe.map (\previous -> current - previous) maybePrevious


view : (a -> Float -> Svg msg) -> List (Html.Attribute msg) -> System a -> Html msg
view viewParticle attrs (System { particles }) =
    Svg.svg attrs (List.map (Particle.view viewParticle) particles)


sub : List (Int -> Generator (List (Particle a))) -> (Msg a -> msg) -> System a -> Sub msg
sub emitters msg ((System system) as outer) =
    if List.isEmpty emitters && List.isEmpty system.particles then
        Sub.none

    else
        Browser.Events.onAnimationFrame
            (\newTime ->
                let
                    frame =
                        Time.posixToMillis newTime

                    ( particles, seed ) =
                        emitterParticles emitters frame outer
                in
                msg <| NewFrame frame particles seed
            )


emitterParticles : List (Int -> Generator (List (Particle a))) -> Int -> System a -> ( List (Particle a), Random.Seed )
emitterParticles emitters newFrame (System { frame, seed }) =
    case frameDelta newFrame frame of
        Just delta ->
            emitters
                |> List.foldl
                    (\emitter ( particles, curSeed ) ->
                        Random.step (emitter delta) curSeed
                            |> Tuple.mapFirst ((++) particles)
                    )
                    ( [], seed )

        Nothing ->
            ( [], seed )
