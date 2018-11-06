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
        { seed : Random.Seed
        , particles : List (Particle a)
        }


init : Random.Seed -> System a
init seed =
    System
        { seed = seed
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
    = NewFrame Float (List (Particle a)) Random.Seed


update : Msg a -> System a -> System a
update (NewFrame delta particles seed) (System system) =
    -- TODO: this should check if the delta is greater than some
    -- value--a second seems fine--and wait for the next frame to
    -- update. This *should* take care of hanging when the browser
    -- tab is unfocused, and it will prevent churn on really slow
    -- computers as well.
    System
        { particles = List.filterMap (Particle.update delta) (particles ++ system.particles)
        , seed = seed
        }


view : (a -> Float -> Svg msg) -> List (Html.Attribute msg) -> System a -> Html msg
view viewParticle attrs (System { particles }) =
    Svg.svg attrs (List.map (Particle.view viewParticle) particles)


sub : List (Float -> Generator (List (Particle a))) -> (Msg a -> msg) -> System a -> Sub msg
sub emitters msg ((System system) as outer) =
    if List.isEmpty emitters && List.isEmpty system.particles then
        Sub.none

    else
        Browser.Events.onAnimationFrameDelta
            (\delta ->
                let
                    ( particles, seed ) =
                        emitterParticles delta emitters outer
                in
                msg <| NewFrame delta particles seed
            )


emitterParticles : Float -> List (Float -> Generator (List (Particle a))) -> System a -> ( List (Particle a), Random.Seed )
emitterParticles delta emitters (System { seed }) =
    emitters
        |> List.foldl
            (\emitter ( particles, curSeed ) ->
                Random.step (emitter delta) curSeed
                    |> Tuple.mapFirst ((++) particles)
            )
            ( [], seed )
