module Particle.System exposing (System, add, init, sub, update, view)

{-| -}

import Browser.Events
import Html exposing (Html)
import Particle exposing (Particle)
import Svg exposing (Svg)
import Time


type System a
    = System
        { lastFrame : Maybe Time.Posix
        , particles : List (Particle a)
        }


init : System a
init =
    System { lastFrame = Nothing, particles = [] }


add : List (Particle a) -> System a -> System a
add particles (System system) =
    System { system | particles = particles ++ system.particles }


update : Time.Posix -> System a -> System a
update newTime (System system) =
    case system.lastFrame of
        Nothing ->
            System { system | lastFrame = Just newTime }

        Just oldTime ->
            let
                -- TODO: this should check if the delta is greater than some
                -- value--a second seems fine--and wait for the next frame to
                -- update. This *should* take care of hanging when the browser
                -- tab is unfocused, and it will prevent churn on really slow
                -- computers as well.
                newParticles =
                    List.filterMap
                        (Particle.update (toFloat (Time.posixToMillis newTime - Time.posixToMillis oldTime) / 1000))
                        system.particles
            in
            System
                { system
                    | lastFrame =
                        if List.isEmpty newParticles then
                            Nothing

                        else
                            Just newTime
                    , particles = newParticles
                }


view : (a -> Float -> Svg msg) -> List (Html.Attribute msg) -> System a -> Html msg
view viewParticle attrs (System { particles }) =
    Svg.svg attrs (List.map (Particle.view viewParticle) particles)


sub : (Time.Posix -> msg) -> System a -> Sub msg
sub msg (System system) =
    case system.particles of
        [] ->
            Sub.none

        _ ->
            Browser.Events.onAnimationFrame msg
