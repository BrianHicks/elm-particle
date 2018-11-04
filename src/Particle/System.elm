module Particle.System exposing (System, add, init, sub, update, view)

{-| -}

import Browser.Events
import Html exposing (Html)
import Particle exposing (Particle)
import Svg exposing (Svg)
import Time


type System a
    = System (Maybe Time.Posix) (List (Particle a))


init : System a
init =
    System Nothing []


add : List (Particle a) -> System a -> System a
add particles (System time system) =
    System time (particles ++ system)


update : Time.Posix -> System a -> System a
update newTime (System maybeTime particles) =
    case maybeTime of
        Nothing ->
            System (Just newTime) particles

        Just oldTime ->
            let
                newParticles =
                    List.filterMap
                        (Particle.update (toFloat (Time.posixToMillis newTime - Time.posixToMillis oldTime) / 1000))
                        particles
            in
            System
                (if List.isEmpty newParticles then
                    Nothing

                 else
                    Just newTime
                )
                newParticles


view : (a -> Float -> Svg msg) -> List (Html.Attribute msg) -> System a -> Html msg
view viewParticle attrs (System _ particles) =
    Svg.svg attrs (List.map (Particle.view viewParticle) particles)


sub : (Time.Posix -> msg) -> System a -> Sub msg
sub msg (System _ particles) =
    case particles of
        [] ->
            Sub.none

        _ ->
            Browser.Events.onAnimationFrame msg
