module System exposing (System, add, init, update, view)

{-| -}

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
update newTime (System time particles) =
    case time of
        Nothing ->
            System (Just newTime) particles

        Just oldTime ->
            System
                (Just newTime)
                (List.filterMap
                    (Particle.update (toFloat (Time.posixToMillis newTime - Time.posixToMillis oldTime) / 1000))
                    particles
                )


view : (a -> Float -> Svg msg) -> List (Html.Attribute msg) -> System a -> Html msg
view viewParticle attrs (System _ particles) =
    Svg.svg attrs (List.map (Particle.view viewParticle) particles)
