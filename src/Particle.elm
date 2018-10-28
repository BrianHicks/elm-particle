module Particle exposing
    ( Particle, Coord, init, at, heading, withGravity
    , update, view
    )

{-|

@docs Particle, Coord, init, at, heading, withGravity

@docs update, view

-}

import Svg exposing (Svg)
import Svg.Attributes as Attrs


{-| -}
type Particle a
    = Particle
        { data : a
        , position : Coord
        , velocity : Coord
        , acceleration : Coord
        , originalLifetime : Float
        , lifetime : Float
        }



-- constructors


init : a -> Float -> Particle a
init data lifetime =
    Particle
        { data = data
        , position = { x = 0, y = 0 }
        , velocity = { x = 0, y = 0 }
        , acceleration = { x = 0, y = 0 }
        , originalLifetime = lifetime
        , lifetime = lifetime
        }


at : Coord -> Particle a -> Particle a
at position (Particle particle) =
    Particle { particle | position = position }


{-| 0° is straight up
-}
heading : { speed : Float, angle : Float } -> Particle a -> Particle a
heading { speed, angle } (Particle particle) =
    Particle
        { particle
            | velocity =
                { x = speed * cos (angle - degrees 90)
                , y = speed * sin (angle - degrees 90)
                }
        }


withGravity : Float -> Particle a -> Particle a
withGravity pxPerSecond (Particle ({ acceleration } as particle)) =
    Particle { particle | acceleration = { acceleration | y = pxPerSecond } }


{-| -}
type alias Coord =
    { x : Float, y : Float }


{-| -}
update : Float -> Particle a -> Maybe (Particle a)
update deltaSeconds (Particle { data, position, velocity, acceleration, originalLifetime, lifetime }) =
    if lifetime - deltaSeconds <= 0 then
        Nothing

    else
        (Just << Particle)
            { data = data
            , position =
                { x = position.x + velocity.x * deltaSeconds + acceleration.x * deltaSeconds * deltaSeconds / 2
                , y = position.y + velocity.y * deltaSeconds + acceleration.y * deltaSeconds * deltaSeconds / 2
                }
            , velocity =
                { x = velocity.x + acceleration.x * deltaSeconds
                , y = velocity.y + acceleration.y * deltaSeconds
                }
            , acceleration = acceleration
            , originalLifetime = originalLifetime
            , lifetime = lifetime - deltaSeconds
            }


{-| -}
view : (a -> Float -> Svg msg) -> Particle a -> Svg msg
view viewData (Particle { data, position, originalLifetime, lifetime }) =
    Svg.g
        [ Attrs.transform ("translate(" ++ String.fromFloat position.x ++ "," ++ String.fromFloat position.y ++ ")") ]
        [ viewData data (lifetime / originalLifetime) ]
