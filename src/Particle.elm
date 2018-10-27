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
type Particle
    = Particle
        { position : Coord
        , velocity : Coord
        , acceleration : Coord
        , lifetime : Float
        }



-- constructors


init : Float -> Particle
init lifetime =
    Particle
        { position = { x = 0, y = 0 }
        , velocity = { x = 0, y = 0 }
        , acceleration = { x = 0, y = 0 }
        , lifetime = lifetime
        }


at : Coord -> Particle -> Particle
at position (Particle particle) =
    Particle { particle | position = position }


{-| TODO: make this an angle and magnitude instead of a coordinate
-}
heading : Coord -> Particle -> Particle
heading velocity (Particle particle) =
    Particle { particle | velocity = velocity }


withGravity : Float -> Particle -> Particle
withGravity pxPerSecond (Particle ({ acceleration } as particle)) =
    Particle { particle | acceleration = { acceleration | y = pxPerSecond } }


{-| -}
type alias Coord =
    { x : Float, y : Float }


{-| -}
update : Float -> Particle -> Maybe Particle
update deltaSeconds (Particle { position, velocity, acceleration, lifetime }) =
    if lifetime - deltaSeconds <= 0 then
        Nothing

    else
        (Just << Particle)
            { position =
                { x = position.x + velocity.x * deltaSeconds + acceleration.x * deltaSeconds * deltaSeconds / 2
                , y = position.y + velocity.y * deltaSeconds + acceleration.y * deltaSeconds * deltaSeconds / 2
                }
            , velocity =
                { x = velocity.x + acceleration.x * deltaSeconds
                , y = velocity.y + acceleration.y * deltaSeconds
                }
            , acceleration = acceleration
            , lifetime = lifetime - deltaSeconds
            }


{-| -}
view : Particle -> Svg msg
view (Particle { position, lifetime }) =
    Svg.rect
        [ Attrs.width "10px"
        , Attrs.height "10px"
        , Attrs.x (String.fromFloat position.x ++ "px")
        , Attrs.y (String.fromFloat position.y ++ "px")
        ]
        [ Svg.text_ [] [ Svg.text (String.fromFloat lifetime) ] ]
