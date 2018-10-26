module Particle exposing
    ( Particle, Coord, init, at, heading, withGravity
    , update, view
    )

{-|

@docs Particle, Coord, init, at, heading, withGravity

@docs update, view

-}

import Html exposing (Html)
import Html.Attributes exposing (style)


{-| -}
type alias Particle =
    { position : Coord
    , velocity : Coord
    , acceleration : Coord
    , lifetime : Float
    }



-- constructors


init : Float -> Particle
init lifetime =
    { position = { x = 0, y = 0 }
    , velocity = { x = 0, y = 0 }
    , acceleration = { x = 0, y = 0 }
    , lifetime = lifetime
    }


at : Coord -> Particle -> Particle
at position particle =
    { particle | position = position }


{-| TODO: make this an angle and magnitude instead of a coordinate
-}
heading : Coord -> Particle -> Particle
heading velocity particle =
    { particle | velocity = velocity }


withGravity : Float -> Particle -> Particle
withGravity pxPerSecond ({ acceleration } as particle) =
    { particle | acceleration = { acceleration | y = pxPerSecond } }


{-| -}
type alias Coord =
    { x : Float, y : Float }


{-| -}
update : Float -> Particle -> Maybe Particle
update deltaSeconds { position, velocity, acceleration, lifetime } =
    if lifetime - deltaSeconds <= 0 then
        Nothing

    else
        Just
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
view : Particle -> Html msg
view { position, lifetime } =
    Html.div
        [ style "position" "absolute"
        , style "left" (String.fromFloat position.x ++ "px")
        , style "top" (String.fromFloat position.y ++ "px")
        ]
        [ Html.text (String.fromFloat lifetime) ]
