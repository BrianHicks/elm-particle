module Particle exposing
    ( Particle, init
    , Coord, at, heading, withGravity
    , view
    , update
    )

{-|


# Constructing Particles

@docs Particle, init

@docs Coord, at, heading, withGravity


# Rendering Particles

@docs view


# Simulation

@docs update

-}

import Svg exposing (Svg)
import Svg.Attributes as Attrs


{-| A single particle, doing... something? Who knows! You get to define that!

There are two things to consider:

1.  **where it is** and **how it's moving** (use things like `at` and `heading` below)
2.  **what it looks like**... that's what the type parameter is for! Read on!

For example, maybe you want to show some confetti when a student finishes a
quiz. Hooray! We'll model each little piece as having both a color and a shape,
like this:

    type alias Confetti =
        { color : Color
        , shape : Shape -- like Circle, Square, Diamond, etc
        }

Then a `Particle` of confetti—just one of those little pieces—would
be... `Particle Confetti`. Boring, but in the best way possible! You'll get to
see this data again in `view` when you render your particle.

-}
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


{-| Get a [`Particle`](#Particle), given some data to use to render it and a
lifetime for it to live (in seconds.)
-}
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


{-| Coordinates—since we're using SVG for rendering, the units here are pixels
from the top left of the image.
-}
type alias Coord =
    { x : Float, y : Float }


{-| Where should this particle start?
-}
at : Coord -> Particle a -> Particle a
at position (Particle particle) =
    Particle { particle | position = position }


{-| In which direction should this particle travel, and how fast should it go?

In this case, speed is a rough measurement—it doesn't correspond exactly to
pixels per second, so you'll have to experiment. Sorry!

On the other hand, angle _is_ well-defined: we use the Elm Standard Units™
(Radians) and 0° is straight up

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


{-| Is this particle affected by gravity?

The unit here ends up being pixels per second per second. If you want something
earthlike, you'll probably want `9.8 * dots/meter`. Buuut that's also super
fast, and you probably want something more cartoony. `980` works well!

(**Note:** under the covers, this is really modeling acceleration over time, so
it's not _only_ gravity. But, I can't think of anything offhand I need this for
other than gravity! So if you have a concrete use case for going sideways or
upwards, open an issue and let me know!)

-}
withGravity : Float -> Particle a -> Particle a
withGravity pxPerSecond (Particle ({ acceleration } as particle)) =
    Particle { particle | acceleration = { acceleration | y = pxPerSecond } }


{-| Update a single particle, given a delta in milliseconds.

**Hey!** You probably shouldn't use this! Instead, manage all your particles
with a `Particle.System`!

-}
update : Float -> Particle a -> Maybe (Particle a)
update deltaMs (Particle { data, position, velocity, acceleration, originalLifetime, lifetime }) =
    let
        deltaSeconds =
            deltaMs / 1000
    in
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


{-| How should this particle look? Give me a function, to which I'll pass the
data you gave me in `init` and a value between 1 and 0 representing the percent
of lifetime this particle has left (this is good things like fading out or
spinning!)
-}
view : (a -> Float -> Svg msg) -> Particle a -> Svg msg
view viewData (Particle { data, position, originalLifetime, lifetime }) =
    Svg.g
        [ Attrs.transform ("translate(" ++ String.fromFloat position.x ++ "," ++ String.fromFloat position.y ++ ")") ]
        [ viewData data (lifetime / originalLifetime) ]
