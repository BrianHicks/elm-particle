module Particle exposing
    ( Particle, generate, withLifetime, withLocation, withHeading, withGravity, withDrag
    , view, data, lifetimePercent, direction
    , update
    )

{-|


# Particle

This module lets you particles! You'll need to define:

1.  **how it acts**, using things like [`withLocation`](#withLocation) and
    [`withHeading`](#withHeading).
2.  **what it looks like**, which you'll provide in the type parameter to
    [`Particle`](#Particle).

For example, maybe you want to show some confetti when a student finishes a
quiz. Hooray! Time to celebrate! 🎉 🎊 We'll model each little piece as having
both a color and a shape, like this:

    type alias Confetti =
        { color : Color -- Red, Green, Blue
        , shape : Shape -- Square, Star, Streamer
        }

Then a `Particle` of confetti—just one of those little pieces—would be `Particle
Confetti`. Boring, but in the best way possible! Now, we _could_ construct our
confetti by hand, like this:

    { color = Red, shape = Square }

… but that's boring! Not only do we have to do every piece we want by hand, but
since all functions in Elm are pure we will never get any variation! Boo!
Instead, we'll generate Confetti randomly using [`elm/random`][random]. If you
haven't used this package before, check out [The Elm Guide's explanation][teg],
or [Chandrika Achar's appearance on Elm Town][chandrika-achar]. We'll use
`Random.map2` and `Random.uniform` to generate particles of a random color and
shape:

  - `Random.uniform` takes a bunch of items, and chooses between them evenly.
  - `Random.map2` takes the random stuff you generate, and gives it as two
    arguments to a function.

The code ends up looking like this:

    confetti : Random.Generator Confetti
    confetti =
        Random.map2 Confetti
            (Random.uniform Red [ Green, Blue ])
            (Random.uniform Square [ Star, Streamer ])

So that's the data for rendering your particles, but how do you get them to
behave how you like? When using `Particle`, you'll create a particle with
[`generate`](#generate), and then use functions like
[`withLocation`](#withLocation) and [`withHeading`](#withHeading) to define
that. Read on for what they do!

One last thing before we get into the documentation in earnest: this page only
scratches the surface of what you can do with particle generators. There are a
few fully-worked-out and documented examples in the `examples` folder of the
source on GitHub. Go check those out!

[teg]: https://guide.elm-lang.org/effects/random.html
[random]: https://package.elm-lang.org/packages/elm/random/latest/
[chandrika-achar]: https://elmtown.simplecast.fm/randomness-chandrika


# Constructing Particles

@docs Particle, generate, withLifetime, withLocation, withHeading, withGravity, withDrag


# Rendering Particles

@docs view, data, lifetimePercent, direction


# Simulation

@docs update

-}

import Random exposing (Generator)
import Svg exposing (Svg)
import Svg.Attributes as Attrs


{-| A single particle, doing... something? Who knows! You get to define that!
See the top of the module docs for how this all fits together.
-}
type Particle a
    = Particle
        { data : a
        , position : Coord
        , velocity : Coord
        , acceleration : Coord
        , drag : { density : Float, area : Float, coefficient : Float }
        , originalLifetime : Float
        , lifetime : Float
        }


type alias Coord =
    { x : Float, y : Float }



-- constructors


{-| Start making a particle, given the data you want to use to render your
particle (which will also be random!)

    generate confetti

-}
generate : Generator a -> Generator (Particle a)
generate generator =
    Random.map
        (\a ->
            Particle
                { data = a
                , position = { x = 0, y = 0 }
                , velocity = { x = 0, y = 0 }
                , acceleration = { x = 0, y = 0 }
                , drag = { density = 0, area = 0, coefficient = 0 }
                , originalLifetime = positiveInfinity
                , lifetime = positiveInfinity
                }
        )
        generator


{-| You don't normally want particles to live forever. It means calculating a
lot of deltas that you don't care about, and which the person using your
software will never see. So, let's give them some lifetimes!

    generate confetti
        |> withLifetime (Random.constant 1)

In the future, it may be possible for `Particle.System.System` to automatically
remove particles which have gone off screen. For now, lifetimes are the best
system we have for this!

-}
withLifetime : Generator Float -> Generator (Particle a) -> Generator (Particle a)
withLifetime =
    Random.map2
        (\lifetime (Particle particle) ->
            Particle
                { particle
                    | originalLifetime = lifetime
                    , lifetime = lifetime
                }
        )


{-| Where should this particle start? `{ x = 0, y = 0}` is at the top left of
the image. So we can render in the center like this:

    generate confetti
        |> withLocation (Random.constant { x = width / 2, y = height / 2 })

Or at a random location on screen like this:

    generate confetti
        |> withLocation
            (Random.map2 (\x y -> { x = x, y = y })
                (Random.map (modBy width << abs) Random.float)
                (Random.map (modBy height << abs) Random.float)
            )

(although you may want to check out `Random.Float.normal` from
`elm-community/random-extra` as it will give you a more "natural-looking"
distribution with a clump in the middle and less on the outsides.)

-}
withLocation : Generator { x : Float, y : Float } -> Generator (Particle a) -> Generator (Particle a)
withLocation =
    Random.map2 (\position (Particle particle) -> Particle { particle | position = position })


{-| In which direction should this particle travel, and how fast should it go?

In this case, speed is a rough measurement—it doesn't correspond exactly to
pixels per second, so you'll have to experiment. Sorry!

On the other hand, angle _is_ well-defined: we use the Elm Standard Units™
(Radians.) `0` is straight up, and rotation goes clockwise.

So if we want our confetti to spray up and to the right, we'll do something
like:

    generate confetti
        |> withHeading (Random.constant { speed = 200, angle = radians 1 })

But like the rest of these, you'll get a nicer-looking result by using
randomness:

    confetti
        |> generate 1
        |> withHeading
            (Random.map2 (\speed angle -> { speed = speed, angle = angle })
                (Random.Float.normal 300 100)
                (Random.Float.normal (degrees 45) (degrees 10))
            )

-}
withHeading : Generator { speed : Float, angle : Float } -> Generator (Particle a) -> Generator (Particle a)
withHeading =
    Random.map2
        (\{ speed, angle } (Particle particle) ->
            Particle
                { particle
                    | velocity =
                        { x = speed * cos (angle - degrees 90)
                        , y = speed * sin (angle - degrees 90)
                        }
                }
        )


{-| Is this particle affected by gravity?

The unit here ends up being pixels per second per second. If you want something
earthlike, you'll probably want `9.8 * dots/meter`. Buuut that's also super
fast, and you probably want something more cartoony. `980` works well!

Back to our confetti, we certainly want it to be affected by gravity, so we'll
do this:

    generate confetti
        |> withGravity 980

This takes a constant, while its siblings take generators. Why is this? Well,
unlike position, withHeading, or lifetime, you probably _do_ want all your particles
to have the same gravity! (Or at least, you want a few groupings of gravity, not
every particle being affected differently.)

**Note:** under the covers, this is really modeling acceleration over time, so
it's not _only_ gravity. But, I can't think of anything offhand I need this for
other than gravity! So if you have a concrete use case for going sideways or up,
[open an issue][issue] and let me know!

[issue]: https://github.com/BrianHicks/elm-particle/issues

-}
withGravity : Float -> Generator (Particle a) -> Generator (Particle a)
withGravity pxPerSecond =
    Random.map
        (\(Particle ({ acceleration } as particle)) ->
            Particle { particle | acceleration = { acceleration | y = pxPerSecond } }
        )


withDrag : (a -> { density : Float, area : Float, coefficient : Float }) -> Generator (Particle a) -> Generator (Particle a)
withDrag drag =
    Random.map (\(Particle particle) -> Particle { particle | drag = drag particle.data })


{-| **Hey!** You probably shouldn't use this! Instead, manage all your particles
at once with the functions in `Particle.System`!

That said, this updates a single particle, given a delta in milliseconds.

-}
update : Float -> Particle a -> Maybe (Particle a)
update deltaMs (Particle ({ position, velocity, acceleration, drag, lifetime } as particle)) =
    let
        deltaSeconds =
            deltaMs / 1000

        dragAtVelocity : Float -> Float
        dragAtVelocity v =
            drag.coefficient * drag.area * 0.5 * drag.density * v * v

        applyDrag : Float -> Float
        applyDrag v =
            if v > 0 then
                v - dragAtVelocity v * deltaSeconds

            else
                v + dragAtVelocity (abs v) * deltaSeconds
    in
    if lifetime < 0 then
        Nothing

    else
        (Just << Particle)
            { data = particle.data
            , position =
                { x = position.x + velocity.x * deltaSeconds + acceleration.x * deltaSeconds * deltaSeconds / 2
                , y = position.y + velocity.y * deltaSeconds + acceleration.y * deltaSeconds * deltaSeconds / 2
                }
            , velocity =
                { x = velocity.x + acceleration.x * deltaSeconds |> applyDrag
                , y = velocity.y + acceleration.y * deltaSeconds |> applyDrag
                }
            , acceleration = acceleration
            , drag = drag
            , originalLifetime = particle.originalLifetime
            , lifetime = lifetime - deltaSeconds
            }


{-| Now that you've generated it, how should this particle look? Give me a
function, to which I'll pass the data you gave me in `generate` and a value
between 1 and 0 representing the percent of lifetime this particle has left
(this is good things like fading out or spinning!) So our confetti might look
like this:

    view
        (\{ color, shape } remainingLifetime ->
            case shape of
                Circle ->
                    Svg.circle
                        [ Svg.Attributes.r "10"
                        , Svg.Attributes.fill (Color.toHex color)
                        ]
                        []

                _ ->
                    -- other shapes here
        )

-}
view : (Particle a -> Svg msg) -> Particle a -> Svg msg
view viewData ((Particle { position }) as particle) =
    Svg.g
        [ Attrs.transform ("translate(" ++ String.fromFloat position.x ++ "," ++ String.fromFloat position.y ++ ")") ]
        [ viewData particle ]



-- view helpers. TODO: organize me!


data : Particle a -> a
data (Particle particle) =
    particle.data


lifetimePercent : Particle a -> Float
lifetimePercent (Particle { lifetime, originalLifetime }) =
    clamp 0 1 <| lifetime / originalLifetime


direction : Particle a -> Float
direction (Particle { velocity }) =
    atan2 velocity.y velocity.x



-- misc


positiveInfinity : Float
positiveInfinity =
    -- I hope there's a better way to do this sometime. For now... nope.
    1 / 0
