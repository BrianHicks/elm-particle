module Particle exposing
    ( Particle, generate, at, heading, withGravity
    , view
    , update
    )

{-|


# Constructing Particles

@docs Particle, generate, at, heading, withGravity


# Rendering Particles

@docs view


# Simulation

@docs update

-}

import Random exposing (Generator)
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

Then a `Particle` of confetti—just one of those little pieces—would be `Particle
Confetti`. Boring, but in the best way possible! You'll get to see this data
again in `view` when you render your particle.

-}
type Particle a
    = Particle
        { data : a
        , position : Coord
        , velocity : Coord
        , acceleration : Coord
        , originalLifetime : Maybe Float
        , lifetime : Maybe Float
        }


type alias Coord =
    { x : Float, y : Float }



-- constructors


{-| Get a [`Generator Particle`](#Particle), given a `Random.Generator` to use
to render get the specific data you want it to have.

For our `Confetti`, we'd get a yellow diamond by calling `generate` like this:

    generate (Random.constant { color = Yellow, shape = Diamond })

TODO: move at least part of this into the module doc instead of the function doc.

Or we could get a random color and shape by using other functions from `Random`:

    generate
        (Random.map2 (\color shape -> { color = color, shape = shape })
            (Random.uniform Red [ Orange, Yellow, Green, Blue, Purple ])
            (Random.uniform Circle [ Square, Diamond ])
        )

This will make a `Confetti` which is one of the colors and shapes listed in the
calls to `Random.uniform`

**Tip!** If you haven't used the `Random` library before, check out [The Elm
Guide's explanation][teg].

That said, this API is designed so that you can compose things together by
piping them. So in your code, it should probably look more like this:

    confetti : Random.Generator Confetti
    confetti =
        Random.map2 (\color shape -> { color = color, shape = shape })
            (Random.uniform Red [ Orange, Yellow, Green, Blue, Purple ])
            (Random.uniform Circle [ Square, Diamond ])

Then, when you're generating particles, you can use `generate` to start your
pipeline like this:

    generate confetti
        |> at someLocation
        |> heading someDirection

So, why does this need to be a `Random.Generator` of your specific particle?
Well, it's rarely interesting for 100 particles to be doing exactly the same
thing. And, in fact, they'll all appear like on particle if you release them in
a `burst`.

[teg]: https://guide.elm-lang.org/effects/random.html

-}
generate : Generator a -> Generator (Particle a)
generate generator =
    Random.map
        (\data ->
            Particle
                { data = data
                , position = { x = 0, y = 0 }
                , velocity = { x = 0, y = 0 }
                , acceleration = { x = 0, y = 0 }
                , originalLifetime = Nothing
                , lifetime = Nothing
                }
        )
        generator


{-| Sometimes, you don't want particles to live forever. It means calculating a
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
    Random.map2 (\lifetime (Particle particle) -> Particle { particle | lifetime = Just lifetime })


{-| Where should this particle start? `{ x = 0, y = 0}` is at the top left of
the image. So we can render in the center like this:

    generate confetti
        |> at (Random.constant { x = width / 2, y = height / 2 })

Or at a random location on screen like this:

    generate confetti
        |> at
            (Random.map2 (\x y -> { x = x, y = y })
                (Random.map (modBy width << abs) Random.float)
                (Random.map (modBy height << abs) Random.float)
            )

(although you may want to check out `Random.Float.normal` from
`elm-community/random-extra` as it will give you a more "natural-looking"
distribution with a clump in the middle and less on the outsides.)

-}
at : Generator { x : Float, y : Float } -> Generator (Particle a) -> Generator (Particle a)
at =
    Random.map2 (\position (Particle particle) -> Particle { particle | position = position })


{-| In which direction should this particle travel, and how fast should it go?

In this case, speed is a rough measurement—it doesn't correspond exactly to
pixels per second, so you'll have to experiment. Sorry!

On the other hand, angle _is_ well-defined: we use the Elm Standard Units™
(Radians.) `0` is straight up, and rotation goes clockwise.

So if we want our confetti to spray up and to the right, we'll do something
like:

    generate confetti
        |> heading (Random.constant { speed = 200, angle = radians 1 })

But like the rest of these, you'll get a nicer-looking result by using
randomness:

    confetti
        |> Particle.generate 1
        |> Particle.heading
            (Random.map2 (\speed angle -> { speed = speed, angle = angle })
                (Random.Float.normal 300 100)
                (Random.Float.normal (degrees 45) (degrees 10))
            )

-}
heading : Generator { speed : Float, angle : Float } -> Generator (Particle a) -> Generator (Particle a)
heading =
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
unlike position, heading, or lifetime, you probably _do_ want all your particles
to have the same gravity! (Or at least, you want a few groupings of gravity, not
every particle being affected differently.)

**Note:** under the covers, this is really modeling acceleration over time, so
it's not _only_ gravity. But, I can't think of anything offhand I need this for
other than gravity! So if you have a concrete use case for going sideways or up,
[open an issue][issue] and let me know!

[issue]: https://github.com/BrianHicks/elm-particle/issues

-}
withGravity : Float -> Particle a -> Particle a
withGravity pxPerSecond (Particle ({ acceleration } as particle)) =
    Particle { particle | acceleration = { acceleration | y = pxPerSecond } }


{-| **Hey!** You probably shouldn't use this! Instead, manage all your particles
at once with the functions in `Particle.System`!

That said, this updates a single particle, given a delta in milliseconds.

-}
update : Float -> Particle a -> Maybe (Particle a)
update deltaMs (Particle { data, position, velocity, acceleration, originalLifetime, lifetime }) =
    let
        deltaSeconds =
            deltaMs / 1000

        newLifetime =
            Maybe.map (\l -> l - deltaSeconds) lifetime

        shouldRemove =
            case newLifetime of
                Just l ->
                    l <= 0

                Nothing ->
                    False
    in
    if shouldRemove then
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
            , lifetime = newLifetime
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
view : (a -> Float -> Svg msg) -> Particle a -> Svg msg
view viewData (Particle { data, position, originalLifetime, lifetime }) =
    let
        lifetimePercent =
            case ( lifetime, originalLifetime ) of
                ( Just lifetime_, Just originalLifetime_ ) ->
                    lifetime_ / originalLifetime_

                _ ->
                    1
    in
    Svg.g
        [ Attrs.transform ("translate(" ++ String.fromFloat position.x ++ "," ++ String.fromFloat position.y ++ ")") ]
        [ viewData data lifetimePercent ]
