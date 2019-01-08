module Particle exposing
    ( Particle, init, withLifetime, withLocation, withDirection, withSpeed, withGravity, withDrag
    , view, data, lifetimePercent, direction, directionDegrees
    , update
    )

{-|


# Particle

This module lets you particles! You'll need to define:

1.  **how it acts**, using things like [`withLocation`](#withLocation) and
    [`withDirection`](#withDirection).
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
[`init`](#init), and then use functions like [`withLocation`](#withLocation) and
[`withDirection`](#withDirection) to define that. Read on for what they do!

You should also read the documentation on `Particle.System` for a managed way to
render these—you don't have to worry about animation yourself!

One last thing before we get into the documentation in earnest: this page only
scratches the surface of what you can do with particle generators. There are a
few fully-worked-out and documented examples in the [`examples`][examples]
folder of the source on GitHub. Go check those out!

[teg]: https://guide.elm-lang.org/effects/random.html
[random]: https://package.elm-lang.org/packages/elm/random/latest/
[chandrika-achar]: https://elmtown.simplecast.fm/randomness-chandrika
[examples]: https://github.com/BrianHicks/elm-particle/tree/master/examples


# Constructing Particles

@docs Particle, init, withLifetime, withLocation, withDirection, withSpeed, withGravity, withDrag


# Rendering Particles

@docs view, data, lifetimePercent, direction, directionDegrees


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
        , position : Cartesian
        , velocity : Polar
        , acceleration : Cartesian
        , drag : { density : Float, area : Float, coefficient : Float }
        , originalLifetime : Float
        , lifetime : Float
        }


type Cartesian
    = Cartesian { x : Float, y : Float }


cartesianToTuple : Cartesian -> ( Float, Float )
cartesianToTuple (Cartesian { x, y }) =
    ( x, y )


type Polar
    = Polar { speed : Float, angle : Float }


polarToTuple : Polar -> ( Float, Float )
polarToTuple (Polar { speed, angle }) =
    ( speed, angle )



-- constructors


{-| Start making a particle, given a generator for the data you want to use to
render your particle.

    init confetti

-}
init : Generator a -> Generator (Particle a)
init generator =
    Random.map
        (\a ->
            Particle
                { data = a
                , position = Cartesian { x = 0, y = 0 }
                , velocity = Polar { speed = 0, angle = 0 }
                , acceleration = Cartesian { x = 0, y = 0 }
                , drag = { density = 0, area = 0, coefficient = 0 }
                , originalLifetime = positiveInfinity
                , lifetime = positiveInfinity
                }
        )
        generator


{-| You don't normally want particles to live forever. It means calculating a
lot of deltas that you don't care about, and which the person using your
software will never see. So, let's give them some lifetimes! The unit here is
seconds.

    init confetti
        |> withLifetime (Random.constant 1)

We use another `Random.Generator` here, since it looks nicer for particles which
have been introduced in a burst all at once to fade out progressively instead of
all at once. You can use `Random.Float.normal` from
[`elm-community/random-extra`][random-extra] to do this. For example: `normal 1
0.1`. This generates a normal distribution with a mean of the first number and a
standard deviation of the second, so it will not be _precisely_ 0.9 to 1.1
seconds, but `normal` tends to produce pretty good results!

**Note:** In the future, it may be possible for `Particle.System.System` to
automatically remove particles which have gone off screen. For now, lifetimes
are the best system we have for this!

[random-extra]: https://package.elm-lang.org/packages/elm-community/random-extra/latest

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


{-| Where should this particle start it's life? `{ x = 0, y = 0}` is at the top
left of the image. So we can render in the center like this:

    init confetti
        |> withLocation (Random.constant { x = width / 2, y = height / 2 })

Or at a random location on screen like this:

    init confetti
        |> withLocation
            (Random.map2 (\x y -> { x = x, y = y })
                (Random.map (modBy width << abs) Random.float)
                (Random.map (modBy height << abs) Random.float)
            )

-}
withLocation : Generator { x : Float, y : Float } -> Generator (Particle a) -> Generator (Particle a)
withLocation =
    Random.map2 (\position (Particle particle) -> Particle { particle | position = Cartesian { x = position.x, y = position.y } })


{-| In what direction is this particle traveling, to start?

`withDirection` uses Elm Standard Units™ (radians.) `0` is straight up, and
rotation goes clockwise. You can, of course, substitute `degrees 45` or `turns
0.125` if that's easier for you to reason about—I prefer degrees, myself!

    init confetti
        |> withDirection (Random.Float.normal (degrees 45) (degrees 10))

-}
withDirection : Generator Float -> Generator (Particle a) -> Generator (Particle a)
withDirection =
    Random.map2
        (\angle (Particle particle) ->
            Particle
                { particle
                    | velocity =
                        case particle.velocity of
                            Polar polar ->
                                Polar { polar | angle = angle - degrees 90 }
                }
        )


{-| How fast is this particle traveling traveling, to start?

In this case, speed is a rough measurement—it's close to but not exactly pixels
per second, so you'll have to experiment to make it look good for your use case.

    init confetti
        |> withSpeed (Random.Float.normal 300 100)

-}
withSpeed : Generator Float -> Generator (Particle a) -> Generator (Particle a)
withSpeed =
    Random.map2
        (\speed (Particle particle) ->
            Particle
                { particle
                    | velocity =
                        case particle.velocity of
                            Polar polar ->
                                Polar { polar | speed = speed }
                }
        )


{-| Is this particle affected by gravity?

The unit here ends up being pixels per second per second. If you want something
earthlike, you'll probably want `9.8 * dots/meter`. Buuut that's also super
fast, and you probably want something slightly slower and more cartoony. `980`
works well!

    init confetti
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
withGravity : Float -> Generator (Particle a) -> Generator (Particle a)
withGravity pxPerSecond =
    Random.map (\(Particle particle) -> Particle { particle | acceleration = Cartesian { x = 0, y = pxPerSecond } })


{-| How is this particle affected by the surrounding environment? Is there air?
Water? Setting the right resistance will help your particles look more
realistic! You'll have to tweak these numbers to get something you like; here's
what they mean:

  - **density** is the density of whatever fluid the particles are in. The
    higher this is, the more particles will be slowed down. Think about trying
    to run on land versus in the water—you're slowed down much more by the water
    than the air, and you experience more resistance the faster you try to
    move. Air will be around 0.001275 (g/cm³). Water will be around 1.

  - **area** is the area of the front surface. For a square, that'd be the side
    facing into the flow. The bigger this is, the more drag happens. When
    setting this, remember that this is a 2-dimensional simulation, so you
    mostly just provide a single dimension's length! In a _real_ simulation,
    we'd calculate this on every frame to account for rotation. But we can get
    acceptable results without that, so it's fine to just give a rough number
    here!

  - **coefficient** is how easily air/water/whatever flows over the surface
    facing into the flow. A higher number means that you will face more
    resistance. [Wikipedia][coefficients] has a nice chart of sample
    coefficients for various surface shapes; choosing one of those will probably
    get you most of the way there.

This function is a bit different from others because it's really convenient to
be able to generate whatever kind of particle and set drag separately. You could
structure your code so that this would not be a concern, but it gets annoying to
have to care about it it in multiple places. So, if we have these shapes in our
`Particle Shape`:

    type Shape
        = Circle Float
        | Square Float

We'd call `withDrag` like this:

    init shapeGenerator
        |> withDrag
            (\shape ->
                { density = 0.001275
                , coefficient =
                    case shape of
                        Circle _ ->
                            0.47

                        Square _ ->
                            1.05
                , area =
                    case shape of
                        Circle radius ->
                            radius * 2

                        Square side ->
                            side
                }
            )

[coefficients](https://en.wikipedia.org/wiki/Drag_coefficient#/media/File:14ilf1l.svg)

-}
withDrag : (a -> { density : Float, area : Float, coefficient : Float }) -> Generator (Particle a) -> Generator (Particle a)
withDrag drag =
    Random.map (\(Particle particle) -> Particle { particle | drag = drag particle.data })



-- view helpers


{-| **Hey!** You should probably be looking at the docs for
`Particle.System.view`, which has the same signature but works with all your
particles at once.

Render the particle as SVG. I'll give you the particle, and you use functions
like [`data`](#data) and [`lifetimePercent`](#lifetimePercent) to get the data
you need for rendering. It might look like this:

    view <|
        \particle ->
            case Particle.data particle of
                Square { color } ->
                    Svg.rect
                        [ Svg.Attributes.width "10"
                        , Svg.Attributes.height "10"
                        , Svg.Attributes.fill (Color.toHex color)
                        ]
                        []

                _ ->
                    -- other shapes here

You don't need to set the location of the particle, as it'll be done for you by
wrapping whatever you pass in a `<g>` element.

-}
view : (Particle a -> Svg msg) -> Particle a -> Svg msg
view viewData ((Particle { position }) as particle) =
    let
        ( x, y ) =
            cartesianToTuple position
    in
    Svg.g
        [ Attrs.transform ("translate(" ++ String.fromFloat x ++ "," ++ String.fromFloat y ++ ")") ]
        [ viewData particle ]


{-| Get the data you passed in out of a particle, for use in view functions.
-}
data : Particle a -> a
data (Particle particle) =
    particle.data


{-| Get the remaining lifetime of a particle, for use in view functions. This
returns a number between 0 and 1, which is useful for setting opacity to
smoothly fade a particle out instead of having it just disappear.
-}
lifetimePercent : Particle a -> Float
lifetimePercent (Particle { lifetime, originalLifetime }) =
    clamp 0 1 <| lifetime / originalLifetime


{-| Get the direction the particle is currently facing. This is useful for
particles whose shape implies a direction, like arrows or boxes.

**Heads up!** The most common use of this function is probably to set rotation
on the particle. That's fine, but the `rotate` transformation can only use
degrees, and this function returns radians. Use
[`directionDegrees`](#directionDegrees) instead so you can avoid doing the math
yourself.

-}
direction : Particle a -> Float
direction (Particle { velocity }) =
    velocity |> polarToTuple |> Tuple.second


{-| Like `direction` but returns the angle in degrees instead of radians to make
SVG transformations easier.
-}
directionDegrees : Particle a -> Float
directionDegrees particle =
    direction particle * 180 / pi



-- update


{-| **Hey!** You probably shouldn't use this! Instead, manage all your particles
at once with the functions in `Particle.System`!

That said, this updates a single particle, given a delta in milliseconds.

-}
update : Float -> Particle a -> Maybe (Particle a)
update deltaMs (Particle ({ position, velocity, acceleration, drag, lifetime } as particle)) =
    let
        deltaSeconds =
            deltaMs / 1000

        ( accelerationX, accelerationY ) =
            cartesianToTuple acceleration

        ( velocityX, velocityY ) =
            velocity |> polarToTuple |> fromPolar
    in
    if lifetime < 0 then
        Nothing

    else
        (Just << Particle)
            { data = particle.data
            , position =
                case position of
                    Cartesian { x, y } ->
                        Cartesian
                            { x = x + velocityX * deltaSeconds + accelerationX * deltaSeconds * deltaSeconds / 2
                            , y = y + velocityY * deltaSeconds + accelerationY * deltaSeconds * deltaSeconds / 2
                            }
            , velocity =
                let
                    ( newVelocitySpeed, newVelocityAngle ) =
                        toPolar
                            ( velocityX + accelerationX * deltaSeconds
                            , velocityY + accelerationY * deltaSeconds
                            )
                in
                Polar
                    { speed = drag.coefficient * drag.area * 0.5 * drag.density * newVelocitySpeed * newVelocitySpeed
                    , angle = newVelocityAngle
                    }
            , acceleration = acceleration
            , drag = drag
            , originalLifetime = particle.originalLifetime
            , lifetime = lifetime - deltaSeconds
            }



-- misc


positiveInfinity : Float
positiveInfinity =
    -- I hope there's a better way to do this sometime. For now... nope.
    1 / 0
