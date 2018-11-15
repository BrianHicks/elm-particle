module Particle.System exposing
    ( System, init
    , burst
    , Msg, update, view, sub
    )

{-|


# Constructing

@docs System, init


# Gimme some particles!

@docs burst


# Simulate Particles

@docs Msg, update, view, sub

-}

import Browser.Events
import Html exposing (Html)
import Particle exposing (Particle)
import Random exposing (Generator)
import Svg exposing (Svg)
import Time


{-| Hold and simulate all your particles at once! This is a whole lot easier
than managing all the subscriptions and emitters yourself.
-}
type System a
    = System
        { seed : Random.Seed
        , particles : List (Particle a)
        }


{-| Get a `System` given a seed. So, why do I need that? Well, it's much nicer
to generate random particles if you don't have to use `Cmd`s, so I keep track of
a seed so we can do everything in one call to your `update`! If you don't really
care about the seed, you can initialize it like this:

    init (Random.initialSeed 0)

If you want one that's actually random, get a random seed with the
`Random.independentSeed` generator.

-}
init : Random.Seed -> System a
init seed =
    System
        { seed = seed
        , particles = []
        }


{-| Get me from [`sub`](#sub) and pass me to [`update`](#update)!
-}
type Msg a
    = NewFrame Float (List (Particle a)) Random.Seed


{-| Make a burst of a bunch of particles at once! Use this for things like
confetti or fireworks. We use randomness here because 100 particles all doing
exactly the same thing tends to be uninterseting.

If you haven't used the `Random` library before, check out [Joël Quenneville's
blog post][rrr] and [talk] about it!

We could make a burst of 50 confetti particles like this:

    burst
        (Random.list 50
            (Random.map4
                (\angle speed color shape ->
                    Particle.init { color = color, shape = shape }
                        |> Particle.at { x = 50, y = 50 }
                        |> Particle.heading { angle = angle, speed = speed }
                        |> Particle.withGravity 980
                )
                -- generate our heading information using
                -- elm-community/random-extra's `Random.Float.normal` function. This
                -- gives us random data centered in a normal distribution around a
                -- given mean. This looks more natural than linear randomness. Give
                -- it a try!
                (Random.Float.normal 0 (degrees 10))
                (Random.Float.normal 300 100)
                -- our colors and shapes, though, are fine to use linear randomness!
                (Random.uniform Color.red [ Color.green, Color.blue ])
                (Random.uniform Shape.Circle [ Shape.Triangle, Shape.Diamond ])
            )
        )

[rrr]: https://robots.thoughtbot.com/rolling-random-romans
[talk]: https://www.youtube.com/watch?v=YxGWQdFo2Yc

-}
burst : Generator (List (Particle a)) -> System a -> System a
burst generator (System system) =
    let
        ( particles, nextSeed ) =
            Random.step generator system.seed
    in
    System { system | particles = particles ++ system.particles, seed = nextSeed }


{-| Update all the particles by one step. Use it in your `update` function
whenever you get a [`Msg`](#Msg). Probably like this:

    update msg model.system

-}
update : Msg a -> System a -> System a
update (NewFrame delta particles seed) (System system) =
    System
        { particles = List.filterMap (Particle.update delta) (particles ++ system.particles)
        , seed = seed
        }


{-| View all the particles in the system. You'll need to provide a rendering
function for each particle—the same one you'd give to `Particle.view`—as well as
a list of attributes on the SVG element itself.

If you wanted a full-screen particle display, it might look like this:

    view viewConfetti
        [ Html.Attributes.style "width" "100%"
        , Html.Attributes.style "height" "100vh"
        ]
        system

-}
view : (Particle a -> Svg msg) -> List (Html.Attribute msg) -> System a -> Html msg
view viewParticle attrs (System { particles }) =
    Svg.svg attrs (List.map (Particle.view viewParticle) particles)


{-| Subscribe to the right events in the browser. In this case, that's
requestAnimationFrame deltas. You mostly don't have to worry about how to hook
up the right functions, just stick this in the subscriptions of your app and
call `update` on the `Msg` you get.

The first parameter here is a list of emitters. These are different than bursts
in that they continuously spout particles. This can create some really neat
efects, like water or fire. These live in the subscription so that we can be
more intelligent about subscribing. It also means that you don't have to store
generators—which contain functions—in your model. So maybe you want to turn the
water on and off, or control the pressure? That's great! You can store those
parameters in your model, and use them to make your generators.

Each emitter gets the time since the last frame in milliseconds so that it can
calculate how many particles to emit. So, for example, a water emitter that
emitted 1,000 drops per second may look like this:

    waterEmitter : Float -> Generator (List (Particle Droplet))
    waterEmitter delta =
        Random.list (ceiling (delta / 1000)) dropletGenerator

Then you'd use this in your subscription like this:

    sub [ waterEmitter ] ParticleMsg model.system

If this still doesn't make sense, go read the `Water.elm` example, which ties
all of this together.

-}
sub : List (Float -> Generator (List (Particle a))) -> (Msg a -> msg) -> System a -> Sub msg
sub emitters msg ((System system) as outer) =
    if List.isEmpty emitters && List.isEmpty system.particles then
        Sub.none

    else
        Browser.Events.onAnimationFrameDelta
            (\delta ->
                let
                    ( particles, seed ) =
                        emitterParticles delta emitters outer
                in
                msg <| NewFrame delta particles seed
            )


emitterParticles : Float -> List (Float -> Generator (List (Particle a))) -> System a -> ( List (Particle a), Random.Seed )
emitterParticles delta emitters (System { seed }) =
    emitters
        |> List.foldl
            (\emitter ( particles, curSeed ) ->
                Random.step (emitter delta) curSeed
                    |> Tuple.mapFirst ((++) particles)
            )
            ( [], seed )
