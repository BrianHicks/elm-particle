module Particle.System exposing
    ( System, init
    , burst
    , update, view, viewHtml, viewCustom, sub
    )

{-|


# Constructing

@docs System, init


# Gimme some particles!

@docs burst


# Simulate Particles

@docs Msg, update, view, viewHtml, viewCustom, sub

-}

import Browser.Events
import Html exposing (Html)
import Particle exposing (Particle)
import Random exposing (Generator)
import Svg exposing (Svg)


{-| Hold and simulate all your particles at once! This is a whole lot easier
than managing all the subscriptions and emitters yourself.
-}
type System a
    = System
        { seed : Random.Seed
        , particles : List (Particle a)
        }


{-| Get a `System` given a seed. So, why do I need that? Well, it's much nicer
to generate random particles if we can do it synchronously, so I keep track of a
seed so we can do everything in one call to your `update`! If you don't really
care about the seed, you can initialize it like this:

    init (Random.initialSeed 0)

If you want one that's actually pseudo-random, get a random seed with the
`Random.independentSeed` generator or pass the current time in when you
initialize your app from JavaScript.

-}
init : Random.Seed -> System a
init seed =
    System
        { seed = seed
        , particles = []
        }


{-| Make a burst of a bunch of particles at once! Use this for things like
confetti or fireworks. We use randomness here because 100 particles all doing
exactly the same thing tends to be uninteresting.

If you haven't used the `Random` library before, check the docs at the top of
`Particle`.

We could make a burst of 50 confetti particles like this:

    burst (Random.list 50 confettiParticles) system

-}
burst : Generator (List (Particle a)) -> System a -> System a
burst generator (System system) =
    let
        ( particles, nextSeed ) =
            Random.step generator system.seed
    in
    System { system | particles = particles ++ system.particles, seed = nextSeed }


{-| Update all the particles by one step. Use it in your `update` function
whenever you get a delta.

The first parameter here is a list of emitters. These are different than bursts
in that they continuously spout particles. This can create particle effects like
water or fire. They live in the subscription so that we can manage our
subscriptions intelligently. It also means that you don't have to store
generators—which contain functions—in your model. So maybe you want to turn a
stream of water on and off, or control the pressure? That's great! You can store
those parameters in your model, and use them to make your generators.

Each emitter gets the time since the last frame in milliseconds so that it can
calculate how many particles to emit. So, for example, a water emitter that
emitted 60 drops per second may look like this:

    waterEmitter : Float -> Generator (List (Particle Droplet))
    waterEmitter delta =
        Random.list (ceiling (delta * (60 / 1000))) dropletGenerator

Then you'd use this in your update like this:

    update [ waterEmitter ] delta model.system

-}
update : List (Float -> Generator (List (Particle a))) -> Float -> System a -> System a
update emitters delta (System system) =
    let
        ( particles, seed ) =
            if emitters /= [] then
                emitterParticles delta emitters system.seed

            else
                ( [], system.seed )
    in
    System
        { particles = List.filterMap (Particle.update delta) (particles ++ system.particles)
        , seed = seed
        }


{-| View all the particles in the system. You'll need to provide a rendering
function for each particle—the same one you'd give to `Particle.view`—as well as
a list of attributes on the SVG element itself.

For example, if you wanted a full-screen particle display, it would look like
this:

    view viewConfetti
        [ Html.Attributes.style "width" "100%"
        , Html.Attributes.style "height" "100vh"
        ]
        system

-}
view : (Particle a -> Svg msg) -> List (Html.Attribute msg) -> System a -> Html msg
view viewParticle attrs (System { particles }) =
    Svg.svg attrs (List.map (Particle.view viewParticle) particles)


{-| Do the same thing as [`view`](#view) but render HTML instead of SVG.
-}
viewHtml : (Particle a -> Html msg) -> List (Html.Attribute msg) -> System a -> Html msg
viewHtml viewParticle attrs (System { particles }) =
    Html.div attrs (List.map (Particle.viewHtml viewParticle) particles)


{-| Do the same thing as [`view`](#view) but render your own custom wrapper type
instead of SVG. You will want this if you are using something like
[`Html.Styled`](https://package.elm-lang.org/packages/rtfeldman/elm-css/latest/Html-Styled)
or [`TypedSvg`](https://package.elm-lang.org/packages/elm-community/typed-svg/latest/),
for example.

If you use this we do not know how to position the particle. Please use
`Particle.leftPixels` and `Particle.topPixels` to do that yourself.

-}
viewCustom : (Particle a -> renderedParticle) -> (List renderedParticle -> wrapper) -> System a -> wrapper
viewCustom viewParticle wrap (System { particles }) =
    wrap (List.map viewParticle particles)


{-| Subscribe to the right events in the browser. In this case, that's
`requestAnimationFrame` deltas. You don't have to worry about how to hook up the
right functions, just stick this in the subscriptions of your app and call
`update` with the `ParticleMsg Float`.

Then you'd use this in your subscription like this:

    sub ParticleMsg model.system

If this doesn't make sense, go read the [`Water.elm`][water] example, which ties
all of this together.

[water]: https://brianhicks.github.io/elm-particle/Water.html

-}
sub : (Float -> msg) -> System a -> Sub msg
sub msg (System { particles }) =
    if List.isEmpty particles then
        Sub.none

    else
        Browser.Events.onAnimationFrameDelta msg


emitterParticles : Float -> List (Float -> Generator (List (Particle a))) -> Random.Seed -> ( List (Particle a), Random.Seed )
emitterParticles delta emitters seed =
    emitters
        |> List.foldl
            (\emitter ( particles, curSeed ) ->
                Random.step (emitter delta) curSeed
                    |> Tuple.mapFirst ((++) particles)
            )
            ( [], seed )
