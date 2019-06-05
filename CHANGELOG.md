# Changelog

## 2019-06-04: 1.2.0

In our first foray into using particles at NoRedInk, we needed to render some `Html msg` as our particles, so now you can do that with `Particle.System.viewHtml` and `Particle.viewHtml`.

Please open an issue detailing your use case if you end up needing to use these. The `view` bits are currently the weakest part of the whole library, so the more information we can get about how people are embedding particles into their apps the better we can make it!

## 2019-01-08: 1.0.2

[Fixed](https://github.com/BrianHicks/elm-particle/pull/2) [bug #1](https://github.com/BrianHicks/elm-particle/issues/1) where drag was applied unevenly to particles since it was calculated for the x and y axes independently.

## 2018-11-16: 1.0.0 (Initial Release)

Details on [the Elm Discourse](https://discourse.elm-lang.org/t/elm-particle-1-0-1-create-visual-flourishes-in-svg/2542)

Quickly followed by 1.0.1 since I forgot to add examples in the README. 😄
