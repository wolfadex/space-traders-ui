# SpaceTraders UI

A UI for [SpaceTraders](https://spacetraders.io/), built with [Elm](https://elm-lang.org/)

## ⚠️ WIP ⚠️

[![Netlify Status](https://api.netlify.com/api/v1/badges/d156e8fd-a09c-4628-b345-1b574e2471c4/deploy-status)](https://app.netlify.com/sites/space-trader-elm-wip/deploys)

## Current Features

- auth
- view your Agent
- view your Contracts
- view (parts of) your ships
  - move ships between being docked and being in orbit
  - move to waypoints within the same system
  - extract resources
  - sell cargo
- view systems, in 3D!
- view waypoints

<img src="./Screenshot 2023-05-20 at 5.09.40 PM.png"/>

<img src="./Screenshot 2023-05-21 at 12.17.05 AM.png" />

<img src="./Screenshot 2023-05-29 at 7.48.55 AM.png" />

<br />

---

<br />

# Development

## Prerequisites

If you haven't yet, read the SpaceTraders API docs https://docs.spacetraders.io/. There's also the useful refernce https://spacetraders.stoplight.io/docs/spacetraders for viewing the OpenAPI spec as docs.

I use [Nix](https://nixos.org/) for my development and highly recommend it. If you also use Nix, then setup will be very quick.

If you don't use Nix, you'll need to install

- [Node.js](https://nodejs.org)
- [Elm](https://guide.elm-lang.org/install/elm.html)

## Setup

_I use [direnv](https://direnv.net/). If you do as well, you'll want to run `direnv allow` in the root of this repo._

Then, regardless of whether you use direnv or not, you'll need to run:

```sh
npm install
```

Once that's complete, run `npm run dev` to start the dev server.

This uses [run-pty](https://www.npmjs.com/package/run-pty) to start up multiple CLI tools and lets you interact with them individually.

- a dev server
- elm compiler, using [elm-watch](https://lydell.github.io/elm-watch/)
- elm code gen
- [elm-review](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) for code review
