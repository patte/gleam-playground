# gleam chat

Exploring Gleam. Svelte frontend using types and functions from Gleam. OTP actors to simulate a chat server and test concurrency.

## Development

### Setup
Use [asdf](https://asdf-vm.com/guide/getting-started.html) to `asdf install` gleam and other dependencies defined in [.tool-versions](./.tool-versions).

### Easy
```sh
gleam run
```
Open [localhost:3000/test.html](http://localhost:3000/test.html) in your browser.

### Svelte Frontend
```sh
# run backend
gleam run

# build shared types
cd src/shared
gleam build --target javascript
rm -rf ../../frontend/src/generated
cp -r build/dev/javascript ../../frontend/src/generated
cd ../..

# run frontend
cd frontend
bun dev
```

Open: [localhost:3000](http://localhost:3000) in your browser.

## Features

### Types and code sharing between Gleam and Svelte
This repo contains 3 projects:
```
./               # gleam module hello_world (backend)
├─ src/shared/   # gleam module shared (shared types)
├─ frontend/     # svelte (frontend)
```
The folder `src/shared` is part of the be project and also it's own project that is built to typescript and [copied to](./frontend/src/generated/) the frontend project. This organisation comes from the limitation that gleam can not have different entrypoints for the targets erlang and javascript but nested projects seem to work.

The [shared module](src/shared/src/shared.gleam) contains type definitions and encode / decode functions for the messages between the fe and be. See how the fe consumes the types at [onMessage](./frontend/src/Chat.svelte#L58) and [sendMessage](./frontend/src/Chat.svelte#L140). End to end types are working: 🎉. Currently, everytime the shared code is edited it needs to be built and copied to the frontend manually with the commands noted below. Whether these files should be checked into version control is debatable, it's done here for this example to be explorable without running it.


### OTP Actors
To test out erlang OTP and scheduling of tasks, the backend provides a simple chat server with OTP actors. One single [room_actor](./src/actors/room_actor.gleam), one [websocker_actor](./src/actors/websocket_actor.gleam) per connection, and one [calculator_actor](./src/actors/calculator_actor.gleam) on demand per websocket actor.

The room actor is created on startup. On connection a new websocket actor is created which [registers itself (a subject)](./src/actors/websocket_actor.gleam#L52) at the room actor. When the websocket actor [receives a message from a client](./src/actors/websocket_actor.gleam#L82) it parses it, sends it to the room actor, which sends it to all registered websocket actors (including the sender), which then [send it to the client](./src/actors/websocket_actor.gleam#L72).


#### Calculator
The calculator actors role is to provide for an example of a long running, blocking, task. It is [created on demand](./src/actors/websocket_actor.gleam#L127) by the websocket actor when it receives a message that is a calculation. The websocket actor then waits for the result before sending it to the room actor. This can be used to block one websocket actor and test that other actors are continuing to work.

OOM: The calculator actor solves factorials (`3! = 3 * 2 * 1 = 6`) and doesn't have an upper limit. For big inputs it will run out of memory and crash... the complete beam vm. Out of memory errors in erlang are not caught by the actor, so the complete beam vm crashes. I tried to find a solution to this "problem", but it doesn't seem to be possible easily also because it's an artificial problem: In the real world, APIs don't tend to offer unbouded calculations. For example image CDNs limit the maximum size of an image, which would by estimating the required memory, allow for a queue actor that doesn't start more calculation actors than the system can handle. Still it's not what I would have expected. It seems OOM is one of the few things that can crash the beam vm. I would be very interested in input on this topic.


### Svelte
The [svelte frontend](./frontend/src/) is a simple chat client that connects to the backend and sends and receives messages. It uses the shared types to encode and decode messages. The code is currently a bit messy.


## Sources
Without these sources this project would not have been possible. Thank you to the authors!
Sources:
- [connellr023/chatter-reborn](https://github.com/connellr023/chatter-reborn)
- [hayleigh-dot-dev/fosdem-2023](https://github.com/hayleigh-dot-dev/fosdem-2023)
- [bcpeinhardt/learn_otp_with_gleam](https://github.com/bcpeinhardt/learn_otp_with_gleam)
- [Exploring the Gleam FFI - Jonas Hietala](https://www.jonashietala.se/blog/2024/01/11/exploring_the_gleam_ffi)
- [The Soul of Erlang and Elixir • Sasa Juric • GOTO 2019](https://www.youtube.com/watch?v=JvBT4XBdoUE)


## Screenshot

![Screenshot](./docs/screenshot.png)