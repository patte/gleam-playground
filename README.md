# gleam chat

Learning Gleam.

Sources:
- [connellr023/chatter-reborn](https://github.com/connellr023/chatter-reborn)
- [hayleigh-dot-dev/fosdem-2023](https://github.com/hayleigh-dot-dev/fosdem-2023)
- [bcpeinhardt/learn_otp_with_gleam](https://github.com/bcpeinhardt/learn_otp_with_gleam)

## Development

### Setup
Use [asdf](https://asdf-vm.com/guide/getting-started.html) to `asdf install` gleam and other dependencies defined in [.tool-versions](./.tool-versions).

### Run
Build the [frontend](./frontend/README.md) `bun build`, then start the server:
```sh
gleam run
```
Open [localhost:3000](http://localhost:3000) in your browser.

Alternatively: [localhost:3000/test.html](http://localhost:3000/test.html) for the test page, which works without the frontend build.

## Screenshot

![Screenshot](./docs/screenshot.png)