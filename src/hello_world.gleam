import gleam/bytes_builder
import gleam/erlang/process.{type Subject}
import gleam/http/request.{type Request}
import gleam/http/response.{type Response}
import gleam/io
import gleam/option.{None, Some}
import gleam/otp/actor.{type StartError}
import gleam/set.{type Set}
import mist.{type Connection, type ResponseData, type WebsocketConnection}

//import radiate

//import gleam/otp/actor
import gleam/result
import gleam/string
import logging

import ip_utils.{get_client_host_port}

pub fn main() {
  io.println("Hello world!")
  logging.configure()

  //let _ =
  //  radiate.new()
  //  |> radiate.add_dir(".")
  //  |> radiate.add_dir("./files")
  //  |> radiate.add_dir("./src")
  //  |> radiate.start()

  let assert Ok(_) = serve()

  // Serve starts a new process so we want to keep the main process alive.
  process.sleep_forever()
}

pub type AppState {
  AppState(clients: Set(WebsocketConnection))
}

fn serve() {
  let not_found =
    response.new(404)
    |> response.set_body(mist.Bytes(bytes_builder.from_string("Not found")))

  let assert Ok(_) =
    fn(req: Request(Connection)) -> Response(ResponseData) {
      let path_segments = request.path_segments(req)

      logging.log(
        logging.Info,
        "Request client: "
          <> get_client_host_port(req.body)
          <> " path: "
          <> string.inspect(path_segments),
      )

      case path_segments {
        [] -> serve_file(req, ["index.html"])
        ["file", ..rest] -> serve_file(req, rest)
        ["hello"] -> serve_hello_world(req)
        ["ws"] ->
          mist.websocket(
            request: req,
            on_init: fn(_conn) { #(Nil, None) },
            on_close: fn(_state) { io.println("goodbye!") },
            handler: handle_ws_message,
          )
        _ -> not_found
      }
    }
    |> mist.new
    |> mist.port(3000)
    |> mist.start_http
}

fn handle_ws_message(state, conn, message) {
  case message {
    mist.Text("ping") -> {
      let assert Ok(_) = mist.send_text_frame(conn, "pong")
      actor.continue(state)
    }
    mist.Text(_) | mist.Binary(_) -> {
      actor.continue(state)
    }
    mist.Custom(_) -> {
      actor.continue(state)
    }
    mist.Closed | mist.Shutdown -> actor.Stop(process.Normal)
  }
}

fn serve_hello_world(_req: Request(Connection)) -> Response(ResponseData) {
  response.new(200)
  |> response.set_body(mist.Bytes(bytes_builder.from_string("Hello, World!")))
}

fn serve_file(
  _req: Request(Connection),
  path: List(String),
) -> Response(ResponseData) {
  let root = "./files"
  let path =
    string.concat([root, "/", string.replace(string.join(path, "/"), "..", "")])

  mist.send_file(path, offset: 0, limit: None)
  |> result.map(fn(file) {
    io.println("Serving file: " <> path)

    let is_js = string.ends_with(path, ".js")
    let is_css = string.ends_with(path, ".css")
    let is_html = string.ends_with(path, ".html")
    let is_svg = string.ends_with(path, ".svg")
    let content_type = case Nil {
      _ if is_js -> "application/javascript"
      _ if is_css -> "text/css"
      _ if is_html -> "text/html"
      _ if is_svg -> "image/svg+xml"
      _ -> "text/plain"
    }
    response.new(200)
    |> response.prepend_header("content-type", content_type)
    |> response.set_body(file)
  })
  |> result.lazy_unwrap(fn() {
    io.println("File not found: " <> path)
    response.new(404)
    |> response.set_body(mist.Bytes(bytes_builder.new()))
  })
}
