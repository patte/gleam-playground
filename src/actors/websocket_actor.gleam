import actors/messages.{
  type CustomWebsocketMessage, type RoomActorMessage, ConnectUser,
  DisconnectUser, SendToAll, SendToClient,
}
import gleam/erlang/process.{type Subject, Normal}
import gleam/function
import gleam/http/request.{type Request}
import gleam/http/response.{type Response}
import gleam/json.{type Json}
import gleam/option.{type Option, None, Some}
import gleam/otp/actor.{type Next, Stop}
import gleam/string
import logging
import mist.{
  type Connection, type ResponseData, type WebsocketConnection,
  type WebsocketMessage, Custom, Text,
}

import shared/src/shared

pub type WebsocketActorState {
  WebsocketActorState(
    name: Option(String),
    ws_subject: Subject(CustomWebsocketMessage),
    room_subject: Option(Subject(RoomActorMessage)),
  )
}

pub fn start(
  req: Request(Connection),
  room_subject: Subject(RoomActorMessage),
) -> Response(ResponseData) {
  mist.websocket(
    request: req,
    on_init: fn(_) {
      let ws_subject = process.new_subject()
      let new_selector =
        process.new_selector()
        |> process.selecting(ws_subject, function.identity)

      let state =
        WebsocketActorState(
          name: None,
          ws_subject: ws_subject,
          room_subject: Some(room_subject),
        )

      // register at room
      process.send(room_subject, ConnectUser(ws_subject))

      #(state, Some(new_selector))
    },
    on_close: fn(state) {
      logging.log(logging.Info, "A connection was closed")
      state |> cleanup
      Nil
    },
    handler: handle_message,
  )
}

pub fn handle_message(
  state: WebsocketActorState,
  connection: WebsocketConnection,
  message: WebsocketMessage(CustomWebsocketMessage),
) -> Next(CustomWebsocketMessage, WebsocketActorState) {
  case message {
    Custom(message) ->
      case message {
        SendToClient(message) -> {
          send_client_string(connection, message)
          state |> actor.continue
        }
      }
    Text(message) -> {
      let parsed_message = message |> shared.message_from_string
      logging.log(
        logging.Info,
        "Received message: " <> string.inspect(parsed_message),
      )
      {
        use room_subject <- option.then(state.room_subject)
        Some(process.send(room_subject, SendToAll(message)))
      }

      state |> actor.continue
    }
    _ -> {
      cleanup(state)
      Stop(Normal)
    }
  }
}

fn send_client_string(connection: WebsocketConnection, message: String) {
  let assert Ok(_) = mist.send_text_frame(connection, message)
  Nil
}

fn cleanup(state: WebsocketActorState) -> WebsocketActorState {
  case state.room_subject {
    Some(room_subject) -> {
      process.send(room_subject, DisconnectUser(state.ws_subject))
      WebsocketActorState(..state, room_subject: None)
    }
    None -> state
  }

  state
}
