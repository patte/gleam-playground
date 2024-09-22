import actors/messages.{
  type CustomWebsocketMessage, type RoomActorMessage, ConnectUser,
  DisconnectUser, SendToAll, SendToClient,
}
import birl
import gleam/erlang/process.{type Subject, Normal}
import gleam/function
import gleam/http/request.{type Request}
import gleam/http/response.{type Response}
import gleam/option.{type Option, None, Some}
import gleam/otp/actor.{type Next, Stop}
import logging
import mist.{
  type Connection, type ResponseData, type WebsocketConnection,
  type WebsocketMessage, Custom, Text,
}

import shared/src/shared.{ChatMessage}

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
    // from internal
    Custom(message) ->
      case message {
        SendToClient(message) -> {
          // stringify to json
          let message_json = shared.message_to_string(message)
          let assert Ok(_) = mist.send_text_frame(connection, message_json)
          state |> actor.continue
        }
      }
    // from browser
    Text(message) -> {
      // parse from json
      let parsed_message = case message |> shared.message_from_string {
        Ok(parsed_message) -> {
          parsed_message
        }
        Error(_) -> {
          logging.log(logging.Error, "Failed to parse message: " <> message)
          let now = birl.now()
          ChatMessage(
            "Failed to parse message: " <> message,
            "System",
            birl.to_iso8601(now),
          )
        }
      }

      // forward ChatMessage to room
      case parsed_message {
        ChatMessage(_, _, _) -> {
          // send to room
          {
            use room_subject <- option.then(state.room_subject)
            Some(process.send(room_subject, SendToAll(parsed_message)))
          }
        }
        _ -> {
          logging.log(
            logging.Error,
            "Received message is not a ChatMessage: " <> message,
          )
          None
        }
      }

      state |> actor.continue
    }
    _ -> {
      cleanup(state)
      Stop(Normal)
    }
  }
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
