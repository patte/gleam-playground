import actors/calculator_actor.{Solve, is_calculation}
import actors/messages.{
  type CustomWebsocketMessage, type RoomActorMessage, ConnectUser,
  DisconnectUser, SendToAll, SendToClient,
}
import birl
import gleam/erlang/process.{type Subject, Normal}
import gleam/function
import gleam/http/request.{type Request}
import gleam/http/response.{type Response}
import gleam/int
import gleam/option.{type Option, None, Some}
import gleam/otp/actor.{type Next, Stop}
import gleam/string
import logging
import mist.{
  type Connection, type ResponseData, type WebsocketConnection,
  type WebsocketMessage, Custom, Text,
}
import shared/src/shared.{ChatMessage}

import process_utils.{process_message_queue_len}

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
      // check queue length of process
      // if too high this means client is sending faster than we can process
      // we stop calculations if > 3
      // in testing it was discovered that this number increases by 1 between
      // runs of handle_message. So our view of how much work is queued is
      // delayed.
      let process_queue_len = process_message_queue_len()
      case process_queue_len {
        i if i > 3 -> {
          logging.log(
            logging.Warning,
            "queue len:" <> process_queue_len |> int.to_string,
          )
        }
        _ -> Nil
      }

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

      // transform message (calculator)
      // forward ChatMessage to room
      case parsed_message {
        ChatMessage(content, author, created_at) -> {
          // calculator (sync)
          let parsed_message = case
            is_calculation(content) && process_queue_len <= 3
          {
            False -> parsed_message
            True -> {
              // start new actor
              let calc_actor = calculator_actor.start()

              // call actor with timeout ms
              let calc_result =
                process.try_call(
                  calc_actor,
                  fn(reply_to) { Solve(reply_to, content) },
                  2000,
                )

              // stop actor
              // without the unlink we kill (our)self
              process.unlink(process.subject_owner(calc_actor))
              process.kill(process.subject_owner(calc_actor))

              case calc_result {
                Ok(Ok(calc_result)) -> {
                  ChatMessage(
                    content <> " = " <> calc_result,
                    author,
                    created_at,
                  )
                }
                Error(e) -> {
                  let error = string.inspect(e)
                  ChatMessage(
                    content <> " = 💥 " <> error,
                    author,
                    created_at,
                  )
                }
                _ -> parsed_message
              }
            }
          }

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
      logging.log(logging.Info, "Stopping websocket actor in handle_message")
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
