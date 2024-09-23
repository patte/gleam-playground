import gleam/erlang/process.{type Subject}
import gleam/int
import gleam/list
import gleam/otp/actor.{type Next}
import gleam/string
import logging

import actors/messages.{
  type CustomWebsocketMessage, type RoomActorMessage, ConnectUser,
  DisconnectUser, SendToAll, SendToClient,
}

import shared/src/shared.{type Message, RoomUpdate}

pub type RoomActorState {
  RoomActorState(participants: List(#(String, Subject(CustomWebsocketMessage))))
}

pub fn start() -> Subject(RoomActorMessage) {
  logging.log(logging.Info, "Starting room actor")

  let state = RoomActorState(participants: [])
  let assert Ok(actor) = actor.start(state, handle_message)

  actor
}

fn handle_message(
  message: RoomActorMessage,
  state: RoomActorState,
) -> Next(RoomActorMessage, RoomActorState) {
  case message {
    ConnectUser(user_subject) -> {
      let new_participant = #("User", user_subject)
      let new_participants = list.append(state.participants, [new_participant])
      let num_participants = list.length(new_participants)

      let new_state = RoomActorState(participants: new_participants)

      send_to_all(new_participants, RoomUpdate(num_participants))

      logging.log(
        logging.Info,
        "User joined! num participants: " <> num_participants |> int.to_string,
      )

      new_state |> actor.continue
    }
    DisconnectUser(user_subject) -> {
      let new_participants =
        list.filter(state.participants, fn(p) { p.1 != user_subject })
      let new_state = RoomActorState(participants: new_participants)
      let num_participants = list.length(new_participants)

      send_to_all(new_participants, RoomUpdate(num_participants))

      logging.log(
        logging.Info,
        "User left! num participants: " <> num_participants |> int.to_string,
      )

      new_state |> actor.continue
    }
    SendToAll(message) -> {
      send_to_all(state.participants, message)

      state |> actor.continue
    }
  }
}

fn send_to_all(
  participants: List(#(String, Subject(CustomWebsocketMessage))),
  message: Message,
) {
  let num_participants = list.length(participants)
  logging.log(
    logging.Info,
    "Sending to "
      <> num_participants |> int.to_string
      <> " participants: "
      <> string.slice(shared.message_to_string(message), 0, 200),
  )
  list.each(participants, fn(p) { process.send(p.1, SendToClient(message)) })
}
