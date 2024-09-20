import gleam/erlang/process.{type Subject}
import gleam/int
import gleam/list
import gleam/otp/actor.{type Next}
import logging

import actors/messages.{
  type CustomWebsocketMessage, type RoomActorMessage, ConnectUser,
  DisconnectUser, SendToAll, SendToClient,
}

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
      logging.log(logging.Info, "Connecting a user to a room")

      let new_participant = #("User", user_subject)
      let new_participants = list.append(state.participants, [new_participant])
      let new_state = RoomActorState(participants: new_participants)

      logging.log(
        logging.Info,
        "num participants: " <> list.length(new_participants) |> int.to_string,
      )

      new_state |> actor.continue
    }
    DisconnectUser(user_subject) -> {
      logging.log(logging.Info, "Disconnecting a user from a room")

      let new_participants =
        list.filter(state.participants, fn(p) { p.1 != user_subject })
      let new_state = RoomActorState(participants: new_participants)

      new_state |> actor.continue
    }
    SendToAll(message) -> {
      let num_participants = list.length(state.participants)
      logging.log(
        logging.Info,
        "Sending to "
          <> num_participants |> int.to_string
          <> " participants: "
          <> message,
      )

      list.each(state.participants, fn(p) {
        process.send(p.1, SendToClient(message))
      })

      state |> actor.continue
    }
  }
}
