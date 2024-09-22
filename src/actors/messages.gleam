import gleam/erlang/process.{type Subject}
import shared/src/shared.{type Message}

pub type CustomWebsocketMessage {
  SendToClient(message: Message)
}

pub type RoomActorMessage {
  ConnectUser(user_subject: Subject(CustomWebsocketMessage))
  DisconnectUser(user_subject: Subject(CustomWebsocketMessage))
  SendToAll(message: Message)
}
