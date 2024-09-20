import gleam/erlang/process.{type Subject}

pub type CustomWebsocketMessage {
  SendToClient(message: String)
}

pub type RoomActorMessage {
  ConnectUser(user_subject: Subject(CustomWebsocketMessage))
  DisconnectUser(user_subject: Subject(CustomWebsocketMessage))
  SendToAll(message: String)
}
