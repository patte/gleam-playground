import gleam/dynamic.{type DecodeError, type Dynamic, DecodeError}
import gleam/json.{type Json}
import gleam/result

pub type Message {
  ChatMessage(text: String, author: String, created_at: String)
  RoomUpdate(num_participants: Int)
}

pub fn message_to_json(msg: Message) -> Json {
  case msg {
    ChatMessage(text, author, created_at) ->
      json.object([
        #("$", json.string("ChatMessage")),
        #("text", json.string(text)),
        #("author", json.string(author)),
        #("created_at", json.string(created_at)),
      ])
    RoomUpdate(num_participants) ->
      json.object([
        #("$", json.string("RoomUpdate")),
        #("num_participants", json.int(num_participants)),
      ])
  }
}

fn decoder(dynamic: Dynamic) -> Result(Message, List(DecodeError)) {
  use tag <- result.then(dynamic.field("$", dynamic.string)(dynamic))
  let decoder = case tag {
    "ChatMessage" ->
      dynamic.decode3(
        ChatMessage,
        dynamic.field("text", dynamic.string),
        dynamic.field("author", dynamic.string),
        dynamic.field("created_at", dynamic.string),
      )
    "RoomUpdate" ->
      dynamic.decode1(
        RoomUpdate,
        dynamic.field("num_participants", dynamic.int),
      )

    _ -> fn(_) { Error([DecodeError("Message", tag, ["$"])]) }
  }

  decoder(dynamic)
}

pub fn message_to_string(msg: Message) -> String {
  message_to_json(msg) |> json.to_string
}

pub fn message_from_string(json: String) -> Result(Message, _) {
  json.decode(json, decoder)
}
