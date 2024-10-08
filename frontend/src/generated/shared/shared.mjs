/// <reference types="./shared.d.mts" />
import * as $json from "../gleam_json/gleam/json.mjs";
import * as $dynamic from "../gleam_stdlib/gleam/dynamic.mjs";
import { DecodeError } from "../gleam_stdlib/gleam/dynamic.mjs";
import * as $result from "../gleam_stdlib/gleam/result.mjs";
import { Error, toList, CustomType as $CustomType } from "./gleam.mjs";

export class ChatMessage extends $CustomType {
  constructor(text, author, created_at) {
    super();
    this.text = text;
    this.author = author;
    this.created_at = created_at;
  }
}

export class RoomUpdate extends $CustomType {
  constructor(num_participants) {
    super();
    this.num_participants = num_participants;
  }
}

export function message_to_json(msg) {
  if (msg instanceof ChatMessage) {
    let text = msg.text;
    let author = msg.author;
    let created_at = msg.created_at;
    return $json.object(
      toList([
        ["$", $json.string("ChatMessage")],
        ["text", $json.string(text)],
        ["author", $json.string(author)],
        ["created_at", $json.string(created_at)],
      ]),
    );
  } else {
    let num_participants = msg.num_participants;
    return $json.object(
      toList([
        ["$", $json.string("RoomUpdate")],
        ["num_participants", $json.int(num_participants)],
      ]),
    );
  }
}

function decoder(dynamic) {
  return $result.then$(
    $dynamic.field("$", $dynamic.string)(dynamic),
    (tag) => {
      let decoder$1 = (() => {
        if (tag === "ChatMessage") {
          return $dynamic.decode3(
            (var0, var1, var2) => { return new ChatMessage(var0, var1, var2); },
            $dynamic.field("text", $dynamic.string),
            $dynamic.field("author", $dynamic.string),
            $dynamic.field("created_at", $dynamic.string),
          );
        } else if (tag === "RoomUpdate") {
          return $dynamic.decode1(
            (var0) => { return new RoomUpdate(var0); },
            $dynamic.field("num_participants", $dynamic.int),
          );
        } else {
          return (_) => {
            return new Error(
              toList([new DecodeError("Message", tag, toList(["$"]))]),
            );
          };
        }
      })();
      return decoder$1(dynamic);
    },
  );
}

export function message_to_string(msg) {
  let _pipe = message_to_json(msg);
  return $json.to_string(_pipe);
}

export function message_from_string(json) {
  return $json.decode(json, decoder);
}
