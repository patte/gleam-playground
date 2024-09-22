import type * as $json from "../gleam_json/gleam/json.d.mts";
import type * as $dynamic from "../gleam_stdlib/gleam/dynamic.d.mts";
import type * as $result from "../gleam_stdlib/gleam/result.d.mts";
import type * as _ from "./gleam.d.mts";

export class ChatMessage extends _.CustomType {
  constructor(text: string, author: string, created_at: string);
  
  text: string;
  author: string;
  created_at: string;
}

export class RoomUpdate extends _.CustomType {
  constructor(num_participants: number);
  
  num_participants: number;
}

export type Message$ = ChatMessage | RoomUpdate;

export function message_to_json(msg: Message$): $json.Json$;

export function message_to_string(msg: Message$): string;

export function message_from_string(json: string): _.Result<
  Message$,
  $json.DecodeError$
>;
