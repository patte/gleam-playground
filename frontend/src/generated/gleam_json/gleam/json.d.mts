import type * as $bit_array from "../../gleam_stdlib/gleam/bit_array.d.mts";
import type * as $dynamic from "../../gleam_stdlib/gleam/dynamic.d.mts";
import type * as $list from "../../gleam_stdlib/gleam/list.d.mts";
import type * as $option from "../../gleam_stdlib/gleam/option.d.mts";
import type * as $result from "../../gleam_stdlib/gleam/result.d.mts";
import type * as $string_builder from "../../gleam_stdlib/gleam/string_builder.d.mts";
import type * as _ from "../gleam.d.mts";

export type Json$ = any;

export class UnexpectedEndOfInput extends _.CustomType {}

export class UnexpectedByte extends _.CustomType {
  constructor(argument$0: string);
  
  0: string;
}

export class UnexpectedSequence extends _.CustomType {
  constructor(argument$0: string);
  
  0: string;
}

export class UnexpectedFormat extends _.CustomType {
  constructor(argument$0: _.List<$dynamic.DecodeError$>);
  
  0: _.List<$dynamic.DecodeError$>;
}

export type DecodeError$ = UnexpectedEndOfInput | UnexpectedByte | UnexpectedSequence | UnexpectedFormat;

export function decode<GFF>(
  json: string,
  decoder: (x0: $dynamic.Dynamic$) => _.Result<
    GFF,
    _.List<$dynamic.DecodeError$>
  >
): _.Result<GFF, DecodeError$>;

export function decode_bits<GFP>(
  json: _.BitArray,
  decoder: (x0: $dynamic.Dynamic$) => _.Result<
    GFP,
    _.List<$dynamic.DecodeError$>
  >
): _.Result<GFP, DecodeError$>;

export function to_string(json: Json$): string;

export function to_string_builder(json: Json$): $string_builder.StringBuilder$;

export function string(input: string): Json$;

export function bool(input: boolean): Json$;

export function int(input: number): Json$;

export function float(input: number): Json$;

export function null$(): Json$;

export function nullable<GFV>(
  input: $option.Option$<GFV>,
  inner_type: (x0: GFV) => Json$
): Json$;

export function object(entries: _.List<[string, Json$]>): Json$;

export function preprocessed_array(from: _.List<Json$>): Json$;

export function array<GFZ>(entries: _.List<GFZ>, inner_type: (x0: GFZ) => Json$): Json$;
