import type * as _ from "../gleam.d.mts";
import type * as $string from "../gleam/string.d.mts";

export function from_string(x: string): _.BitArray;

export function byte_size(x: _.BitArray): number;

export function slice(string: _.BitArray, position: number, length: number): _.Result<
  _.BitArray,
  undefined
>;

export function concat(bit_arrays: _.List<_.BitArray>): _.BitArray;

export function append(first: _.BitArray, second: _.BitArray): _.BitArray;

export function base64_encode(input: _.BitArray, padding: boolean): string;

export function base64_decode(encoded: string): _.Result<_.BitArray, undefined>;

export function base64_url_encode(input: _.BitArray, padding: boolean): string;

export function base64_url_decode(encoded: string): _.Result<
  _.BitArray,
  undefined
>;

export function base16_encode(input: _.BitArray): string;

export function base16_decode(input: string): _.Result<_.BitArray, undefined>;

export function inspect(input: _.BitArray): string;

export function is_utf8(bits: _.BitArray): boolean;

export function to_string(bits: _.BitArray): _.Result<string, undefined>;
