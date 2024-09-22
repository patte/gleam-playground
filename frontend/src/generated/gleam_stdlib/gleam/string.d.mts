import type * as _ from "../gleam.d.mts";
import type * as $iterator from "../gleam/iterator.d.mts";
import type * as $list from "../gleam/list.d.mts";
import type * as $option from "../gleam/option.d.mts";
import type * as $order from "../gleam/order.d.mts";
import type * as $string_builder from "../gleam/string_builder.d.mts";

export function is_empty(str: string): boolean;

export function length(string: string): number;

export function replace(string: string, pattern: string, substitute: string): string;

export function lowercase(string: string): string;

export function uppercase(string: string): string;

export function compare(a: string, b: string): $order.Order$;

export function crop(string: string, substring: string): string;

export function contains(haystack: string, needle: string): boolean;

export function starts_with(string: string, prefix: string): boolean;

export function ends_with(string: string, suffix: string): boolean;

export function split_once(x: string, substring: string): _.Result<
  [string, string],
  undefined
>;

export function append(first: string, second: string): string;

export function concat(strings: _.List<string>): string;

export function repeat(string: string, times: number): string;

export function join(strings: _.List<string>, separator: string): string;

export function trim(string: string): string;

export function trim_left(string: string): string;

export function trim_right(string: string): string;

export function pop_grapheme(string: string): _.Result<
  [string, string],
  undefined
>;

export function to_graphemes(string: string): _.List<string>;

export function reverse(string: string): string;

export function slice(string: string, idx: number, len: number): string;

export function drop_left(string: string, num_graphemes: number): string;

export function drop_right(string: string, num_graphemes: number): string;

export function split(x: string, substring: string): _.List<string>;

export function pad_left(
  string: string,
  desired_length: number,
  pad_string: string
): string;

export function pad_right(
  string: string,
  desired_length: number,
  pad_string: string
): string;

export function to_utf_codepoints(string: string): _.List<_.UtfCodepoint>;

export function from_utf_codepoints(utf_codepoints: _.List<_.UtfCodepoint>): string;

export function utf_codepoint(value: number): _.Result<
  _.UtfCodepoint,
  undefined
>;

export function utf_codepoint_to_int(cp: _.UtfCodepoint): number;

export function to_option(s: string): $option.Option$<string>;

export function first(s: string): _.Result<string, undefined>;

export function last(s: string): _.Result<string, undefined>;

export function capitalise(s: string): string;

export function inspect(term: any): string;

export function byte_size(string: string): number;
