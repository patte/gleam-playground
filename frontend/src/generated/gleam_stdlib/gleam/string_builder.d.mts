import type * as _ from "../gleam.d.mts";
import type * as $list from "../gleam/list.d.mts";

export type StringBuilder$ = any;

export function prepend_builder(builder: StringBuilder$, prefix: StringBuilder$): StringBuilder$;

export function append_builder(builder: StringBuilder$, suffix: StringBuilder$): StringBuilder$;

export function new$(): StringBuilder$;

export function from_strings(strings: _.List<string>): StringBuilder$;

export function concat(builders: _.List<StringBuilder$>): StringBuilder$;

export function from_string(string: string): StringBuilder$;

export function prepend(builder: StringBuilder$, prefix: string): StringBuilder$;

export function append(builder: StringBuilder$, second: string): StringBuilder$;

export function to_string(builder: StringBuilder$): string;

export function byte_size(builder: StringBuilder$): number;

export function join(builders: _.List<StringBuilder$>, sep: string): StringBuilder$;

export function lowercase(builder: StringBuilder$): StringBuilder$;

export function uppercase(builder: StringBuilder$): StringBuilder$;

export function reverse(builder: StringBuilder$): StringBuilder$;

export function split(iodata: StringBuilder$, pattern: string): _.List<
  StringBuilder$
>;

export function replace(
  builder: StringBuilder$,
  pattern: string,
  substitute: string
): StringBuilder$;

export function is_equal(a: StringBuilder$, b: StringBuilder$): boolean;

export function is_empty(builder: StringBuilder$): boolean;
