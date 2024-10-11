import type * as $dynamic from "../../gleam_stdlib/gleam/dynamic.d.mts";
import type * as $list from "../../gleam_stdlib/gleam/list.d.mts";
import type * as _ from "../gleam.d.mts";
import type * as $atom from "../gleam/erlang/atom.d.mts";
import type * as $charlist from "../gleam/erlang/charlist.d.mts";

export class Eof extends _.CustomType {}

export class NoData extends _.CustomType {}

export type GetLineError$ = Eof | NoData;

export class Second extends _.CustomType {}

export class Millisecond extends _.CustomType {}

export class Microsecond extends _.CustomType {}

export class Nanosecond extends _.CustomType {}

export type TimeUnit$ = Second | Millisecond | Microsecond | Nanosecond;

export class Exited extends _.CustomType {
  constructor(argument$0: $dynamic.Dynamic$);
  
  0: $dynamic.Dynamic$;
}

export class Thrown extends _.CustomType {
  constructor(argument$0: $dynamic.Dynamic$);
  
  0: $dynamic.Dynamic$;
}

export class Errored extends _.CustomType {
  constructor(argument$0: $dynamic.Dynamic$);
  
  0: $dynamic.Dynamic$;
}

export type Crash$ = Exited | Thrown | Errored;

export class UnknownApplication extends _.CustomType {
  constructor(name: $atom.Atom$);
  
  name: $atom.Atom$;
}

export class ApplicationFailedToStart extends _.CustomType {
  constructor(name: $atom.Atom$, reason: $dynamic.Dynamic$);
  
  name: $atom.Atom$;
  reason: $dynamic.Dynamic$;
}

export type EnsureAllStartedError$ = UnknownApplication | ApplicationFailedToStart;

export type Reference$ = any;

export function format(term: any): string;

export function term_to_binary(a: any): _.BitArray;

export function get_line(prompt: string): _.Result<string, GetLineError$>;

export function system_time(a: TimeUnit$): number;

export function erlang_timestamp(): [number, number, number];

export function rescue<FTO>(a: () => FTO): _.Result<FTO, Crash$>;

export function binary_to_term(binary: _.BitArray): _.Result<
  $dynamic.Dynamic$,
  undefined
>;

export function unsafe_binary_to_term(binary: _.BitArray): _.Result<
  $dynamic.Dynamic$,
  undefined
>;

export function start_arguments(): _.List<string>;

export function ensure_all_started(application: $atom.Atom$): _.Result<
  _.List<$atom.Atom$>,
  EnsureAllStartedError$
>;

export function make_reference(): Reference$;

export function priv_directory(name: string): _.Result<string, undefined>;
