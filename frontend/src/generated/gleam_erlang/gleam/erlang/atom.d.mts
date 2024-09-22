import type * as $dynamic from "../../../gleam_stdlib/gleam/dynamic.d.mts";
import type * as _ from "../../gleam.d.mts";

export type Atom$ = any;

export class AtomNotLoaded extends _.CustomType {}

export type FromStringError$ = AtomNotLoaded;

export function from_string(a: string): _.Result<Atom$, FromStringError$>;

export function create_from_string(a: string): Atom$;

export function to_string(a: Atom$): string;

export function from_dynamic(from: $dynamic.Dynamic$): _.Result<
  Atom$,
  _.List<$dynamic.DecodeError$>
>;
