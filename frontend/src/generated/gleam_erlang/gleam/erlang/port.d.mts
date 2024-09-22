import type * as $dynamic from "../../../gleam_stdlib/gleam/dynamic.d.mts";
import type * as _ from "../../gleam.d.mts";

export type Port$ = any;

export function port_from_dynamic(from: $dynamic.Dynamic$): _.Result<
  Port$,
  _.List<$dynamic.DecodeError$>
>;
