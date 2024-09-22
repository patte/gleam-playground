import type * as _ from "../../gleam.d.mts";
import type * as $atom from "../../gleam/erlang/atom.d.mts";

export type Node$ = any;

export class FailedToConnect extends _.CustomType {}

export class LocalNodeIsNotAlive extends _.CustomType {}

export type ConnectError$ = FailedToConnect | LocalNodeIsNotAlive;

export function self(): Node$;

export function visible(): _.List<Node$>;

export function connect(node: $atom.Atom$): _.Result<Node$, ConnectError$>;

export function send(node: Node$, name: $atom.Atom$, message: any): undefined;

export function to_atom(node: Node$): $atom.Atom$;
