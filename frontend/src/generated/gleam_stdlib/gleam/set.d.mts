import type * as _ from "../gleam.d.mts";
import type * as $dict from "../gleam/dict.d.mts";
import type * as $list from "../gleam/list.d.mts";
import type * as $result from "../gleam/result.d.mts";

class Set<FDI> extends _.CustomType {
  constructor(dict: $dict.Dict$<any, undefined>);
  
  dict: $dict.Dict$<any, undefined>;
}

export type Set$<FDI> = Set<FDI>;

export function new$(): Set$<any>;

export function size(set: Set$<any>): number;

export function is_empty(set: Set$<any>): boolean;

export function contains<FDS>(set: Set$<FDS>, member: FDS): boolean;

export function delete$<FDU>(set: Set$<FDU>, member: FDU): Set$<FDU>;

export function to_list<FDX>(set: Set$<FDX>): _.List<FDX>;

export function fold<FED, FEF>(
  set: Set$<FED>,
  initial: FEF,
  reducer: (x0: FEF, x1: FED) => FEF
): FEF;

export function filter<FEG>(set: Set$<FEG>, predicate: (x0: FEG) => boolean): Set$<
  FEG
>;

export function drop<FEN>(set: Set$<FEN>, disallowed: _.List<FEN>): Set$<FEN>;

export function take<FER>(set: Set$<FER>, desired: _.List<FER>): Set$<FER>;

export function intersection<FFE>(first: Set$<FFE>, second: Set$<FFE>): Set$<
  FFE
>;

export function difference<FFI>(first: Set$<FFI>, second: Set$<FFI>): Set$<FFI>;

export function is_subset<FFM>(first: Set$<FFM>, second: Set$<FFM>): boolean;

export function is_disjoint<FFP>(first: Set$<FFP>, second: Set$<FFP>): boolean;

export function insert<FDP>(set: Set$<FDP>, member: FDP): Set$<FDP>;

export function from_list<FEA>(members: _.List<FEA>): Set$<FEA>;

export function map<FEJ, FEL>(set: Set$<FEJ>, fun: (x0: FEJ) => FEL): Set$<FEL>;

export function union<FFA>(first: Set$<FFA>, second: Set$<FFA>): Set$<FFA>;

export function symmetric_difference<FFS>(first: Set$<FFS>, second: Set$<FFS>): Set$<
  FFS
>;
