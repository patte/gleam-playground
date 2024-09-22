import type * as _ from "../gleam.d.mts";
import type * as $dict from "../gleam/dict.d.mts";
import type * as $list from "../gleam/list.d.mts";
import type * as $result from "../gleam/result.d.mts";

class Set<FDJ> extends _.CustomType {
  constructor(dict: $dict.Dict$<any, undefined>);
  
  dict: $dict.Dict$<any, undefined>;
}

export type Set$<FDJ> = Set<FDJ>;

export function new$(): Set$<any>;

export function size(set: Set$<any>): number;

export function is_empty(set: Set$<any>): boolean;

export function contains<FDT>(set: Set$<FDT>, member: FDT): boolean;

export function delete$<FDV>(set: Set$<FDV>, member: FDV): Set$<FDV>;

export function to_list<FDY>(set: Set$<FDY>): _.List<FDY>;

export function fold<FEE, FEG>(
  set: Set$<FEE>,
  initial: FEG,
  reducer: (x0: FEG, x1: FEE) => FEG
): FEG;

export function filter<FEH>(set: Set$<FEH>, predicate: (x0: FEH) => boolean): Set$<
  FEH
>;

export function drop<FEO>(set: Set$<FEO>, disallowed: _.List<FEO>): Set$<FEO>;

export function take<FES>(set: Set$<FES>, desired: _.List<FES>): Set$<FES>;

export function intersection<FFF>(first: Set$<FFF>, second: Set$<FFF>): Set$<
  FFF
>;

export function difference<FFJ>(first: Set$<FFJ>, second: Set$<FFJ>): Set$<FFJ>;

export function is_subset<FFN>(first: Set$<FFN>, second: Set$<FFN>): boolean;

export function is_disjoint<FFQ>(first: Set$<FFQ>, second: Set$<FFQ>): boolean;

export function insert<FDQ>(set: Set$<FDQ>, member: FDQ): Set$<FDQ>;

export function from_list<FEB>(members: _.List<FEB>): Set$<FEB>;

export function map<FEK, FEM>(set: Set$<FEK>, fun: (x0: FEK) => FEM): Set$<FEM>;

export function union<FFB>(first: Set$<FFB>, second: Set$<FFB>): Set$<FFB>;

export function symmetric_difference<FFT>(first: Set$<FFT>, second: Set$<FFT>): Set$<
  FFT
>;
