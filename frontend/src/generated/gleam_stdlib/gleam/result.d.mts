import type * as _ from "../gleam.d.mts";
import type * as $list from "../gleam/list.d.mts";

export function is_ok(result: _.Result<any, any>): boolean;

export function is_error(result: _.Result<any, any>): boolean;

export function map<BNM, BNN, BNQ>(
  result: _.Result<BNM, BNN>,
  fun: (x0: BNM) => BNQ
): _.Result<BNQ, BNN>;

export function map_error<BNT, BNU, BNX>(
  result: _.Result<BNT, BNU>,
  fun: (x0: BNU) => BNX
): _.Result<BNT, BNX>;

export function flatten<BOA, BOB>(result: _.Result<_.Result<BOA, BOB>, BOB>): _.Result<
  BOA,
  BOB
>;

export function try$<BOI, BOJ, BOM>(
  result: _.Result<BOI, BOJ>,
  fun: (x0: BOI) => _.Result<BOM, BOJ>
): _.Result<BOM, BOJ>;

export function then$<BOR, BOS, BOV>(
  result: _.Result<BOR, BOS>,
  fun: (x0: BOR) => _.Result<BOV, BOS>
): _.Result<BOV, BOS>;

export function unwrap<BPA>(result: _.Result<BPA, any>, default$: BPA): BPA;

export function lazy_unwrap<BPE>(
  result: _.Result<BPE, any>,
  default$: () => BPE
): BPE;

export function unwrap_error<BPJ>(result: _.Result<any, BPJ>, default$: BPJ): BPJ;

export function unwrap_both<BPM>(result: _.Result<BPM, BPM>): BPM;

export function nil_error<BPP>(result: _.Result<BPP, any>): _.Result<
  BPP,
  undefined
>;

export function or<BPV, BPW>(
  first: _.Result<BPV, BPW>,
  second: _.Result<BPV, BPW>
): _.Result<BPV, BPW>;

export function lazy_or<BQD, BQE>(
  first: _.Result<BQD, BQE>,
  second: () => _.Result<BQD, BQE>
): _.Result<BQD, BQE>;

export function all<BQL, BQM>(results: _.List<_.Result<BQL, BQM>>): _.Result<
  _.List<BQL>,
  BQM
>;

export function partition<BQT, BQU>(results: _.List<_.Result<BQT, BQU>>): [
  _.List<BQT>,
  _.List<BQU>
];

export function replace<BRJ, BRM>(result: _.Result<any, BRJ>, value: BRM): _.Result<
  BRM,
  BRJ
>;

export function replace_error<BRP, BRT>(result: _.Result<BRP, any>, error: BRT): _.Result<
  BRP,
  BRT
>;

export function values<BRW>(results: _.List<_.Result<BRW, any>>): _.List<BRW>;

export function try_recover<BSC, BSD, BSG>(
  result: _.Result<BSC, BSD>,
  fun: (x0: BSD) => _.Result<BSC, BSG>
): _.Result<BSC, BSG>;
