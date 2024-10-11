import type * as _ from "../gleam.d.mts";
import type * as $list from "../gleam/list.d.mts";

export function is_ok(result: _.Result<any, any>): boolean;

export function is_error(result: _.Result<any, any>): boolean;

export function map<BNL, BNM, BNP>(
  result: _.Result<BNL, BNM>,
  fun: (x0: BNL) => BNP
): _.Result<BNP, BNM>;

export function map_error<BNS, BNT, BNW>(
  result: _.Result<BNS, BNT>,
  fun: (x0: BNT) => BNW
): _.Result<BNS, BNW>;

export function flatten<BNZ, BOA>(result: _.Result<_.Result<BNZ, BOA>, BOA>): _.Result<
  BNZ,
  BOA
>;

export function try$<BOH, BOI, BOL>(
  result: _.Result<BOH, BOI>,
  fun: (x0: BOH) => _.Result<BOL, BOI>
): _.Result<BOL, BOI>;

export function then$<BOQ, BOR, BOU>(
  result: _.Result<BOQ, BOR>,
  fun: (x0: BOQ) => _.Result<BOU, BOR>
): _.Result<BOU, BOR>;

export function unwrap<BOZ>(result: _.Result<BOZ, any>, default$: BOZ): BOZ;

export function lazy_unwrap<BPD>(
  result: _.Result<BPD, any>,
  default$: () => BPD
): BPD;

export function unwrap_error<BPI>(result: _.Result<any, BPI>, default$: BPI): BPI;

export function unwrap_both<BPL>(result: _.Result<BPL, BPL>): BPL;

export function nil_error<BPO>(result: _.Result<BPO, any>): _.Result<
  BPO,
  undefined
>;

export function or<BPU, BPV>(
  first: _.Result<BPU, BPV>,
  second: _.Result<BPU, BPV>
): _.Result<BPU, BPV>;

export function lazy_or<BQC, BQD>(
  first: _.Result<BQC, BQD>,
  second: () => _.Result<BQC, BQD>
): _.Result<BQC, BQD>;

export function all<BQK, BQL>(results: _.List<_.Result<BQK, BQL>>): _.Result<
  _.List<BQK>,
  BQL
>;

export function partition<BQS, BQT>(results: _.List<_.Result<BQS, BQT>>): [
  _.List<BQS>,
  _.List<BQT>
];

export function replace<BRI, BRL>(result: _.Result<any, BRI>, value: BRL): _.Result<
  BRL,
  BRI
>;

export function replace_error<BRO, BRS>(result: _.Result<BRO, any>, error: BRS): _.Result<
  BRO,
  BRS
>;

export function values<BRV>(results: _.List<_.Result<BRV, any>>): _.List<BRV>;

export function try_recover<BSB, BSC, BSF>(
  result: _.Result<BSB, BSC>,
  fun: (x0: BSC) => _.Result<BSB, BSF>
): _.Result<BSB, BSF>;
