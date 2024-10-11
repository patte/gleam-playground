import type * as _ from "../gleam.d.mts";
import type * as $dict from "../gleam/dict.d.mts";
import type * as $int from "../gleam/int.d.mts";
import type * as $list from "../gleam/list.d.mts";
import type * as $option from "../gleam/option.d.mts";
import type * as $order from "../gleam/order.d.mts";
import type * as $result from "../gleam/result.d.mts";

class Iterator<BXF> extends _.CustomType {
  constructor(continuation: () => Action$<any>);
  
  continuation: () => Action$<any>;
}

export type Iterator$<BXF> = Iterator<BXF>;

export class Next<BXG, BXH> extends _.CustomType {
  constructor(element: BXG, accumulator: BXH);
  
  element: BXG;
  accumulator: BXH;
}

export class Done extends _.CustomType {}

export type Step$<BXG, BXH> = Next<BXG, BXH> | Done;

export function unfold<BXS, BXT>(initial: BXS, f: (x0: BXS) => Step$<BXT, BXS>): Iterator$<
  BXT
>;

export function repeatedly<BXX>(f: () => BXX): Iterator$<BXX>;

export function repeat<BXZ>(x: BXZ): Iterator$<BXZ>;

export function from_list<BYB>(list: _.List<BYB>): Iterator$<BYB>;

export function transform<BYL, BYN, BYO>(
  iterator: Iterator$<BYL>,
  initial: BYN,
  f: (x0: BYN, x1: BYL) => Step$<BYO, BYN>
): Iterator$<BYO>;

export function fold<BYV, BYX>(
  iterator: Iterator$<BYV>,
  initial: BYX,
  f: (x0: BYX, x1: BYV) => BYX
): BYX;

export function run(iterator: Iterator$<any>): undefined;

export function to_list<BZA>(iterator: Iterator$<BZA>): _.List<BZA>;

export function step<BZD>(iterator: Iterator$<BZD>): Step$<BZD, Iterator$<BZD>>;

export function take<BZL>(iterator: Iterator$<BZL>, desired: number): Iterator$<
  BZL
>;

export function drop<BZR>(iterator: Iterator$<BZR>, desired: number): Iterator$<
  BZR
>;

export function map<BZY, CAA>(iterator: Iterator$<BZY>, f: (x0: BZY) => CAA): Iterator$<
  CAA
>;

export function map2<CAI, CAK, CAM>(
  iterator1: Iterator$<CAI>,
  iterator2: Iterator$<CAK>,
  fun: (x0: CAI, x1: CAK) => CAM
): Iterator$<CAM>;

export function append<CAS>(first: Iterator$<CAS>, second: Iterator$<CAS>): Iterator$<
  CAS
>;

export function flatten<CBA>(iterator: Iterator$<Iterator$<CBA>>): Iterator$<
  CBA
>;

export function concat<CBE>(iterators: _.List<Iterator$<CBE>>): Iterator$<CBE>;

export function flat_map<CBI, CBK>(
  iterator: Iterator$<CBI>,
  f: (x0: CBI) => Iterator$<CBK>
): Iterator$<CBK>;

export function filter<CBQ>(
  iterator: Iterator$<CBQ>,
  predicate: (x0: CBQ) => boolean
): Iterator$<CBQ>;

export function filter_map<CCA, CCC>(
  iterator: Iterator$<CCA>,
  f: (x0: CCA) => _.Result<CCC, any>
): Iterator$<CCC>;

export function cycle<CCH>(iterator: Iterator$<CCH>): Iterator$<CCH>;

export function find<CCP>(
  haystack: Iterator$<CCP>,
  is_desired: (x0: CCP) => boolean
): _.Result<CCP, undefined>;

export function find_map<CDB, CDD>(
  haystack: Iterator$<CDB>,
  is_desired: (x0: CDB) => _.Result<CDD, any>
): _.Result<CDD, undefined>;

export function index<CDM>(iterator: Iterator$<CDM>): Iterator$<[CDM, number]>;

export function iterate<CDP>(initial: CDP, f: (x0: CDP) => CDP): Iterator$<CDP>;

export function take_while<CDU>(
  iterator: Iterator$<CDU>,
  predicate: (x0: CDU) => boolean
): Iterator$<CDU>;

export function drop_while<CEA>(
  iterator: Iterator$<CEA>,
  predicate: (x0: CEA) => boolean
): Iterator$<CEA>;

export function scan<CEH, CEJ>(
  iterator: Iterator$<CEH>,
  initial: CEJ,
  f: (x0: CEJ, x1: CEH) => CEJ
): Iterator$<CEJ>;

export function zip<CEQ, CES>(left: Iterator$<CEQ>, right: Iterator$<CES>): Iterator$<
  [CEQ, CES]
>;

export function chunk<CFG>(iterator: Iterator$<CFG>, f: (x0: CFG) => any): Iterator$<
  _.List<CFG>
>;

export function sized_chunk<CFT>(iterator: Iterator$<CFT>, count: number): Iterator$<
  _.List<CFT>
>;

export function intersperse<CGA>(iterator: Iterator$<CGA>, elem: CGA): Iterator$<
  CGA
>;

export function any<CGF>(
  iterator: Iterator$<CGF>,
  predicate: (x0: CGF) => boolean
): boolean;

export function all<CGJ>(
  iterator: Iterator$<CGJ>,
  predicate: (x0: CGJ) => boolean
): boolean;

export function group<CGX, CGZ>(iterator: Iterator$<CGX>, key: (x0: CGX) => CGZ): $dict.Dict$<
  CGZ,
  _.List<CGX>
>;

export function reduce<CHD>(
  iterator: Iterator$<CHD>,
  f: (x0: CHD, x1: CHD) => CHD
): _.Result<CHD, undefined>;

export function last<CHH>(iterator: Iterator$<CHH>): _.Result<CHH, undefined>;

export function empty(): Iterator$<any>;

export function once<CHN>(f: () => CHN): Iterator$<CHN>;

export function range(start: number, stop: number): Iterator$<number>;

export function single<CHP>(elem: CHP): Iterator$<CHP>;

export function interleave<CHV>(left: Iterator$<CHV>, right: Iterator$<CHV>): Iterator$<
  CHV
>;

export function fold_until<CID, CIF>(
  iterator: Iterator$<CID>,
  initial: CIF,
  f: (x0: CIF, x1: CID) => $list.ContinueOrStop$<CIF>
): CIF;

export function try_fold<CIP, CIR, CIS>(
  iterator: Iterator$<CIP>,
  initial: CIR,
  f: (x0: CIR, x1: CIP) => _.Result<CIR, CIS>
): _.Result<CIR, CIS>;

export function first<CIX>(iterator: Iterator$<CIX>): _.Result<CIX, undefined>;

export function at<CJB>(iterator: Iterator$<CJB>, index: number): _.Result<
  CJB,
  undefined
>;

export function length(iterator: Iterator$<any>): number;

export function each<CJJ>(iterator: Iterator$<CJJ>, f: (x0: CJJ) => any): undefined;

export function yield$<CJM>(element: CJM, next: () => Iterator$<CJM>): Iterator$<
  CJM
>;
