import type * as _ from "../gleam.d.mts";
import type * as $dict from "../gleam/dict.d.mts";
import type * as $int from "../gleam/int.d.mts";
import type * as $list from "../gleam/list.d.mts";
import type * as $option from "../gleam/option.d.mts";
import type * as $order from "../gleam/order.d.mts";
import type * as $result from "../gleam/result.d.mts";

class Iterator<BXG> extends _.CustomType {
  constructor(continuation: () => Action$<any>);
  
  continuation: () => Action$<any>;
}

export type Iterator$<BXG> = Iterator<BXG>;

export class Next<BXH, BXI> extends _.CustomType {
  constructor(element: BXH, accumulator: BXI);
  
  element: BXH;
  accumulator: BXI;
}

export class Done extends _.CustomType {}

export type Step$<BXI, BXH> = Next<BXI, BXH> | Done;

export function unfold<BXT, BXU>(initial: BXT, f: (x0: BXT) => Step$<BXU, BXT>): Iterator$<
  BXU
>;

export function repeatedly<BXY>(f: () => BXY): Iterator$<BXY>;

export function repeat<BYA>(x: BYA): Iterator$<BYA>;

export function from_list<BYC>(list: _.List<BYC>): Iterator$<BYC>;

export function transform<BYM, BYO, BYP>(
  iterator: Iterator$<BYM>,
  initial: BYO,
  f: (x0: BYO, x1: BYM) => Step$<BYP, BYO>
): Iterator$<BYP>;

export function fold<BYW, BYY>(
  iterator: Iterator$<BYW>,
  initial: BYY,
  f: (x0: BYY, x1: BYW) => BYY
): BYY;

export function run(iterator: Iterator$<any>): undefined;

export function to_list<BZB>(iterator: Iterator$<BZB>): _.List<BZB>;

export function step<BZE>(iterator: Iterator$<BZE>): Step$<BZE, Iterator$<BZE>>;

export function take<BZM>(iterator: Iterator$<BZM>, desired: number): Iterator$<
  BZM
>;

export function drop<BZS>(iterator: Iterator$<BZS>, desired: number): Iterator$<
  BZS
>;

export function map<BZZ, CAB>(iterator: Iterator$<BZZ>, f: (x0: BZZ) => CAB): Iterator$<
  CAB
>;

export function map2<CAJ, CAL, CAN>(
  iterator1: Iterator$<CAJ>,
  iterator2: Iterator$<CAL>,
  fun: (x0: CAJ, x1: CAL) => CAN
): Iterator$<CAN>;

export function append<CAT>(first: Iterator$<CAT>, second: Iterator$<CAT>): Iterator$<
  CAT
>;

export function flatten<CBB>(iterator: Iterator$<Iterator$<CBB>>): Iterator$<
  CBB
>;

export function concat<CBF>(iterators: _.List<Iterator$<CBF>>): Iterator$<CBF>;

export function flat_map<CBJ, CBL>(
  iterator: Iterator$<CBJ>,
  f: (x0: CBJ) => Iterator$<CBL>
): Iterator$<CBL>;

export function filter<CBR>(
  iterator: Iterator$<CBR>,
  predicate: (x0: CBR) => boolean
): Iterator$<CBR>;

export function filter_map<CCB, CCD>(
  iterator: Iterator$<CCB>,
  f: (x0: CCB) => _.Result<CCD, any>
): Iterator$<CCD>;

export function cycle<CCI>(iterator: Iterator$<CCI>): Iterator$<CCI>;

export function find<CCQ>(
  haystack: Iterator$<CCQ>,
  is_desired: (x0: CCQ) => boolean
): _.Result<CCQ, undefined>;

export function find_map<CDC, CDE>(
  haystack: Iterator$<CDC>,
  is_desired: (x0: CDC) => _.Result<CDE, any>
): _.Result<CDE, undefined>;

export function index<CDN>(iterator: Iterator$<CDN>): Iterator$<[CDN, number]>;

export function iterate<CDQ>(initial: CDQ, f: (x0: CDQ) => CDQ): Iterator$<CDQ>;

export function take_while<CDV>(
  iterator: Iterator$<CDV>,
  predicate: (x0: CDV) => boolean
): Iterator$<CDV>;

export function drop_while<CEB>(
  iterator: Iterator$<CEB>,
  predicate: (x0: CEB) => boolean
): Iterator$<CEB>;

export function scan<CEI, CEK>(
  iterator: Iterator$<CEI>,
  initial: CEK,
  f: (x0: CEK, x1: CEI) => CEK
): Iterator$<CEK>;

export function zip<CER, CET>(left: Iterator$<CER>, right: Iterator$<CET>): Iterator$<
  [CER, CET]
>;

export function chunk<CFH>(iterator: Iterator$<CFH>, f: (x0: CFH) => any): Iterator$<
  _.List<CFH>
>;

export function sized_chunk<CFU>(iterator: Iterator$<CFU>, count: number): Iterator$<
  _.List<CFU>
>;

export function intersperse<CGB>(iterator: Iterator$<CGB>, elem: CGB): Iterator$<
  CGB
>;

export function any<CGG>(
  iterator: Iterator$<CGG>,
  predicate: (x0: CGG) => boolean
): boolean;

export function all<CGK>(
  iterator: Iterator$<CGK>,
  predicate: (x0: CGK) => boolean
): boolean;

export function group<CGY, CHA>(iterator: Iterator$<CGY>, key: (x0: CGY) => CHA): $dict.Dict$<
  CHA,
  _.List<CGY>
>;

export function reduce<CHE>(
  iterator: Iterator$<CHE>,
  f: (x0: CHE, x1: CHE) => CHE
): _.Result<CHE, undefined>;

export function last<CHI>(iterator: Iterator$<CHI>): _.Result<CHI, undefined>;

export function empty(): Iterator$<any>;

export function once<CHO>(f: () => CHO): Iterator$<CHO>;

export function range(start: number, stop: number): Iterator$<number>;

export function single<CHQ>(elem: CHQ): Iterator$<CHQ>;

export function interleave<CHW>(left: Iterator$<CHW>, right: Iterator$<CHW>): Iterator$<
  CHW
>;

export function fold_until<CIE, CIG>(
  iterator: Iterator$<CIE>,
  initial: CIG,
  f: (x0: CIG, x1: CIE) => $list.ContinueOrStop$<CIG>
): CIG;

export function try_fold<CIQ, CIS, CIT>(
  iterator: Iterator$<CIQ>,
  initial: CIS,
  f: (x0: CIS, x1: CIQ) => _.Result<CIS, CIT>
): _.Result<CIS, CIT>;

export function first<CIY>(iterator: Iterator$<CIY>): _.Result<CIY, undefined>;

export function at<CJC>(iterator: Iterator$<CJC>, index: number): _.Result<
  CJC,
  undefined
>;

export function length(iterator: Iterator$<any>): number;

export function each<CJK>(iterator: Iterator$<CJK>, f: (x0: CJK) => any): undefined;

export function yield$<CJN>(element: CJN, next: () => Iterator$<CJN>): Iterator$<
  CJN
>;
