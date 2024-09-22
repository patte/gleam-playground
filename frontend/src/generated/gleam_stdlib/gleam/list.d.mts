import type * as _ from "../gleam.d.mts";
import type * as $dict from "../gleam/dict.d.mts";
import type * as $float from "../gleam/float.d.mts";
import type * as $int from "../gleam/int.d.mts";
import type * as $order from "../gleam/order.d.mts";
import type * as $pair from "../gleam/pair.d.mts";

export class Continue<AAO> extends _.CustomType {
  constructor(argument$0: AAO);
  
  0: AAO;
}

export class Stop<AAO> extends _.CustomType {
  constructor(argument$0: AAO);
  
  0: AAO;
}

export type ContinueOrStop$<AAO> = Continue<AAO> | Stop<AAO>;

export function length(list: _.List<any>): number;

export function reverse<AAV>(xs: _.List<AAV>): _.List<AAV>;

export function is_empty(list: _.List<any>): boolean;

export function contains<ABD>(list: _.List<ABD>, elem: ABD): boolean;

export function first<ABF>(list: _.List<ABF>): _.Result<ABF, undefined>;

export function rest<ABJ>(list: _.List<ABJ>): _.Result<_.List<ABJ>, undefined>;

export function filter<ACG>(list: _.List<ACG>, predicate: (x0: ACG) => boolean): _.List<
  ACG
>;

export function filter_map<ACR, ACT>(
  list: _.List<ACR>,
  fun: (x0: ACR) => _.Result<ACT, any>
): _.List<ACT>;

export function map<ADD, ADF>(list: _.List<ADD>, fun: (x0: ADD) => ADF): _.List<
  ADF
>;

export function map2<ADH, ADJ, ADL>(
  list1: _.List<ADH>,
  list2: _.List<ADJ>,
  fun: (x0: ADH, x1: ADJ) => ADL
): _.List<ADL>;

export function index_map<AEE, AEG>(
  list: _.List<AEE>,
  fun: (x0: AEE, x1: number) => AEG
): _.List<AEG>;

export function try_map<AES, AEU, AEV>(
  list: _.List<AES>,
  fun: (x0: AES) => _.Result<AEU, AEV>
): _.Result<_.List<AEU>, AEV>;

export function drop<AFB>(list: _.List<AFB>, n: number): _.List<AFB>;

export function take<AFI>(list: _.List<AFI>, n: number): _.List<AFI>;

export function new$(): _.List<any>;

export function wrap<AFN>(item: AFN): _.List<AFN>;

export function append<AFP>(first: _.List<AFP>, second: _.List<AFP>): _.List<
  AFP
>;

export function prepend<AFX>(list: _.List<AFX>, item: AFX): _.List<AFX>;

export function concat<AGJ>(lists: _.List<_.List<AGJ>>): _.List<AGJ>;

export function flatten<AGN>(lists: _.List<_.List<AGN>>): _.List<AGN>;

export function flat_map<AGR, AGT>(
  list: _.List<AGR>,
  fun: (x0: AGR) => _.List<AGT>
): _.List<AGT>;

export function fold<AGW, AGY>(
  list: _.List<AGW>,
  initial: AGY,
  fun: (x0: AGY, x1: AGW) => AGY
): AGY;

export function count<AAT>(list: _.List<AAT>, predicate: (x0: AAT) => boolean): number;

export function group<ABW, ABY>(list: _.List<ABW>, key: (x0: ABW) => ABY): $dict.Dict$<
  ABY,
  _.List<ABW>
>;

export function map_fold<ADU, ADW, ADX>(
  list: _.List<ADU>,
  acc: ADW,
  fun: (x0: ADW, x1: ADU) => [ADW, ADX]
): [ADW, _.List<ADX>];

export function fold_right<AGZ, AHB>(
  list: _.List<AGZ>,
  initial: AHB,
  fun: (x0: AHB, x1: AGZ) => AHB
): AHB;

export function index_fold<AHF, AHH>(
  over: _.List<AHF>,
  initial: AHH,
  fun: (x0: AHH, x1: AHF, x2: number) => AHH
): AHH;

export function try_fold<AHI, AHK, AHL>(
  collection: _.List<AHI>,
  accumulator: AHK,
  fun: (x0: AHK, x1: AHI) => _.Result<AHK, AHL>
): _.Result<AHK, AHL>;

export function fold_until<AHQ, AHS>(
  collection: _.List<AHQ>,
  accumulator: AHS,
  fun: (x0: AHS, x1: AHQ) => ContinueOrStop$<AHS>
): AHS;

export function find<AHU>(
  haystack: _.List<AHU>,
  is_desired: (x0: AHU) => boolean
): _.Result<AHU, undefined>;

export function find_map<AHY, AIA>(
  haystack: _.List<AHY>,
  fun: (x0: AHY) => _.Result<AIA, any>
): _.Result<AIA, undefined>;

export function all<AIG>(list: _.List<AIG>, predicate: (x0: AIG) => boolean): boolean;

export function any<AII>(list: _.List<AII>, predicate: (x0: AII) => boolean): boolean;

export function zip<AIQ, AIS>(list: _.List<AIQ>, other: _.List<AIS>): _.List<
  [AIQ, AIS]
>;

export function strict_zip<AIV, AIX>(list: _.List<AIV>, other: _.List<AIX>): _.Result<
  _.List<[AIV, AIX]>,
  undefined
>;

export function unzip<AJG, AJH>(input: _.List<[AJG, AJH]>): [
  _.List<AJG>,
  _.List<AJH>
];

export function intersperse<AJP>(list: _.List<AJP>, elem: AJP): _.List<AJP>;

export function unique<AJS>(list: _.List<AJS>): _.List<AJS>;

export function sort<AJV>(
  list: _.List<AJV>,
  compare: (x0: AJV, x1: AJV) => $order.Order$
): _.List<AJV>;

export function range(start: number, stop: number): _.List<number>;

export function repeat<ALL>(a: ALL, times: number): _.List<ALL>;

export function split<ALS>(list: _.List<ALS>, index: number): [
  _.List<ALS>,
  _.List<ALS>
];

export function split_while<AMB>(
  list: _.List<AMB>,
  predicate: (x0: AMB) => boolean
): [_.List<AMB>, _.List<AMB>];

export function key_find<AMF, AMG>(
  keyword_list: _.List<[AMF, AMG]>,
  desired_key: AMF
): _.Result<AMG, undefined>;

export function key_filter<AMK, AML>(
  keyword_list: _.List<[AMK, AML]>,
  desired_key: AMK
): _.List<AML>;

export function pop<AMS>(
  haystack: _.List<AMS>,
  is_desired: (x0: AMS) => boolean
): _.Result<[AMS, _.List<AMS>], undefined>;

export function pop_map<ANB, AND>(
  haystack: _.List<ANB>,
  is_desired: (x0: ANB) => _.Result<AND, any>
): _.Result<[AND, _.List<ANB>], undefined>;

export function key_pop<ANK, ANL>(haystack: _.List<[ANK, ANL]>, key: ANK): _.Result<
  [ANL, _.List<[ANK, ANL]>],
  undefined
>;

export function key_set<ANQ, ANR>(
  list: _.List<[ANQ, ANR]>,
  key: ANQ,
  value: ANR
): _.List<[ANQ, ANR]>;

export function each<ANU>(list: _.List<ANU>, f: (x0: ANU) => any): undefined;

export function try_each<ANX, AOA>(
  list: _.List<ANX>,
  fun: (x0: ANX) => _.Result<any, AOA>
): _.Result<undefined, AOA>;

export function partition<AOK>(
  list: _.List<AOK>,
  categorise: (x0: AOK) => boolean
): [_.List<AOK>, _.List<AOK>];

export function permutations<AOO>(l: _.List<AOO>): _.List<_.List<AOO>>;

export function window<AOY>(l: _.List<AOY>, n: number): _.List<_.List<AOY>>;

export function window_by_2<APC>(l: _.List<APC>): _.List<[APC, APC]>;

export function drop_while<APF>(
  list: _.List<APF>,
  predicate: (x0: APF) => boolean
): _.List<APF>;

export function take_while<APM>(
  list: _.List<APM>,
  predicate: (x0: APM) => boolean
): _.List<APM>;

export function chunk<APX>(list: _.List<APX>, f: (x0: APX) => any): _.List<
  _.List<APX>
>;

export function sized_chunk<AQJ>(list: _.List<AQJ>, count: number): _.List<
  _.List<AQJ>
>;

export function reduce<AQN>(list: _.List<AQN>, fun: (x0: AQN, x1: AQN) => AQN): _.Result<
  AQN,
  undefined
>;

export function scan<AQW, AQY>(
  list: _.List<AQW>,
  initial: AQY,
  fun: (x0: AQY, x1: AQW) => AQY
): _.List<AQY>;

export function last<ARA>(list: _.List<ARA>): _.Result<ARA, undefined>;

export function combinations<ARE>(items: _.List<ARE>, n: number): _.List<
  _.List<ARE>
>;

export function combination_pairs<ARM>(items: _.List<ARM>): _.List<[ARM, ARM]>;

export function transpose<ART>(list_of_list: _.List<_.List<ART>>): _.List<
  _.List<ART>
>;

export function interleave<ARP>(list: _.List<_.List<ARP>>): _.List<ARP>;

export function shuffle<ASF>(list: _.List<ASF>): _.List<ASF>;
