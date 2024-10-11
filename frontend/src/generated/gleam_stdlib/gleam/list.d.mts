import type * as _ from "../gleam.d.mts";
import type * as $dict from "../gleam/dict.d.mts";
import type * as $float from "../gleam/float.d.mts";
import type * as $int from "../gleam/int.d.mts";
import type * as $order from "../gleam/order.d.mts";
import type * as $pair from "../gleam/pair.d.mts";

export class Continue<AAN> extends _.CustomType {
  constructor(argument$0: AAN);
  
  0: AAN;
}

export class Stop<AAN> extends _.CustomType {
  constructor(argument$0: AAN);
  
  0: AAN;
}

export type ContinueOrStop$<AAN> = Continue<AAN> | Stop<AAN>;

export function length(list: _.List<any>): number;

export function reverse<AAU>(xs: _.List<AAU>): _.List<AAU>;

export function is_empty(list: _.List<any>): boolean;

export function contains<ABC>(list: _.List<ABC>, elem: ABC): boolean;

export function first<ABE>(list: _.List<ABE>): _.Result<ABE, undefined>;

export function rest<ABI>(list: _.List<ABI>): _.Result<_.List<ABI>, undefined>;

export function filter<ACF>(list: _.List<ACF>, predicate: (x0: ACF) => boolean): _.List<
  ACF
>;

export function filter_map<ACQ, ACS>(
  list: _.List<ACQ>,
  fun: (x0: ACQ) => _.Result<ACS, any>
): _.List<ACS>;

export function map<ADC, ADE>(list: _.List<ADC>, fun: (x0: ADC) => ADE): _.List<
  ADE
>;

export function map2<ADG, ADI, ADK>(
  list1: _.List<ADG>,
  list2: _.List<ADI>,
  fun: (x0: ADG, x1: ADI) => ADK
): _.List<ADK>;

export function index_map<AED, AEF>(
  list: _.List<AED>,
  fun: (x0: AED, x1: number) => AEF
): _.List<AEF>;

export function try_map<AER, AET, AEU>(
  list: _.List<AER>,
  fun: (x0: AER) => _.Result<AET, AEU>
): _.Result<_.List<AET>, AEU>;

export function drop<AFA>(list: _.List<AFA>, n: number): _.List<AFA>;

export function take<AFH>(list: _.List<AFH>, n: number): _.List<AFH>;

export function new$(): _.List<any>;

export function wrap<AFM>(item: AFM): _.List<AFM>;

export function append<AFO>(first: _.List<AFO>, second: _.List<AFO>): _.List<
  AFO
>;

export function prepend<AFW>(list: _.List<AFW>, item: AFW): _.List<AFW>;

export function concat<AGI>(lists: _.List<_.List<AGI>>): _.List<AGI>;

export function flatten<AGM>(lists: _.List<_.List<AGM>>): _.List<AGM>;

export function flat_map<AGQ, AGS>(
  list: _.List<AGQ>,
  fun: (x0: AGQ) => _.List<AGS>
): _.List<AGS>;

export function fold<AGV, AGX>(
  list: _.List<AGV>,
  initial: AGX,
  fun: (x0: AGX, x1: AGV) => AGX
): AGX;

export function count<AAS>(list: _.List<AAS>, predicate: (x0: AAS) => boolean): number;

export function group<ABV, ABX>(list: _.List<ABV>, key: (x0: ABV) => ABX): $dict.Dict$<
  ABX,
  _.List<ABV>
>;

export function map_fold<ADT, ADV, ADW>(
  list: _.List<ADT>,
  acc: ADV,
  fun: (x0: ADV, x1: ADT) => [ADV, ADW]
): [ADV, _.List<ADW>];

export function fold_right<AGY, AHA>(
  list: _.List<AGY>,
  initial: AHA,
  fun: (x0: AHA, x1: AGY) => AHA
): AHA;

export function index_fold<AHE, AHG>(
  over: _.List<AHE>,
  initial: AHG,
  fun: (x0: AHG, x1: AHE, x2: number) => AHG
): AHG;

export function try_fold<AHH, AHJ, AHK>(
  collection: _.List<AHH>,
  accumulator: AHJ,
  fun: (x0: AHJ, x1: AHH) => _.Result<AHJ, AHK>
): _.Result<AHJ, AHK>;

export function fold_until<AHP, AHR>(
  collection: _.List<AHP>,
  accumulator: AHR,
  fun: (x0: AHR, x1: AHP) => ContinueOrStop$<AHR>
): AHR;

export function find<AHT>(
  haystack: _.List<AHT>,
  is_desired: (x0: AHT) => boolean
): _.Result<AHT, undefined>;

export function find_map<AHX, AHZ>(
  haystack: _.List<AHX>,
  fun: (x0: AHX) => _.Result<AHZ, any>
): _.Result<AHZ, undefined>;

export function all<AIF>(list: _.List<AIF>, predicate: (x0: AIF) => boolean): boolean;

export function any<AIH>(list: _.List<AIH>, predicate: (x0: AIH) => boolean): boolean;

export function zip<AIP, AIR>(list: _.List<AIP>, other: _.List<AIR>): _.List<
  [AIP, AIR]
>;

export function strict_zip<AIU, AIW>(list: _.List<AIU>, other: _.List<AIW>): _.Result<
  _.List<[AIU, AIW]>,
  undefined
>;

export function unzip<AJF, AJG>(input: _.List<[AJF, AJG]>): [
  _.List<AJF>,
  _.List<AJG>
];

export function intersperse<AJO>(list: _.List<AJO>, elem: AJO): _.List<AJO>;

export function unique<AJR>(list: _.List<AJR>): _.List<AJR>;

export function sort<AJU>(
  list: _.List<AJU>,
  compare: (x0: AJU, x1: AJU) => $order.Order$
): _.List<AJU>;

export function range(start: number, stop: number): _.List<number>;

export function repeat<ALK>(a: ALK, times: number): _.List<ALK>;

export function split<ALR>(list: _.List<ALR>, index: number): [
  _.List<ALR>,
  _.List<ALR>
];

export function split_while<AMA>(
  list: _.List<AMA>,
  predicate: (x0: AMA) => boolean
): [_.List<AMA>, _.List<AMA>];

export function key_find<AME, AMF>(
  keyword_list: _.List<[AME, AMF]>,
  desired_key: AME
): _.Result<AMF, undefined>;

export function key_filter<AMJ, AMK>(
  keyword_list: _.List<[AMJ, AMK]>,
  desired_key: AMJ
): _.List<AMK>;

export function pop<AMR>(
  haystack: _.List<AMR>,
  is_desired: (x0: AMR) => boolean
): _.Result<[AMR, _.List<AMR>], undefined>;

export function pop_map<ANA, ANC>(
  haystack: _.List<ANA>,
  is_desired: (x0: ANA) => _.Result<ANC, any>
): _.Result<[ANC, _.List<ANA>], undefined>;

export function key_pop<ANJ, ANK>(haystack: _.List<[ANJ, ANK]>, key: ANJ): _.Result<
  [ANK, _.List<[ANJ, ANK]>],
  undefined
>;

export function key_set<ANP, ANQ>(
  list: _.List<[ANP, ANQ]>,
  key: ANP,
  value: ANQ
): _.List<[ANP, ANQ]>;

export function each<ANT>(list: _.List<ANT>, f: (x0: ANT) => any): undefined;

export function try_each<ANW, ANZ>(
  list: _.List<ANW>,
  fun: (x0: ANW) => _.Result<any, ANZ>
): _.Result<undefined, ANZ>;

export function partition<AOJ>(
  list: _.List<AOJ>,
  categorise: (x0: AOJ) => boolean
): [_.List<AOJ>, _.List<AOJ>];

export function permutations<AON>(l: _.List<AON>): _.List<_.List<AON>>;

export function window<AOX>(l: _.List<AOX>, n: number): _.List<_.List<AOX>>;

export function window_by_2<APB>(l: _.List<APB>): _.List<[APB, APB]>;

export function drop_while<APE>(
  list: _.List<APE>,
  predicate: (x0: APE) => boolean
): _.List<APE>;

export function take_while<APL>(
  list: _.List<APL>,
  predicate: (x0: APL) => boolean
): _.List<APL>;

export function chunk<APW>(list: _.List<APW>, f: (x0: APW) => any): _.List<
  _.List<APW>
>;

export function sized_chunk<AQI>(list: _.List<AQI>, count: number): _.List<
  _.List<AQI>
>;

export function reduce<AQM>(list: _.List<AQM>, fun: (x0: AQM, x1: AQM) => AQM): _.Result<
  AQM,
  undefined
>;

export function scan<AQV, AQX>(
  list: _.List<AQV>,
  initial: AQX,
  fun: (x0: AQX, x1: AQV) => AQX
): _.List<AQX>;

export function last<AQZ>(list: _.List<AQZ>): _.Result<AQZ, undefined>;

export function combinations<ARD>(items: _.List<ARD>, n: number): _.List<
  _.List<ARD>
>;

export function combination_pairs<ARL>(items: _.List<ARL>): _.List<[ARL, ARL]>;

export function transpose<ARS>(list_of_list: _.List<_.List<ARS>>): _.List<
  _.List<ARS>
>;

export function interleave<ARO>(list: _.List<_.List<ARO>>): _.List<ARO>;

export function shuffle<ASE>(list: _.List<ASE>): _.List<ASE>;
