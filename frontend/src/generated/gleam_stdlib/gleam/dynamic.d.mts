import type * as _ from "../gleam.d.mts";
import type * as $bit_array from "../gleam/bit_array.d.mts";
import type * as $dict from "../gleam/dict.d.mts";
import type * as $int from "../gleam/int.d.mts";
import type * as $list from "../gleam/list.d.mts";
import type * as $option from "../gleam/option.d.mts";
import type * as $result from "../gleam/result.d.mts";
import type * as $string_builder from "../gleam/string_builder.d.mts";

export type Dynamic$ = any;

export class DecodeError extends _.CustomType {
  constructor(expected: string, found: string, path: _.List<string>);
  
  expected: string;
  found: string;
  path: _.List<string>;
}

export type DecodeError$ = DecodeError;

export type DecodeErrors = _.List<DecodeError$>;

export type Decoder = (x0: Dynamic$) => _.Result<any, _.List<DecodeError$>>;

export function from(a: any): Dynamic$;

export function dynamic(value: Dynamic$): _.Result<
  Dynamic$,
  _.List<DecodeError$>
>;

export function bit_array(data: Dynamic$): _.Result<
  _.BitArray,
  _.List<DecodeError$>
>;

export function classify(data: Dynamic$): string;

export function int(data: Dynamic$): _.Result<number, _.List<DecodeError$>>;

export function float(data: Dynamic$): _.Result<number, _.List<DecodeError$>>;

export function bool(data: Dynamic$): _.Result<boolean, _.List<DecodeError$>>;

export function shallow_list(value: Dynamic$): _.Result<
  _.List<Dynamic$>,
  _.List<DecodeError$>
>;

export function optional<DOG>(
  decode: (x0: Dynamic$) => _.Result<DOG, _.List<DecodeError$>>
): (x0: Dynamic$) => _.Result<$option.Option$<DOG>, _.List<DecodeError$>>;

export function any<DSG>(
  decoders: _.List<(x0: Dynamic$) => _.Result<DSG, _.List<DecodeError$>>>
): (x0: Dynamic$) => _.Result<DSG, _.List<DecodeError$>>;

export function decode1<DSK, DSL>(
  constructor: (x0: DSK) => DSL,
  t1: (x0: Dynamic$) => _.Result<DSK, _.List<DecodeError$>>
): (x0: Dynamic$) => _.Result<DSL, _.List<DecodeError$>>;

export function result<DNU, DNW>(
  decode_ok: (x0: Dynamic$) => _.Result<DNU, _.List<DecodeError$>>,
  decode_error: (x0: Dynamic$) => _.Result<DNW, _.List<DecodeError$>>
): (x0: Dynamic$) => _.Result<_.Result<DNU, DNW>, _.List<DecodeError$>>;

export function list<DOB>(
  decoder_type: (x0: Dynamic$) => _.Result<DOB, _.List<DecodeError$>>
): (x0: Dynamic$) => _.Result<_.List<DOB>, _.List<DecodeError$>>;

export function string(data: Dynamic$): _.Result<string, _.List<DecodeError$>>;

export function field<DOQ>(
  name: any,
  inner_type: (x0: Dynamic$) => _.Result<DOQ, _.List<DecodeError$>>
): (x0: Dynamic$) => _.Result<DOQ, _.List<DecodeError$>>;

export function optional_field<DOU>(
  name: any,
  inner_type: (x0: Dynamic$) => _.Result<DOU, _.List<DecodeError$>>
): (x0: Dynamic$) => _.Result<$option.Option$<DOU>, _.List<DecodeError$>>;

export function element<DPC>(
  index: number,
  inner_type: (x0: Dynamic$) => _.Result<DPC, _.List<DecodeError$>>
): (x0: Dynamic$) => _.Result<DPC, _.List<DecodeError$>>;

export function tuple2<DQC, DQE>(
  decode1: (x0: Dynamic$) => _.Result<DQC, _.List<DecodeError$>>,
  decode2: (x0: Dynamic$) => _.Result<DQE, _.List<DecodeError$>>
): (x0: Dynamic$) => _.Result<[DQC, DQE], _.List<DecodeError$>>;

export function tuple3<DQH, DQJ, DQL>(
  decode1: (x0: Dynamic$) => _.Result<DQH, _.List<DecodeError$>>,
  decode2: (x0: Dynamic$) => _.Result<DQJ, _.List<DecodeError$>>,
  decode3: (x0: Dynamic$) => _.Result<DQL, _.List<DecodeError$>>
): (x0: Dynamic$) => _.Result<[DQH, DQJ, DQL], _.List<DecodeError$>>;

export function tuple4<DQO, DQQ, DQS, DQU>(
  decode1: (x0: Dynamic$) => _.Result<DQO, _.List<DecodeError$>>,
  decode2: (x0: Dynamic$) => _.Result<DQQ, _.List<DecodeError$>>,
  decode3: (x0: Dynamic$) => _.Result<DQS, _.List<DecodeError$>>,
  decode4: (x0: Dynamic$) => _.Result<DQU, _.List<DecodeError$>>
): (x0: Dynamic$) => _.Result<[DQO, DQQ, DQS, DQU], _.List<DecodeError$>>;

export function tuple5<DQX, DQZ, DRB, DRD, DRF>(
  decode1: (x0: Dynamic$) => _.Result<DQX, _.List<DecodeError$>>,
  decode2: (x0: Dynamic$) => _.Result<DQZ, _.List<DecodeError$>>,
  decode3: (x0: Dynamic$) => _.Result<DRB, _.List<DecodeError$>>,
  decode4: (x0: Dynamic$) => _.Result<DRD, _.List<DecodeError$>>,
  decode5: (x0: Dynamic$) => _.Result<DRF, _.List<DecodeError$>>
): (x0: Dynamic$) => _.Result<[DQX, DQZ, DRB, DRD, DRF], _.List<DecodeError$>>;

export function tuple6<DRI, DRK, DRM, DRO, DRQ, DRS>(
  decode1: (x0: Dynamic$) => _.Result<DRI, _.List<DecodeError$>>,
  decode2: (x0: Dynamic$) => _.Result<DRK, _.List<DecodeError$>>,
  decode3: (x0: Dynamic$) => _.Result<DRM, _.List<DecodeError$>>,
  decode4: (x0: Dynamic$) => _.Result<DRO, _.List<DecodeError$>>,
  decode5: (x0: Dynamic$) => _.Result<DRQ, _.List<DecodeError$>>,
  decode6: (x0: Dynamic$) => _.Result<DRS, _.List<DecodeError$>>
): (x0: Dynamic$) => _.Result<
  [DRI, DRK, DRM, DRO, DRQ, DRS],
  _.List<DecodeError$>
>;

export function dict<DRV, DRX>(
  key_type: (x0: Dynamic$) => _.Result<DRV, _.List<DecodeError$>>,
  value_type: (x0: Dynamic$) => _.Result<DRX, _.List<DecodeError$>>
): (x0: Dynamic$) => _.Result<$dict.Dict$<DRV, DRX>, _.List<DecodeError$>>;

export function decode2<DSO, DSP, DSQ>(
  constructor: (x0: DSO, x1: DSP) => DSQ,
  t1: (x0: Dynamic$) => _.Result<DSO, _.List<DecodeError$>>,
  t2: (x0: Dynamic$) => _.Result<DSP, _.List<DecodeError$>>
): (x0: Dynamic$) => _.Result<DSQ, _.List<DecodeError$>>;

export function decode3<DSU, DSV, DSW, DSX>(
  constructor: (x0: DSU, x1: DSV, x2: DSW) => DSX,
  t1: (x0: Dynamic$) => _.Result<DSU, _.List<DecodeError$>>,
  t2: (x0: Dynamic$) => _.Result<DSV, _.List<DecodeError$>>,
  t3: (x0: Dynamic$) => _.Result<DSW, _.List<DecodeError$>>
): (x0: Dynamic$) => _.Result<DSX, _.List<DecodeError$>>;

export function decode4<DTC, DTD, DTE, DTF, DTG>(
  constructor: (x0: DTC, x1: DTD, x2: DTE, x3: DTF) => DTG,
  t1: (x0: Dynamic$) => _.Result<DTC, _.List<DecodeError$>>,
  t2: (x0: Dynamic$) => _.Result<DTD, _.List<DecodeError$>>,
  t3: (x0: Dynamic$) => _.Result<DTE, _.List<DecodeError$>>,
  t4: (x0: Dynamic$) => _.Result<DTF, _.List<DecodeError$>>
): (x0: Dynamic$) => _.Result<DTG, _.List<DecodeError$>>;

export function decode5<DTM, DTN, DTO, DTP, DTQ, DTR>(
  constructor: (x0: DTM, x1: DTN, x2: DTO, x3: DTP, x4: DTQ) => DTR,
  t1: (x0: Dynamic$) => _.Result<DTM, _.List<DecodeError$>>,
  t2: (x0: Dynamic$) => _.Result<DTN, _.List<DecodeError$>>,
  t3: (x0: Dynamic$) => _.Result<DTO, _.List<DecodeError$>>,
  t4: (x0: Dynamic$) => _.Result<DTP, _.List<DecodeError$>>,
  t5: (x0: Dynamic$) => _.Result<DTQ, _.List<DecodeError$>>
): (x0: Dynamic$) => _.Result<DTR, _.List<DecodeError$>>;

export function decode6<DTY, DTZ, DUA, DUB, DUC, DUD, DUE>(
  constructor: (x0: DTY, x1: DTZ, x2: DUA, x3: DUB, x4: DUC, x5: DUD) => DUE,
  t1: (x0: Dynamic$) => _.Result<DTY, _.List<DecodeError$>>,
  t2: (x0: Dynamic$) => _.Result<DTZ, _.List<DecodeError$>>,
  t3: (x0: Dynamic$) => _.Result<DUA, _.List<DecodeError$>>,
  t4: (x0: Dynamic$) => _.Result<DUB, _.List<DecodeError$>>,
  t5: (x0: Dynamic$) => _.Result<DUC, _.List<DecodeError$>>,
  t6: (x0: Dynamic$) => _.Result<DUD, _.List<DecodeError$>>
): (x0: Dynamic$) => _.Result<DUE, _.List<DecodeError$>>;

export function decode7<DUM, DUN, DUO, DUP, DUQ, DUR, DUS, DUT>(
  constructor: (x0: DUM, x1: DUN, x2: DUO, x3: DUP, x4: DUQ, x5: DUR, x6: DUS) => DUT,
  t1: (x0: Dynamic$) => _.Result<DUM, _.List<DecodeError$>>,
  t2: (x0: Dynamic$) => _.Result<DUN, _.List<DecodeError$>>,
  t3: (x0: Dynamic$) => _.Result<DUO, _.List<DecodeError$>>,
  t4: (x0: Dynamic$) => _.Result<DUP, _.List<DecodeError$>>,
  t5: (x0: Dynamic$) => _.Result<DUQ, _.List<DecodeError$>>,
  t6: (x0: Dynamic$) => _.Result<DUR, _.List<DecodeError$>>,
  t7: (x0: Dynamic$) => _.Result<DUS, _.List<DecodeError$>>
): (x0: Dynamic$) => _.Result<DUT, _.List<DecodeError$>>;

export function decode8<DVC, DVD, DVE, DVF, DVG, DVH, DVI, DVJ, DVK>(
  constructor: (
    x0: DVC,
    x1: DVD,
    x2: DVE,
    x3: DVF,
    x4: DVG,
    x5: DVH,
    x6: DVI,
    x7: DVJ
  ) => DVK,
  t1: (x0: Dynamic$) => _.Result<DVC, _.List<DecodeError$>>,
  t2: (x0: Dynamic$) => _.Result<DVD, _.List<DecodeError$>>,
  t3: (x0: Dynamic$) => _.Result<DVE, _.List<DecodeError$>>,
  t4: (x0: Dynamic$) => _.Result<DVF, _.List<DecodeError$>>,
  t5: (x0: Dynamic$) => _.Result<DVG, _.List<DecodeError$>>,
  t6: (x0: Dynamic$) => _.Result<DVH, _.List<DecodeError$>>,
  t7: (x0: Dynamic$) => _.Result<DVI, _.List<DecodeError$>>,
  t8: (x0: Dynamic$) => _.Result<DVJ, _.List<DecodeError$>>
): (x0: Dynamic$) => _.Result<DVK, _.List<DecodeError$>>;

export function decode9<DVU, DVV, DVW, DVX, DVY, DVZ, DWA, DWB, DWC, DWD>(
  constructor: (
    x0: DVU,
    x1: DVV,
    x2: DVW,
    x3: DVX,
    x4: DVY,
    x5: DVZ,
    x6: DWA,
    x7: DWB,
    x8: DWC
  ) => DWD,
  t1: (x0: Dynamic$) => _.Result<DVU, _.List<DecodeError$>>,
  t2: (x0: Dynamic$) => _.Result<DVV, _.List<DecodeError$>>,
  t3: (x0: Dynamic$) => _.Result<DVW, _.List<DecodeError$>>,
  t4: (x0: Dynamic$) => _.Result<DVX, _.List<DecodeError$>>,
  t5: (x0: Dynamic$) => _.Result<DVY, _.List<DecodeError$>>,
  t6: (x0: Dynamic$) => _.Result<DVZ, _.List<DecodeError$>>,
  t7: (x0: Dynamic$) => _.Result<DWA, _.List<DecodeError$>>,
  t8: (x0: Dynamic$) => _.Result<DWB, _.List<DecodeError$>>,
  t9: (x0: Dynamic$) => _.Result<DWC, _.List<DecodeError$>>
): (x0: Dynamic$) => _.Result<DWD, _.List<DecodeError$>>;
