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

export function optional<DOF>(
  decode: (x0: Dynamic$) => _.Result<DOF, _.List<DecodeError$>>
): (x0: Dynamic$) => _.Result<$option.Option$<DOF>, _.List<DecodeError$>>;

export function any<DSF>(
  decoders: _.List<(x0: Dynamic$) => _.Result<DSF, _.List<DecodeError$>>>
): (x0: Dynamic$) => _.Result<DSF, _.List<DecodeError$>>;

export function decode1<DSJ, DSK>(
  constructor: (x0: DSJ) => DSK,
  t1: (x0: Dynamic$) => _.Result<DSJ, _.List<DecodeError$>>
): (x0: Dynamic$) => _.Result<DSK, _.List<DecodeError$>>;

export function result<DNT, DNV>(
  decode_ok: (x0: Dynamic$) => _.Result<DNT, _.List<DecodeError$>>,
  decode_error: (x0: Dynamic$) => _.Result<DNV, _.List<DecodeError$>>
): (x0: Dynamic$) => _.Result<_.Result<DNT, DNV>, _.List<DecodeError$>>;

export function list<DOA>(
  decoder_type: (x0: Dynamic$) => _.Result<DOA, _.List<DecodeError$>>
): (x0: Dynamic$) => _.Result<_.List<DOA>, _.List<DecodeError$>>;

export function string(data: Dynamic$): _.Result<string, _.List<DecodeError$>>;

export function field<DOP>(
  name: any,
  inner_type: (x0: Dynamic$) => _.Result<DOP, _.List<DecodeError$>>
): (x0: Dynamic$) => _.Result<DOP, _.List<DecodeError$>>;

export function optional_field<DOT>(
  name: any,
  inner_type: (x0: Dynamic$) => _.Result<DOT, _.List<DecodeError$>>
): (x0: Dynamic$) => _.Result<$option.Option$<DOT>, _.List<DecodeError$>>;

export function element<DPB>(
  index: number,
  inner_type: (x0: Dynamic$) => _.Result<DPB, _.List<DecodeError$>>
): (x0: Dynamic$) => _.Result<DPB, _.List<DecodeError$>>;

export function tuple2<DQB, DQD>(
  decode1: (x0: Dynamic$) => _.Result<DQB, _.List<DecodeError$>>,
  decode2: (x0: Dynamic$) => _.Result<DQD, _.List<DecodeError$>>
): (x0: Dynamic$) => _.Result<[DQB, DQD], _.List<DecodeError$>>;

export function tuple3<DQG, DQI, DQK>(
  decode1: (x0: Dynamic$) => _.Result<DQG, _.List<DecodeError$>>,
  decode2: (x0: Dynamic$) => _.Result<DQI, _.List<DecodeError$>>,
  decode3: (x0: Dynamic$) => _.Result<DQK, _.List<DecodeError$>>
): (x0: Dynamic$) => _.Result<[DQG, DQI, DQK], _.List<DecodeError$>>;

export function tuple4<DQN, DQP, DQR, DQT>(
  decode1: (x0: Dynamic$) => _.Result<DQN, _.List<DecodeError$>>,
  decode2: (x0: Dynamic$) => _.Result<DQP, _.List<DecodeError$>>,
  decode3: (x0: Dynamic$) => _.Result<DQR, _.List<DecodeError$>>,
  decode4: (x0: Dynamic$) => _.Result<DQT, _.List<DecodeError$>>
): (x0: Dynamic$) => _.Result<[DQN, DQP, DQR, DQT], _.List<DecodeError$>>;

export function tuple5<DQW, DQY, DRA, DRC, DRE>(
  decode1: (x0: Dynamic$) => _.Result<DQW, _.List<DecodeError$>>,
  decode2: (x0: Dynamic$) => _.Result<DQY, _.List<DecodeError$>>,
  decode3: (x0: Dynamic$) => _.Result<DRA, _.List<DecodeError$>>,
  decode4: (x0: Dynamic$) => _.Result<DRC, _.List<DecodeError$>>,
  decode5: (x0: Dynamic$) => _.Result<DRE, _.List<DecodeError$>>
): (x0: Dynamic$) => _.Result<[DQW, DQY, DRA, DRC, DRE], _.List<DecodeError$>>;

export function tuple6<DRH, DRJ, DRL, DRN, DRP, DRR>(
  decode1: (x0: Dynamic$) => _.Result<DRH, _.List<DecodeError$>>,
  decode2: (x0: Dynamic$) => _.Result<DRJ, _.List<DecodeError$>>,
  decode3: (x0: Dynamic$) => _.Result<DRL, _.List<DecodeError$>>,
  decode4: (x0: Dynamic$) => _.Result<DRN, _.List<DecodeError$>>,
  decode5: (x0: Dynamic$) => _.Result<DRP, _.List<DecodeError$>>,
  decode6: (x0: Dynamic$) => _.Result<DRR, _.List<DecodeError$>>
): (x0: Dynamic$) => _.Result<
  [DRH, DRJ, DRL, DRN, DRP, DRR],
  _.List<DecodeError$>
>;

export function dict<DRU, DRW>(
  key_type: (x0: Dynamic$) => _.Result<DRU, _.List<DecodeError$>>,
  value_type: (x0: Dynamic$) => _.Result<DRW, _.List<DecodeError$>>
): (x0: Dynamic$) => _.Result<$dict.Dict$<DRU, DRW>, _.List<DecodeError$>>;

export function decode2<DSN, DSO, DSP>(
  constructor: (x0: DSN, x1: DSO) => DSP,
  t1: (x0: Dynamic$) => _.Result<DSN, _.List<DecodeError$>>,
  t2: (x0: Dynamic$) => _.Result<DSO, _.List<DecodeError$>>
): (x0: Dynamic$) => _.Result<DSP, _.List<DecodeError$>>;

export function decode3<DST, DSU, DSV, DSW>(
  constructor: (x0: DST, x1: DSU, x2: DSV) => DSW,
  t1: (x0: Dynamic$) => _.Result<DST, _.List<DecodeError$>>,
  t2: (x0: Dynamic$) => _.Result<DSU, _.List<DecodeError$>>,
  t3: (x0: Dynamic$) => _.Result<DSV, _.List<DecodeError$>>
): (x0: Dynamic$) => _.Result<DSW, _.List<DecodeError$>>;

export function decode4<DTB, DTC, DTD, DTE, DTF>(
  constructor: (x0: DTB, x1: DTC, x2: DTD, x3: DTE) => DTF,
  t1: (x0: Dynamic$) => _.Result<DTB, _.List<DecodeError$>>,
  t2: (x0: Dynamic$) => _.Result<DTC, _.List<DecodeError$>>,
  t3: (x0: Dynamic$) => _.Result<DTD, _.List<DecodeError$>>,
  t4: (x0: Dynamic$) => _.Result<DTE, _.List<DecodeError$>>
): (x0: Dynamic$) => _.Result<DTF, _.List<DecodeError$>>;

export function decode5<DTL, DTM, DTN, DTO, DTP, DTQ>(
  constructor: (x0: DTL, x1: DTM, x2: DTN, x3: DTO, x4: DTP) => DTQ,
  t1: (x0: Dynamic$) => _.Result<DTL, _.List<DecodeError$>>,
  t2: (x0: Dynamic$) => _.Result<DTM, _.List<DecodeError$>>,
  t3: (x0: Dynamic$) => _.Result<DTN, _.List<DecodeError$>>,
  t4: (x0: Dynamic$) => _.Result<DTO, _.List<DecodeError$>>,
  t5: (x0: Dynamic$) => _.Result<DTP, _.List<DecodeError$>>
): (x0: Dynamic$) => _.Result<DTQ, _.List<DecodeError$>>;

export function decode6<DTX, DTY, DTZ, DUA, DUB, DUC, DUD>(
  constructor: (x0: DTX, x1: DTY, x2: DTZ, x3: DUA, x4: DUB, x5: DUC) => DUD,
  t1: (x0: Dynamic$) => _.Result<DTX, _.List<DecodeError$>>,
  t2: (x0: Dynamic$) => _.Result<DTY, _.List<DecodeError$>>,
  t3: (x0: Dynamic$) => _.Result<DTZ, _.List<DecodeError$>>,
  t4: (x0: Dynamic$) => _.Result<DUA, _.List<DecodeError$>>,
  t5: (x0: Dynamic$) => _.Result<DUB, _.List<DecodeError$>>,
  t6: (x0: Dynamic$) => _.Result<DUC, _.List<DecodeError$>>
): (x0: Dynamic$) => _.Result<DUD, _.List<DecodeError$>>;

export function decode7<DUL, DUM, DUN, DUO, DUP, DUQ, DUR, DUS>(
  constructor: (x0: DUL, x1: DUM, x2: DUN, x3: DUO, x4: DUP, x5: DUQ, x6: DUR) => DUS,
  t1: (x0: Dynamic$) => _.Result<DUL, _.List<DecodeError$>>,
  t2: (x0: Dynamic$) => _.Result<DUM, _.List<DecodeError$>>,
  t3: (x0: Dynamic$) => _.Result<DUN, _.List<DecodeError$>>,
  t4: (x0: Dynamic$) => _.Result<DUO, _.List<DecodeError$>>,
  t5: (x0: Dynamic$) => _.Result<DUP, _.List<DecodeError$>>,
  t6: (x0: Dynamic$) => _.Result<DUQ, _.List<DecodeError$>>,
  t7: (x0: Dynamic$) => _.Result<DUR, _.List<DecodeError$>>
): (x0: Dynamic$) => _.Result<DUS, _.List<DecodeError$>>;

export function decode8<DVB, DVC, DVD, DVE, DVF, DVG, DVH, DVI, DVJ>(
  constructor: (
    x0: DVB,
    x1: DVC,
    x2: DVD,
    x3: DVE,
    x4: DVF,
    x5: DVG,
    x6: DVH,
    x7: DVI
  ) => DVJ,
  t1: (x0: Dynamic$) => _.Result<DVB, _.List<DecodeError$>>,
  t2: (x0: Dynamic$) => _.Result<DVC, _.List<DecodeError$>>,
  t3: (x0: Dynamic$) => _.Result<DVD, _.List<DecodeError$>>,
  t4: (x0: Dynamic$) => _.Result<DVE, _.List<DecodeError$>>,
  t5: (x0: Dynamic$) => _.Result<DVF, _.List<DecodeError$>>,
  t6: (x0: Dynamic$) => _.Result<DVG, _.List<DecodeError$>>,
  t7: (x0: Dynamic$) => _.Result<DVH, _.List<DecodeError$>>,
  t8: (x0: Dynamic$) => _.Result<DVI, _.List<DecodeError$>>
): (x0: Dynamic$) => _.Result<DVJ, _.List<DecodeError$>>;

export function decode9<DVT, DVU, DVV, DVW, DVX, DVY, DVZ, DWA, DWB, DWC>(
  constructor: (
    x0: DVT,
    x1: DVU,
    x2: DVV,
    x3: DVW,
    x4: DVX,
    x5: DVY,
    x6: DVZ,
    x7: DWA,
    x8: DWB
  ) => DWC,
  t1: (x0: Dynamic$) => _.Result<DVT, _.List<DecodeError$>>,
  t2: (x0: Dynamic$) => _.Result<DVU, _.List<DecodeError$>>,
  t3: (x0: Dynamic$) => _.Result<DVV, _.List<DecodeError$>>,
  t4: (x0: Dynamic$) => _.Result<DVW, _.List<DecodeError$>>,
  t5: (x0: Dynamic$) => _.Result<DVX, _.List<DecodeError$>>,
  t6: (x0: Dynamic$) => _.Result<DVY, _.List<DecodeError$>>,
  t7: (x0: Dynamic$) => _.Result<DVZ, _.List<DecodeError$>>,
  t8: (x0: Dynamic$) => _.Result<DWA, _.List<DecodeError$>>,
  t9: (x0: Dynamic$) => _.Result<DWB, _.List<DecodeError$>>
): (x0: Dynamic$) => _.Result<DWC, _.List<DecodeError$>>;
