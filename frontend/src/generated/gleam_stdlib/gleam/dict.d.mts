import type * as _ from "../gleam.d.mts";
import type * as $option from "../gleam/option.d.mts";

export type Dict$<DZ, EA> = any;

export function size(dict: Dict$<any, any>): number;

export function to_list<EJ, EK>(dict: Dict$<EJ, EK>): _.List<[EJ, EK]>;

export function new$(): Dict$<any, any>;

export function is_empty(dict: Dict$<any, any>): boolean;

export function get<FQ, FR>(from: Dict$<FQ, FR>, get: FQ): _.Result<
  FR,
  undefined
>;

export function has_key<FA>(dict: Dict$<FA, any>, key: FA): boolean;

export function insert<GC, GD>(dict: Dict$<GC, GD>, key: GC, value: GD): Dict$<
  GC,
  GD
>;

export function from_list<EO, EP>(list: _.List<[EO, EP]>): Dict$<EO, EP>;

export function keys<HC>(dict: Dict$<HC, any>): _.List<HC>;

export function values<HV>(dict: Dict$<any, HV>): _.List<HV>;

export function take<IV, IW>(dict: Dict$<IV, IW>, desired_keys: _.List<IV>): Dict$<
  IV,
  IW
>;

export function merge<JS, JT>(dict: Dict$<JS, JT>, new_entries: Dict$<JS, JT>): Dict$<
  JS,
  JT
>;

export function delete$<KV, KW>(dict: Dict$<KV, KW>, key: KV): Dict$<KV, KW>;

export function drop<LH, LI>(dict: Dict$<LH, LI>, disallowed_keys: _.List<LH>): Dict$<
  LH,
  LI
>;

export function upsert<LO, LP>(
  dict: Dict$<LO, LP>,
  key: LO,
  fun: (x0: $option.Option$<LP>) => LP
): Dict$<LO, LP>;

export function fold<LZ, MA, MD>(
  dict: Dict$<LZ, MA>,
  initial: MD,
  fun: (x0: MD, x1: LZ, x2: MA) => MD
): MD;

export function map_values<GO, GP, GS>(
  dict: Dict$<GO, GP>,
  fun: (x0: GO, x1: GP) => GS
): Dict$<GO, GS>;

export function filter<IJ, IK>(
  dict: Dict$<IJ, IK>,
  predicate: (x0: IJ, x1: IK) => boolean
): Dict$<IJ, IK>;

export function each<ME, MF>(dict: Dict$<ME, MF>, fun: (x0: ME, x1: MF) => any): undefined;

export function combine<MJ, MK>(
  dict: Dict$<MJ, MK>,
  other: Dict$<MJ, MK>,
  fun: (x0: MK, x1: MK) => MK
): Dict$<MJ, MK>;
