import type * as _ from "../gleam.d.mts";
import type * as $list from "../gleam/list.d.mts";

class Queue<EYB> extends _.CustomType {
  constructor(in$: _.List<any>, out: _.List<any>);
  
  in$: _.List<any>;
  out: _.List<any>;
}

export type Queue$<EYB> = Queue<EYB>;

export function new$(): Queue$<any>;

export function from_list<EYE>(list: _.List<EYE>): Queue$<EYE>;

export function to_list<EYH>(queue: Queue$<EYH>): _.List<EYH>;

export function is_empty(queue: Queue$<any>): boolean;

export function length(queue: Queue$<any>): number;

export function push_back<EYO>(queue: Queue$<EYO>, item: EYO): Queue$<EYO>;

export function push_front<EYR>(queue: Queue$<EYR>, item: EYR): Queue$<EYR>;

export function pop_back<EYU>(queue: Queue$<EYU>): _.Result<
  [EYU, Queue$<EYU>],
  undefined
>;

export function pop_front<EYZ>(queue: Queue$<EYZ>): _.Result<
  [EYZ, Queue$<EYZ>],
  undefined
>;

export function reverse<EZE>(queue: Queue$<EZE>): Queue$<EZE>;

export function is_logically_equal<EZM>(
  a: Queue$<EZM>,
  b: Queue$<EZM>,
  element_is_equal: (x0: EZM, x1: EZM) => boolean
): boolean;

export function is_equal<EZP>(a: Queue$<EZP>, b: Queue$<EZP>): boolean;
