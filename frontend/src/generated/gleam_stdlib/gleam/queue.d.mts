import type * as _ from "../gleam.d.mts";
import type * as $list from "../gleam/list.d.mts";

class Queue<EYA> extends _.CustomType {
  constructor(in$: _.List<any>, out: _.List<any>);
  
  in$: _.List<any>;
  out: _.List<any>;
}

export type Queue$<EYA> = Queue<EYA>;

export function new$(): Queue$<any>;

export function from_list<EYD>(list: _.List<EYD>): Queue$<EYD>;

export function to_list<EYG>(queue: Queue$<EYG>): _.List<EYG>;

export function is_empty(queue: Queue$<any>): boolean;

export function length(queue: Queue$<any>): number;

export function push_back<EYN>(queue: Queue$<EYN>, item: EYN): Queue$<EYN>;

export function push_front<EYQ>(queue: Queue$<EYQ>, item: EYQ): Queue$<EYQ>;

export function pop_back<EYT>(queue: Queue$<EYT>): _.Result<
  [EYT, Queue$<EYT>],
  undefined
>;

export function pop_front<EYY>(queue: Queue$<EYY>): _.Result<
  [EYY, Queue$<EYY>],
  undefined
>;

export function reverse<EZD>(queue: Queue$<EZD>): Queue$<EZD>;

export function is_logically_equal<EZL>(
  a: Queue$<EZL>,
  b: Queue$<EZL>,
  element_is_equal: (x0: EZL, x1: EZL) => boolean
): boolean;

export function is_equal<EZO>(a: Queue$<EZO>, b: Queue$<EZO>): boolean;
