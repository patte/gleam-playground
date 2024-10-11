import type * as _ from "../gleam.d.mts";

export class Lt extends _.CustomType {}

export class Eq extends _.CustomType {}

export class Gt extends _.CustomType {}

export type Order$ = Lt | Eq | Gt;

export function negate(order: Order$): Order$;

export function to_int(order: Order$): number;

export function compare(a: Order$, b: Order$): Order$;

export function reverse<SM>(orderer: (x0: SM, x1: SM) => Order$): (
  x0: SM,
  x1: SM
) => Order$;

export function break_tie(order: Order$, other: Order$): Order$;

export function lazy_break_tie(order: Order$, comparison: () => Order$): Order$;
