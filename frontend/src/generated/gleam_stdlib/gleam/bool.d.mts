import type * as $order from "../gleam/order.d.mts";

export function and(a: boolean, b: boolean): boolean;

export function or(a: boolean, b: boolean): boolean;

export function negate(bool: boolean): boolean;

export function nor(a: boolean, b: boolean): boolean;

export function nand(a: boolean, b: boolean): boolean;

export function exclusive_or(a: boolean, b: boolean): boolean;

export function exclusive_nor(a: boolean, b: boolean): boolean;

export function compare(a: boolean, b: boolean): $order.Order$;

export function to_int(bool: boolean): number;

export function to_string(bool: boolean): string;

export function guard<DKF>(
  requirement: boolean,
  consequence: DKF,
  alternative: () => DKF
): DKF;

export function lazy_guard<DKG>(
  requirement: boolean,
  consequence: () => DKG,
  alternative: () => DKG
): DKG;
