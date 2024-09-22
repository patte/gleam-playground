import type * as $option from "../../gleam_stdlib/gleam/option.d.mts";
import type * as $string from "../../gleam_stdlib/gleam/string.d.mts";
import type * as _ from "../gleam.d.mts";

export function equal<GIB>(a: GIB, b: GIB): undefined;

export function not_equal<GIC>(a: GIC, b: GIC): undefined;

export function be_ok<GID>(a: _.Result<GID, any>): GID;

export function be_error<GII>(a: _.Result<any, GII>): GII;

export function be_some<GIL>(a: $option.Option$<GIL>): GIL;

export function be_none(a: $option.Option$<any>): undefined;

export function be_true(actual: boolean): undefined;

export function be_false(actual: boolean): undefined;

export function fail(): undefined;
