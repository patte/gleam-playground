import type * as $option from "../../gleam_stdlib/gleam/option.d.mts";
import type * as $string from "../../gleam_stdlib/gleam/string.d.mts";
import type * as _ from "../gleam.d.mts";

export function equal<GIA>(a: GIA, b: GIA): undefined;

export function not_equal<GIB>(a: GIB, b: GIB): undefined;

export function be_ok<GIC>(a: _.Result<GIC, any>): GIC;

export function be_error<GIH>(a: _.Result<any, GIH>): GIH;

export function be_some<GIK>(a: $option.Option$<GIK>): GIK;

export function be_none(a: $option.Option$<any>): undefined;

export function be_true(actual: boolean): undefined;

export function be_false(actual: boolean): undefined;

export function fail(): undefined;
