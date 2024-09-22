import type * as $string from "../gleam/string.d.mts";

export function print(string: string): undefined;

export function print_error(string: string): undefined;

export function println(string: string): undefined;

export function println_error(string: string): undefined;

export function debug<EXR>(term: EXR): EXR;
