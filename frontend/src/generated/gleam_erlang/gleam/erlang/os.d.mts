import type * as $dict from "../../../gleam_stdlib/gleam/dict.d.mts";
import type * as _ from "../../gleam.d.mts";

export class WindowsNt extends _.CustomType {}

export class Linux extends _.CustomType {}

export class Darwin extends _.CustomType {}

export class FreeBsd extends _.CustomType {}

export class Other extends _.CustomType {
  constructor(argument$0: string);
  
  0: string;
}

export type OsFamily$ = WindowsNt | Linux | Darwin | FreeBsd | Other;

export function get_all_env(): $dict.Dict$<string, string>;

export function get_env(name: string): _.Result<string, undefined>;

export function set_env(name: string, value: string): undefined;

export function unset_env(name: string): undefined;

export function family(): OsFamily$;
