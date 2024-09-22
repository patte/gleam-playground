import type * as _ from "./gleam.d.mts";

export class Emergency extends _.CustomType {}

export class Alert extends _.CustomType {}

export class Critical extends _.CustomType {}

export class Error extends _.CustomType {}

export class Warning extends _.CustomType {}

export class Notice extends _.CustomType {}

export class Info extends _.CustomType {}

export class Debug extends _.CustomType {}

export type LogLevel$ = Emergency | Alert | Critical | Error | Warning | Notice | Info | Debug;

export function configure(): undefined;

export function log(level: LogLevel$, message: string): undefined;

export function set_level(level: LogLevel$): undefined;
