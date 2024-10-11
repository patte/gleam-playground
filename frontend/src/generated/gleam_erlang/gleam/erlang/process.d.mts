import type * as $dynamic from "../../../gleam_stdlib/gleam/dynamic.d.mts";
import type * as $string from "../../../gleam_stdlib/gleam/string.d.mts";
import type * as _ from "../../gleam.d.mts";
import type * as $erlang from "../../gleam/erlang.d.mts";
import type * as $atom from "../../gleam/erlang/atom.d.mts";

export type Pid$ = any;

class Subject extends _.CustomType {
  constructor(owner: Pid$, tag: $erlang.Reference$);
  
  owner: Pid$;
  tag: $erlang.Reference$;
}

export type Subject$<FWK> = Subject;

export type Selector$<FWL> = any;

export class ExitMessage extends _.CustomType {
  constructor(pid: Pid$, reason: ExitReason$);
  
  pid: Pid$;
  reason: ExitReason$;
}

export type ExitMessage$ = ExitMessage;

export class Normal extends _.CustomType {}

export class Killed extends _.CustomType {}

export class Abnormal extends _.CustomType {
  constructor(reason: string);
  
  reason: string;
}

export type ExitReason$ = Normal | Killed | Abnormal;

class ProcessMonitor extends _.CustomType {
  constructor(tag: $erlang.Reference$);
  
  tag: $erlang.Reference$;
}

export type ProcessMonitor$ = ProcessMonitor;

export class ProcessDown extends _.CustomType {
  constructor(pid: Pid$, reason: $dynamic.Dynamic$);
  
  pid: Pid$;
  reason: $dynamic.Dynamic$;
}

export type ProcessDown$ = ProcessDown;

export class CalleeDown extends _.CustomType {
  constructor(reason: $dynamic.Dynamic$);
  
  reason: $dynamic.Dynamic$;
}

export class CallTimeout extends _.CustomType {}

export type CallError$<FWM> = CalleeDown | CallTimeout;

export type Timer$ = any;

export class TimerNotFound extends _.CustomType {}

export class Cancelled extends _.CustomType {
  constructor(time_remaining: number);
  
  time_remaining: number;
}

export type Cancelled$ = TimerNotFound | Cancelled;

export function self(): Pid$;

export function start(implementation: () => any, link: boolean): Pid$;

export function new_subject(): Subject$<any>;

export function subject_owner(subject: Subject$<any>): Pid$;

export function send<FWV>(subject: Subject$<FWV>, message: FWV): undefined;

export function new_selector(): Selector$<any>;

export function select<FXD>(from: Selector$<FXD>, within: number): _.Result<
  FXD,
  undefined
>;

export function select_forever<FXH>(from: Selector$<FXH>): FXH;

export function map_selector<FXJ, FXL>(a: Selector$<FXJ>, b: (x0: FXJ) => FXL): Selector$<
  FXL
>;

export function merge_selector<FXN>(a: Selector$<FXN>, b: Selector$<FXN>): Selector$<
  FXN
>;

export function flush_messages(): undefined;

export function selecting_trapped_exits<FXR>(
  selector: Selector$<FXR>,
  handler: (x0: ExitMessage$) => FXR
): Selector$<FXR>;

export function selecting<FXU, FXW>(
  selector: Selector$<FXU>,
  subject: Subject$<FXW>,
  transform: (x0: FXW) => FXU
): Selector$<FXU>;

export function receive<FWX>(subject: Subject$<FWX>, timeout: number): _.Result<
  FWX,
  undefined
>;

export function selecting_record2<FXZ>(
  selector: Selector$<FXZ>,
  tag: any,
  transform: (x0: $dynamic.Dynamic$) => FXZ
): Selector$<FXZ>;

export function selecting_record3<FYD>(
  selector: Selector$<FYD>,
  tag: any,
  transform: (x0: $dynamic.Dynamic$, x1: $dynamic.Dynamic$) => FYD
): Selector$<FYD>;

export function selecting_record4<FYH>(
  selector: Selector$<FYH>,
  tag: any,
  transform: (
    x0: $dynamic.Dynamic$,
    x1: $dynamic.Dynamic$,
    x2: $dynamic.Dynamic$
  ) => FYH
): Selector$<FYH>;

export function selecting_record5<FYL>(
  selector: Selector$<FYL>,
  tag: any,
  transform: (
    x0: $dynamic.Dynamic$,
    x1: $dynamic.Dynamic$,
    x2: $dynamic.Dynamic$,
    x3: $dynamic.Dynamic$
  ) => FYL
): Selector$<FYL>;

export function selecting_record6<FYP>(
  selector: Selector$<FYP>,
  tag: any,
  transform: (
    x0: $dynamic.Dynamic$,
    x1: $dynamic.Dynamic$,
    x2: $dynamic.Dynamic$,
    x3: $dynamic.Dynamic$,
    x4: $dynamic.Dynamic$
  ) => FYP
): Selector$<FYP>;

export function selecting_record7<FYT>(
  selector: Selector$<FYT>,
  tag: any,
  transform: (
    x0: $dynamic.Dynamic$,
    x1: $dynamic.Dynamic$,
    x2: $dynamic.Dynamic$,
    x3: $dynamic.Dynamic$,
    x4: $dynamic.Dynamic$,
    x5: $dynamic.Dynamic$
  ) => FYT
): Selector$<FYT>;

export function selecting_record8<FYX>(
  selector: Selector$<FYX>,
  tag: any,
  transform: (
    x0: $dynamic.Dynamic$,
    x1: $dynamic.Dynamic$,
    x2: $dynamic.Dynamic$,
    x3: $dynamic.Dynamic$,
    x4: $dynamic.Dynamic$,
    x5: $dynamic.Dynamic$,
    x6: $dynamic.Dynamic$
  ) => FYX
): Selector$<FYX>;

export function selecting_anything<FZB>(
  selector: Selector$<FZB>,
  handler: (x0: $dynamic.Dynamic$) => FZB
): Selector$<FZB>;

export function sleep(a: number): undefined;

export function sleep_forever(): undefined;

export function is_alive(a: Pid$): boolean;

export function monitor_process(pid: Pid$): ProcessMonitor$;

export function selecting_process_down<FZJ>(
  selector: Selector$<FZJ>,
  monitor: ProcessMonitor$,
  mapping: (x0: ProcessDown$) => FZJ
): Selector$<FZJ>;

export function demonitor_process(monitor: ProcessMonitor$): undefined;

export function try_call<FZM, FZO>(
  subject: Subject$<FZM>,
  make_request: (x0: Subject$<FZO>) => FZM,
  timeout: number
): _.Result<FZO, CallError$<FZO>>;

export function call<FZT, FZV>(
  subject: Subject$<FZT>,
  make_request: (x0: Subject$<FZV>) => FZT,
  timeout: number
): FZV;

export function link(pid: Pid$): boolean;

export function unlink(pid: Pid$): undefined;

export function send_after<FZY>(
  subject: Subject$<FZY>,
  delay: number,
  message: FZY
): Timer$;

export function cancel_timer(timer: Timer$): Cancelled$;

export function kill(pid: Pid$): undefined;

export function send_exit(pid: Pid$): undefined;

export function send_abnormal_exit(pid: Pid$, reason: string): undefined;

export function trap_exits(a: boolean): undefined;

export function register(pid: Pid$, name: $atom.Atom$): _.Result<
  undefined,
  undefined
>;

export function unregister(name: $atom.Atom$): _.Result<undefined, undefined>;

export function named(name: $atom.Atom$): _.Result<Pid$, undefined>;

export function pid_from_dynamic(from: $dynamic.Dynamic$): _.Result<
  Pid$,
  _.List<$dynamic.DecodeError$>
>;
