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

export type Subject$<FWL> = Subject;

export type Selector$<FWM> = any;

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

export type CallError$<FWN> = CalleeDown | CallTimeout;

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

export function send<FWW>(subject: Subject$<FWW>, message: FWW): undefined;

export function new_selector(): Selector$<any>;

export function select<FXE>(from: Selector$<FXE>, within: number): _.Result<
  FXE,
  undefined
>;

export function select_forever<FXI>(from: Selector$<FXI>): FXI;

export function map_selector<FXK, FXM>(a: Selector$<FXK>, b: (x0: FXK) => FXM): Selector$<
  FXM
>;

export function merge_selector<FXO>(a: Selector$<FXO>, b: Selector$<FXO>): Selector$<
  FXO
>;

export function flush_messages(): undefined;

export function selecting_trapped_exits<FXS>(
  selector: Selector$<FXS>,
  handler: (x0: ExitMessage$) => FXS
): Selector$<FXS>;

export function selecting<FXV, FXX>(
  selector: Selector$<FXV>,
  subject: Subject$<FXX>,
  transform: (x0: FXX) => FXV
): Selector$<FXV>;

export function receive<FWY>(subject: Subject$<FWY>, timeout: number): _.Result<
  FWY,
  undefined
>;

export function selecting_record2<FYA>(
  selector: Selector$<FYA>,
  tag: any,
  transform: (x0: $dynamic.Dynamic$) => FYA
): Selector$<FYA>;

export function selecting_record3<FYE>(
  selector: Selector$<FYE>,
  tag: any,
  transform: (x0: $dynamic.Dynamic$, x1: $dynamic.Dynamic$) => FYE
): Selector$<FYE>;

export function selecting_record4<FYI>(
  selector: Selector$<FYI>,
  tag: any,
  transform: (
    x0: $dynamic.Dynamic$,
    x1: $dynamic.Dynamic$,
    x2: $dynamic.Dynamic$
  ) => FYI
): Selector$<FYI>;

export function selecting_record5<FYM>(
  selector: Selector$<FYM>,
  tag: any,
  transform: (
    x0: $dynamic.Dynamic$,
    x1: $dynamic.Dynamic$,
    x2: $dynamic.Dynamic$,
    x3: $dynamic.Dynamic$
  ) => FYM
): Selector$<FYM>;

export function selecting_record6<FYQ>(
  selector: Selector$<FYQ>,
  tag: any,
  transform: (
    x0: $dynamic.Dynamic$,
    x1: $dynamic.Dynamic$,
    x2: $dynamic.Dynamic$,
    x3: $dynamic.Dynamic$,
    x4: $dynamic.Dynamic$
  ) => FYQ
): Selector$<FYQ>;

export function selecting_record7<FYU>(
  selector: Selector$<FYU>,
  tag: any,
  transform: (
    x0: $dynamic.Dynamic$,
    x1: $dynamic.Dynamic$,
    x2: $dynamic.Dynamic$,
    x3: $dynamic.Dynamic$,
    x4: $dynamic.Dynamic$,
    x5: $dynamic.Dynamic$
  ) => FYU
): Selector$<FYU>;

export function selecting_record8<FYY>(
  selector: Selector$<FYY>,
  tag: any,
  transform: (
    x0: $dynamic.Dynamic$,
    x1: $dynamic.Dynamic$,
    x2: $dynamic.Dynamic$,
    x3: $dynamic.Dynamic$,
    x4: $dynamic.Dynamic$,
    x5: $dynamic.Dynamic$,
    x6: $dynamic.Dynamic$
  ) => FYY
): Selector$<FYY>;

export function selecting_anything<FZC>(
  selector: Selector$<FZC>,
  handler: (x0: $dynamic.Dynamic$) => FZC
): Selector$<FZC>;

export function sleep(a: number): undefined;

export function sleep_forever(): undefined;

export function is_alive(a: Pid$): boolean;

export function monitor_process(pid: Pid$): ProcessMonitor$;

export function selecting_process_down<FZK>(
  selector: Selector$<FZK>,
  monitor: ProcessMonitor$,
  mapping: (x0: ProcessDown$) => FZK
): Selector$<FZK>;

export function demonitor_process(monitor: ProcessMonitor$): undefined;

export function try_call<FZN, FZP>(
  subject: Subject$<FZN>,
  make_request: (x0: Subject$<FZP>) => FZN,
  timeout: number
): _.Result<FZP, CallError$<FZP>>;

export function call<FZU, FZW>(
  subject: Subject$<FZU>,
  make_request: (x0: Subject$<FZW>) => FZU,
  timeout: number
): FZW;

export function link(pid: Pid$): boolean;

export function unlink(pid: Pid$): undefined;

export function send_after<FZZ>(
  subject: Subject$<FZZ>,
  delay: number,
  message: FZZ
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
