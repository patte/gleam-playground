import type * as _ from "../gleam.d.mts";

export class Some<I> extends _.CustomType {
  constructor(argument$0: I);
  
  0: I;
}

export class None extends _.CustomType {}

export type Option$<I> = Some<I> | None;

export function all<P>(list: _.List<Option$<P>>): Option$<_.List<P>>;

export function is_some(option: Option$<any>): boolean;

export function is_none(option: Option$<any>): boolean;

export function to_result<Y, AB>(option: Option$<Y>, e: AB): _.Result<Y, AB>;

export function from_result<AE>(result: _.Result<AE, any>): Option$<AE>;

export function unwrap<AJ>(option: Option$<AJ>, default$: AJ): AJ;

export function lazy_unwrap<AL>(option: Option$<AL>, default$: () => AL): AL;

export function map<AN, AP>(option: Option$<AN>, fun: (x0: AN) => AP): Option$<
  AP
>;

export function flatten<AR>(option: Option$<Option$<AR>>): Option$<AR>;

export function then$<AV, AX>(option: Option$<AV>, fun: (x0: AV) => Option$<AX>): Option$<
  AX
>;

export function or<BA>(first: Option$<BA>, second: Option$<BA>): Option$<BA>;

export function lazy_or<BE>(first: Option$<BE>, second: () => Option$<BE>): Option$<
  BE
>;

export function values<BN>(options: _.List<Option$<BN>>): _.List<BN>;
