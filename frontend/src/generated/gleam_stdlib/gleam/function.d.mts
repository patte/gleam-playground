export function compose<EUU, EUV, EUW>(
  fun1: (x0: EUU) => EUV,
  fun2: (x0: EUV) => EUW
): (x0: EUU) => EUW;

export function curry2<EUX, EUY, EUZ>(fun: (x0: EUX, x1: EUY) => EUZ): (x0: EUX) => (
  x0: EUY
) => EUZ;

export function curry3<EVB, EVC, EVD, EVE>(
  fun: (x0: EVB, x1: EVC, x2: EVD) => EVE
): (x0: EVB) => (x0: EVC) => (x0: EVD) => EVE;

export function curry4<EVG, EVH, EVI, EVJ, EVK>(
  fun: (x0: EVG, x1: EVH, x2: EVI, x3: EVJ) => EVK
): (x0: EVG) => (x0: EVH) => (x0: EVI) => (x0: EVJ) => EVK;

export function curry5<EVM, EVN, EVO, EVP, EVQ, EVR>(
  fun: (x0: EVM, x1: EVN, x2: EVO, x3: EVP, x4: EVQ) => EVR
): (x0: EVM) => (x0: EVN) => (x0: EVO) => (x0: EVP) => (x0: EVQ) => EVR;

export function curry6<EVT, EVU, EVV, EVW, EVX, EVY, EVZ>(
  fun: (x0: EVT, x1: EVU, x2: EVV, x3: EVW, x4: EVX, x5: EVY) => EVZ
): (x0: EVT) => (x0: EVU) => (x0: EVV) => (x0: EVW) => (x0: EVX) => (x0: EVY) => EVZ;

export function flip<EWB, EWC, EWD>(fun: (x0: EWB, x1: EWC) => EWD): (
  x0: EWC,
  x1: EWB
) => EWD;

export function identity<EWE>(x: EWE): EWE;

export function constant<EWF>(value: EWF): (x0: any) => EWF;

export function tap<EWH>(arg: EWH, effect: (x0: EWH) => any): EWH;

export function apply1<EWJ, EWK>(fun: (x0: EWJ) => EWK, arg1: EWJ): EWK;

export function apply2<EWL, EWM, EWN>(
  fun: (x0: EWL, x1: EWM) => EWN,
  arg1: EWL,
  arg2: EWM
): EWN;

export function apply3<EWO, EWP, EWQ, EWR>(
  fun: (x0: EWO, x1: EWP, x2: EWQ) => EWR,
  arg1: EWO,
  arg2: EWP,
  arg3: EWQ
): EWR;
