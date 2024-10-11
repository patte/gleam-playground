export function compose<EUT, EUU, EUV>(
  fun1: (x0: EUT) => EUU,
  fun2: (x0: EUU) => EUV
): (x0: EUT) => EUV;

export function curry2<EUW, EUX, EUY>(fun: (x0: EUW, x1: EUX) => EUY): (x0: EUW) => (
  x0: EUX
) => EUY;

export function curry3<EVA, EVB, EVC, EVD>(
  fun: (x0: EVA, x1: EVB, x2: EVC) => EVD
): (x0: EVA) => (x0: EVB) => (x0: EVC) => EVD;

export function curry4<EVF, EVG, EVH, EVI, EVJ>(
  fun: (x0: EVF, x1: EVG, x2: EVH, x3: EVI) => EVJ
): (x0: EVF) => (x0: EVG) => (x0: EVH) => (x0: EVI) => EVJ;

export function curry5<EVL, EVM, EVN, EVO, EVP, EVQ>(
  fun: (x0: EVL, x1: EVM, x2: EVN, x3: EVO, x4: EVP) => EVQ
): (x0: EVL) => (x0: EVM) => (x0: EVN) => (x0: EVO) => (x0: EVP) => EVQ;

export function curry6<EVS, EVT, EVU, EVV, EVW, EVX, EVY>(
  fun: (x0: EVS, x1: EVT, x2: EVU, x3: EVV, x4: EVW, x5: EVX) => EVY
): (x0: EVS) => (x0: EVT) => (x0: EVU) => (x0: EVV) => (x0: EVW) => (x0: EVX) => EVY;

export function flip<EWA, EWB, EWC>(fun: (x0: EWA, x1: EWB) => EWC): (
  x0: EWB,
  x1: EWA
) => EWC;

export function identity<EWD>(x: EWD): EWD;

export function constant<EWE>(value: EWE): (x0: any) => EWE;

export function tap<EWG>(arg: EWG, effect: (x0: EWG) => any): EWG;

export function apply1<EWI, EWJ>(fun: (x0: EWI) => EWJ, arg1: EWI): EWJ;

export function apply2<EWK, EWL, EWM>(
  fun: (x0: EWK, x1: EWL) => EWM,
  arg1: EWK,
  arg2: EWL
): EWM;

export function apply3<EWN, EWO, EWP, EWQ>(
  fun: (x0: EWN, x1: EWO, x2: EWP) => EWQ,
  arg1: EWN,
  arg2: EWO,
  arg3: EWP
): EWQ;
