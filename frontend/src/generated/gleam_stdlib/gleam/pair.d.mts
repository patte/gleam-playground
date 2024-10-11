export function first<YY>(pair: [YY, any]): YY;

export function second<AAB>(pair: [any, AAB]): AAB;

export function swap<AAC, AAD>(pair: [AAC, AAD]): [AAD, AAC];

export function map_first<AAE, AAF, AAG>(
  pair: [AAE, AAF],
  fun: (x0: AAE) => AAG
): [AAG, AAF];

export function map_second<AAH, AAI, AAJ>(
  pair: [AAH, AAI],
  fun: (x0: AAI) => AAJ
): [AAH, AAJ];

export function new$<AAK, AAL>(first: AAK, second: AAL): [AAK, AAL];
