export function first<YZ>(pair: [YZ, any]): YZ;

export function second<AAC>(pair: [any, AAC]): AAC;

export function swap<AAD, AAE>(pair: [AAD, AAE]): [AAE, AAD];

export function map_first<AAF, AAG, AAH>(
  pair: [AAF, AAG],
  fun: (x0: AAF) => AAH
): [AAH, AAG];

export function map_second<AAI, AAJ, AAK>(
  pair: [AAI, AAJ],
  fun: (x0: AAJ) => AAK
): [AAI, AAK];

export function new$<AAL, AAM>(first: AAL, second: AAM): [AAL, AAM];
