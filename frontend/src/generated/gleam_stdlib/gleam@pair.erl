-module(gleam@pair).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([first/1, second/1, swap/1, map_first/2, map_second/2, new/2]).

-spec first({YZ, any()}) -> YZ.
first(Pair) ->
    {A, _} = Pair,
    A.

-spec second({any(), AAC}) -> AAC.
second(Pair) ->
    {_, A} = Pair,
    A.

-spec swap({AAD, AAE}) -> {AAE, AAD}.
swap(Pair) ->
    {A, B} = Pair,
    {B, A}.

-spec map_first({AAF, AAG}, fun((AAF) -> AAH)) -> {AAH, AAG}.
map_first(Pair, Fun) ->
    {A, B} = Pair,
    {Fun(A), B}.

-spec map_second({AAI, AAJ}, fun((AAJ) -> AAK)) -> {AAI, AAK}.
map_second(Pair, Fun) ->
    {A, B} = Pair,
    {A, Fun(B)}.

-spec new(AAL, AAM) -> {AAL, AAM}.
new(First, Second) ->
    {First, Second}.
