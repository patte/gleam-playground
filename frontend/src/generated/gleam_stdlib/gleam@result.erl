-module(gleam@result).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([is_ok/1, is_error/1, map/2, map_error/2, flatten/1, 'try'/2, then/2, unwrap/2, lazy_unwrap/2, unwrap_error/2, unwrap_both/1, nil_error/1, 'or'/2, lazy_or/2, all/1, partition/1, replace/2, replace_error/2, values/1, try_recover/2]).

-spec is_ok({ok, any()} | {error, any()}) -> boolean().
is_ok(Result) ->
    case Result of
        {error, _} ->
            false;

        {ok, _} ->
            true
    end.

-spec is_error({ok, any()} | {error, any()}) -> boolean().
is_error(Result) ->
    case Result of
        {ok, _} ->
            false;

        {error, _} ->
            true
    end.

-spec map({ok, BNM} | {error, BNN}, fun((BNM) -> BNQ)) -> {ok, BNQ} |
    {error, BNN}.
map(Result, Fun) ->
    case Result of
        {ok, X} ->
            {ok, Fun(X)};

        {error, E} ->
            {error, E}
    end.

-spec map_error({ok, BNT} | {error, BNU}, fun((BNU) -> BNX)) -> {ok, BNT} |
    {error, BNX}.
map_error(Result, Fun) ->
    case Result of
        {ok, X} ->
            {ok, X};

        {error, Error} ->
            {error, Fun(Error)}
    end.

-spec flatten({ok, {ok, BOA} | {error, BOB}} | {error, BOB}) -> {ok, BOA} |
    {error, BOB}.
flatten(Result) ->
    case Result of
        {ok, X} ->
            X;

        {error, Error} ->
            {error, Error}
    end.

-spec 'try'({ok, BOI} | {error, BOJ}, fun((BOI) -> {ok, BOM} | {error, BOJ})) -> {ok,
        BOM} |
    {error, BOJ}.
'try'(Result, Fun) ->
    case Result of
        {ok, X} ->
            Fun(X);

        {error, E} ->
            {error, E}
    end.

-spec then({ok, BOR} | {error, BOS}, fun((BOR) -> {ok, BOV} | {error, BOS})) -> {ok,
        BOV} |
    {error, BOS}.
then(Result, Fun) ->
    'try'(Result, Fun).

-spec unwrap({ok, BPA} | {error, any()}, BPA) -> BPA.
unwrap(Result, Default) ->
    case Result of
        {ok, V} ->
            V;

        {error, _} ->
            Default
    end.

-spec lazy_unwrap({ok, BPE} | {error, any()}, fun(() -> BPE)) -> BPE.
lazy_unwrap(Result, Default) ->
    case Result of
        {ok, V} ->
            V;

        {error, _} ->
            Default()
    end.

-spec unwrap_error({ok, any()} | {error, BPJ}, BPJ) -> BPJ.
unwrap_error(Result, Default) ->
    case Result of
        {ok, _} ->
            Default;

        {error, E} ->
            E
    end.

-spec unwrap_both({ok, BPM} | {error, BPM}) -> BPM.
unwrap_both(Result) ->
    case Result of
        {ok, A} ->
            A;

        {error, A@1} ->
            A@1
    end.

-spec nil_error({ok, BPP} | {error, any()}) -> {ok, BPP} | {error, nil}.
nil_error(Result) ->
    map_error(Result, fun(_) -> nil end).

-spec 'or'({ok, BPV} | {error, BPW}, {ok, BPV} | {error, BPW}) -> {ok, BPV} |
    {error, BPW}.
'or'(First, Second) ->
    case First of
        {ok, _} ->
            First;

        {error, _} ->
            Second
    end.

-spec lazy_or({ok, BQD} | {error, BQE}, fun(() -> {ok, BQD} | {error, BQE})) -> {ok,
        BQD} |
    {error, BQE}.
lazy_or(First, Second) ->
    case First of
        {ok, _} ->
            First;

        {error, _} ->
            Second()
    end.

-spec all(list({ok, BQL} | {error, BQM})) -> {ok, list(BQL)} | {error, BQM}.
all(Results) ->
    gleam@list:try_map(Results, fun(X) -> X end).

-spec do_partition(list({ok, BRA} | {error, BRB}), list(BRA), list(BRB)) -> {list(BRA),
    list(BRB)}.
do_partition(Results, Oks, Errors) ->
    case Results of
        [] ->
            {Oks, Errors};

        [{ok, A} | Rest] ->
            do_partition(Rest, [A | Oks], Errors);

        [{error, E} | Rest@1] ->
            do_partition(Rest@1, Oks, [E | Errors])
    end.

-spec partition(list({ok, BQT} | {error, BQU})) -> {list(BQT), list(BQU)}.
partition(Results) ->
    do_partition(Results, [], []).

-spec replace({ok, any()} | {error, BRJ}, BRM) -> {ok, BRM} | {error, BRJ}.
replace(Result, Value) ->
    case Result of
        {ok, _} ->
            {ok, Value};

        {error, Error} ->
            {error, Error}
    end.

-spec replace_error({ok, BRP} | {error, any()}, BRT) -> {ok, BRP} | {error, BRT}.
replace_error(Result, Error) ->
    case Result of
        {ok, X} ->
            {ok, X};

        {error, _} ->
            {error, Error}
    end.

-spec values(list({ok, BRW} | {error, any()})) -> list(BRW).
values(Results) ->
    gleam@list:filter_map(Results, fun(R) -> R end).

-spec try_recover(
    {ok, BSC} | {error, BSD},
    fun((BSD) -> {ok, BSC} | {error, BSG})
) -> {ok, BSC} | {error, BSG}.
try_recover(Result, Fun) ->
    case Result of
        {ok, Value} ->
            {ok, Value};

        {error, Error} ->
            Fun(Error)
    end.
