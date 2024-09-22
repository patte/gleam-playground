-module(gleam@dict).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([size/1, to_list/1, new/0, is_empty/1, get/2, has_key/2, insert/3, from_list/1, keys/1, values/1, take/2, merge/2, delete/2, drop/2, upsert/3, fold/3, map_values/2, filter/2, each/2, combine/3]).
-export_type([dict/2]).

-type dict(KL, KM) :: any() | {gleam_phantom, KL, KM}.

-spec size(dict(any(), any())) -> integer().
size(Dict) ->
    maps:size(Dict).

-spec to_list(dict(KV, KW)) -> list({KV, KW}).
to_list(Dict) ->
    maps:to_list(Dict).

-spec new() -> dict(any(), any()).
new() ->
    maps:new().

-spec is_empty(dict(any(), any())) -> boolean().
is_empty(Dict) ->
    Dict =:= new().

-spec get(dict(MC, MD), MC) -> {ok, MD} | {error, nil}.
get(From, Get) ->
    gleam_stdlib:map_get(From, Get).

-spec has_key(dict(LM, any()), LM) -> boolean().
has_key(Dict, Key) ->
    maps:is_key(Key, Dict).

-spec insert(dict(MO, MP), MO, MP) -> dict(MO, MP).
insert(Dict, Key, Value) ->
    maps:put(Key, Value, Dict).

-spec fold_list_of_pair(list({LF, LG}), dict(LF, LG)) -> dict(LF, LG).
fold_list_of_pair(List, Initial) ->
    case List of
        [] ->
            Initial;

        [X | Rest] ->
            fold_list_of_pair(
                Rest,
                insert(Initial, erlang:element(1, X), erlang:element(2, X))
            )
    end.

-spec from_list(list({LA, LB})) -> dict(LA, LB).
from_list(List) ->
    maps:from_list(List).

-spec reverse_and_concat(list(UE), list(UE)) -> list(UE).
reverse_and_concat(Remaining, Accumulator) ->
    case Remaining of
        [] ->
            Accumulator;

        [Item | Rest] ->
            reverse_and_concat(Rest, [Item | Accumulator])
    end.

-spec do_keys_acc(list({OB, any()}), list(OB)) -> list(OB).
do_keys_acc(List, Acc) ->
    case List of
        [] ->
            reverse_and_concat(Acc, []);

        [X | Xs] ->
            do_keys_acc(Xs, [erlang:element(1, X) | Acc])
    end.

-spec keys(dict(NO, any())) -> list(NO).
keys(Dict) ->
    maps:keys(Dict).

-spec do_values_acc(list({any(), OR}), list(OR)) -> list(OR).
do_values_acc(List, Acc) ->
    case List of
        [] ->
            reverse_and_concat(Acc, []);

        [X | Xs] ->
            do_values_acc(Xs, [erlang:element(2, X) | Acc])
    end.

-spec values(dict(any(), OH)) -> list(OH).
values(Dict) ->
    maps:values(Dict).

-spec insert_taken(dict(PV, PW), list(PV), dict(PV, PW)) -> dict(PV, PW).
insert_taken(Dict, Desired_keys, Acc) ->
    Insert = fun(Taken, Key) -> case get(Dict, Key) of
            {ok, Value} ->
                insert(Taken, Key, Value);

            _ ->
                Taken
        end end,
    case Desired_keys of
        [] ->
            Acc;

        [X | Xs] ->
            insert_taken(Dict, Xs, Insert(Acc, X))
    end.

-spec take(dict(PH, PI), list(PH)) -> dict(PH, PI).
take(Dict, Desired_keys) ->
    maps:with(Desired_keys, Dict).

-spec insert_pair(dict(QU, QV), {QU, QV}) -> dict(QU, QV).
insert_pair(Dict, Pair) ->
    insert(Dict, erlang:element(1, Pair), erlang:element(2, Pair)).

-spec fold_inserts(list({RA, RB}), dict(RA, RB)) -> dict(RA, RB).
fold_inserts(New_entries, Dict) ->
    case New_entries of
        [] ->
            Dict;

        [X | Xs] ->
            fold_inserts(Xs, insert_pair(Dict, X))
    end.

-spec merge(dict(QE, QF), dict(QE, QF)) -> dict(QE, QF).
merge(Dict, New_entries) ->
    maps:merge(Dict, New_entries).

-spec delete(dict(RH, RI), RH) -> dict(RH, RI).
delete(Dict, Key) ->
    maps:remove(Key, Dict).

-spec drop(dict(RT, RU), list(RT)) -> dict(RT, RU).
drop(Dict, Disallowed_keys) ->
    case Disallowed_keys of
        [] ->
            Dict;

        [X | Xs] ->
            drop(delete(Dict, X), Xs)
    end.

-spec upsert(dict(SA, SB), SA, fun((gleam@option:option(SB)) -> SB)) -> dict(SA, SB).
upsert(Dict, Key, Fun) ->
    _pipe = Dict,
    _pipe@1 = get(_pipe, Key),
    _pipe@2 = gleam@option:from_result(_pipe@1),
    _pipe@3 = Fun(_pipe@2),
    insert(Dict, Key, _pipe@3).

-spec do_fold(list({SH, SI}), SK, fun((SK, SH, SI) -> SK)) -> SK.
do_fold(List, Initial, Fun) ->
    case List of
        [] ->
            Initial;

        [{K, V} | Rest] ->
            do_fold(Rest, Fun(Initial, K, V), Fun)
    end.

-spec fold(dict(SL, SM), SP, fun((SP, SL, SM) -> SP)) -> SP.
fold(Dict, Initial, Fun) ->
    _pipe = Dict,
    _pipe@1 = maps:to_list(_pipe),
    do_fold(_pipe@1, Initial, Fun).

-spec map_values(dict(NA, NB), fun((NA, NB) -> NE)) -> dict(NA, NE).
map_values(Dict, Fun) ->
    maps:map(Fun, Dict).

-spec filter(dict(OV, OW), fun((OV, OW) -> boolean())) -> dict(OV, OW).
filter(Dict, Predicate) ->
    maps:filter(Predicate, Dict).

-spec each(dict(SQ, SR), fun((SQ, SR) -> any())) -> nil.
each(Dict, Fun) ->
    fold(
        Dict,
        nil,
        fun(Nil, K, V) ->
            Fun(K, V),
            Nil
        end
    ).

-spec combine(dict(SV, SW), dict(SV, SW), fun((SW, SW) -> SW)) -> dict(SV, SW).
combine(Dict, Other, Fun) ->
    fold(Dict, Other, fun(Acc, Key, Value) -> case get(Acc, Key) of
                {ok, Other_value} ->
                    insert(Acc, Key, Fun(Value, Other_value));

                {error, _} ->
                    insert(Acc, Key, Value)
            end end).
