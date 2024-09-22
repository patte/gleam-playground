-module(gleam@function).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([compose/2, curry2/1, curry3/1, curry4/1, curry5/1, curry6/1, flip/1, identity/1, constant/1, tap/2, apply1/2, apply2/3, apply3/4]).

-spec compose(fun((EUU) -> EUV), fun((EUV) -> EUW)) -> fun((EUU) -> EUW).
compose(Fun1, Fun2) ->
    fun(A) -> Fun2(Fun1(A)) end.

-spec curry2(fun((EUX, EUY) -> EUZ)) -> fun((EUX) -> fun((EUY) -> EUZ)).
curry2(Fun) ->
    fun(A) -> fun(B) -> Fun(A, B) end end.

-spec curry3(fun((EVB, EVC, EVD) -> EVE)) -> fun((EVB) -> fun((EVC) -> fun((EVD) -> EVE))).
curry3(Fun) ->
    fun(A) -> fun(B) -> fun(C) -> Fun(A, B, C) end end end.

-spec curry4(fun((EVG, EVH, EVI, EVJ) -> EVK)) -> fun((EVG) -> fun((EVH) -> fun((EVI) -> fun((EVJ) -> EVK)))).
curry4(Fun) ->
    fun(A) -> fun(B) -> fun(C) -> fun(D) -> Fun(A, B, C, D) end end end end.

-spec curry5(fun((EVM, EVN, EVO, EVP, EVQ) -> EVR)) -> fun((EVM) -> fun((EVN) -> fun((EVO) -> fun((EVP) -> fun((EVQ) -> EVR))))).
curry5(Fun) ->
    fun(A) ->
        fun(B) ->
            fun(C) -> fun(D) -> fun(E) -> Fun(A, B, C, D, E) end end end
        end
    end.

-spec curry6(fun((EVT, EVU, EVV, EVW, EVX, EVY) -> EVZ)) -> fun((EVT) -> fun((EVU) -> fun((EVV) -> fun((EVW) -> fun((EVX) -> fun((EVY) -> EVZ)))))).
curry6(Fun) ->
    fun(A) ->
        fun(B) ->
            fun(C) ->
                fun(D) -> fun(E) -> fun(F) -> Fun(A, B, C, D, E, F) end end end
            end
        end
    end.

-spec flip(fun((EWB, EWC) -> EWD)) -> fun((EWC, EWB) -> EWD).
flip(Fun) ->
    fun(B, A) -> Fun(A, B) end.

-spec identity(EWE) -> EWE.
identity(X) ->
    X.

-spec constant(EWF) -> fun((any()) -> EWF).
constant(Value) ->
    fun(_) -> Value end.

-spec tap(EWH, fun((EWH) -> any())) -> EWH.
tap(Arg, Effect) ->
    Effect(Arg),
    Arg.

-spec apply1(fun((EWJ) -> EWK), EWJ) -> EWK.
apply1(Fun, Arg1) ->
    Fun(Arg1).

-spec apply2(fun((EWL, EWM) -> EWN), EWL, EWM) -> EWN.
apply2(Fun, Arg1, Arg2) ->
    Fun(Arg1, Arg2).

-spec apply3(fun((EWO, EWP, EWQ) -> EWR), EWO, EWP, EWQ) -> EWR.
apply3(Fun, Arg1, Arg2, Arg3) ->
    Fun(Arg1, Arg2, Arg3).
