-module(cauterize_schema_test).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

roundtrip_test() ->
    I0 = [{somearray, [1, 2, 3, 4, 5, 6, 7, 8]}],
    {ok, O0} = erlang_test:encode(I0),
    ?assertEqual({ok, I0}, erlang_test:decode(O0, somearray)),
    I1 = [{somevector, [1, 2, 3, 4, 5]}],
    {ok, O1} = erlang_test:encode(I1),
    ?assertEqual({ok, I1}, erlang_test:decode(O1, somevector)),
    I2 = [{arecord, [{z, [10, 11, 12, 13]}, {a, -1}, {d, [{a, 1}, {d, [{a, 0}, {b, 1}]}]}]}],
    {ok, O2} = erlang_test:encode(I2),
    ?assertEqual({ok, I2}, erlang_test:decode(O2, arecord)),
    I3 = [{a_union, {c, 99}}],
    {ok, O3} = erlang_test:encode(I3),
    ?assertEqual({ok, I3}, erlang_test:decode(O3, a_union)),
    I4 = [{a_union, e}],
    {ok, O4} = erlang_test:encode(I4),
    ?assertEqual({ok, I4}, erlang_test:decode(O4, a_union)),
    I5 = [{a_combination, [{a, 223372036854775808}, {b, -99}, d]}],
    {ok, O5} = erlang_test:encode(I5),
    ?assertEqual({ok, I5}, erlang_test:decode(O5, a_combination)),


    ok.
