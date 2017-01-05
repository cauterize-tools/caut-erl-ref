-module(cauterize_schema_test).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

roundtrip_test() ->
    I0 = [{somearray, [1, 2, 3, 4, 5, 6, 7, 8]}],
    {ok, O0} = erlang_test:encode(I0),
    ?assertEqual({ok, I0}, erlang_test:decode(list_to_binary(O0), somearray)),
    I1 = [{somevector, [1, 2, 3, 4, 5]}],
    {ok, O1} = erlang_test:encode(I1),
    ?assertEqual({ok, I1}, erlang_test:decode(list_to_binary(O1), somevector)),
    I2 = [{arecord, [{z, [10, 11, 12, 13]}, {a, -1}, {d, [{a, 1}, {d, [{a, 0}, {b, 1}]}]}]}],
    {ok, O2} = erlang_test:encode(I2),
    ?assertEqual({ok, I2}, erlang_test:decode(list_to_binary(O2), arecord)),
    I3 = [{a_union, {c, 99}}],
    {ok, O3} = erlang_test:encode(I3),
    ?assertEqual({ok, I3}, erlang_test:decode(list_to_binary(O3), a_union)),
    I4 = [{a_union, e}],
    {ok, O4} = erlang_test:encode(I4),
    ?assertEqual({ok, I4}, erlang_test:decode(list_to_binary(O4), a_union)),
    I5 = [{a_combination, [{a, 223372036854775808}, {b, -99}, d]}],
    {ok, O5} = erlang_test:encode(I5),
    ?assertEqual({ok, I5}, erlang_test:decode(list_to_binary(O5), a_combination)),
    I6 = [{someenum, green}],
    {ok, O6} = erlang_test:encode(I6),
    ?assertEqual({ok, I6}, erlang_test:decode(list_to_binary(O6), someenum)),
    I7 = [{some_range, 1005}],
    {ok, O7} = erlang_test:encode(I7),
    ?assertEqual({ok, I7}, erlang_test:decode(list_to_binary(O7), some_range)),
    I8 = [{arecord, [{z, [10, 11, 12, 13]}, {a, -1}, {d, [{a, 1}, {d, [{a, 0}, {b, 1}]}]}]}],
    {ok, O8} = erlang_test:encode(I8),
    ?assertEqual({ok, I8}, erlang_test:decode(binary:part(list_to_binary(O8), {0, 32}), arecord)),
    ok.

primitive_test_() ->
    [
       {"u8", fun() ->
         I = [{primitivetest, {u8, 255}}],
         {ok, O} = erlang_test:encode(I),
         ?assertEqual({ok, I}, erlang_test:decode(list_to_binary(O), primitivetest))
        end},
       {"u16", fun() ->
         I = [{primitivetest, {u16, 65535}}],
         {ok, O} = erlang_test:encode(I),
         ?assertEqual({ok, I}, erlang_test:decode(list_to_binary(O), primitivetest))
        end},
       {"u32", fun() ->
         I = [{primitivetest, {u32, 4294967295}}],
         {ok, O} = erlang_test:encode(I),
         ?assertEqual({ok, I}, erlang_test:decode(list_to_binary(O), primitivetest))
        end},
       {"u64", fun() ->
         I = [{primitivetest, {u64, 18446744073709551615}}],
         {ok, O} = erlang_test:encode(I),
         ?assertEqual({ok, I}, erlang_test:decode(list_to_binary(O), primitivetest))
        end},
       {"s16", fun() ->
         I = [{primitivetest, {s16, 32767}}],
         {ok, O} = erlang_test:encode(I),
         ?assertEqual({ok, I}, erlang_test:decode(list_to_binary(O), primitivetest))
        end},
       {"s32", fun() ->
         I = [{primitivetest, {s32, 2147483647}}],
         {ok, O} = erlang_test:encode(I),
         ?assertEqual({ok, I}, erlang_test:decode(list_to_binary(O), primitivetest))
        end},
       {"true", fun() ->
         I = [{primitivetest, {bool, true}}],
         {ok, O} = erlang_test:encode(I),
         ?assertEqual({ok, I}, erlang_test:decode(list_to_binary(O), primitivetest))
        end},
       {"false", fun() ->
         I = [{primitivetest, {bool, false}}],
         {ok, O} = erlang_test:encode(I),
         ?assertEqual({ok, I}, erlang_test:decode(list_to_binary(O), primitivetest))
        end},
       {"f32 nan", fun() ->
         I = [{primitivetest, {f32, nan}}],
         {ok, O} = erlang_test:encode(I),
         ?assertEqual({ok, I}, erlang_test:decode(list_to_binary(O), primitivetest))
        end},
       {"f32 pos_inf", fun() ->
         I = [{primitivetest, {f32, pos_inf}}],
         {ok, O} = erlang_test:encode(I),
         ?assertEqual({ok, I}, erlang_test:decode(list_to_binary(O), primitivetest))
        end},
       {"f32 neg_inf", fun() ->
         I = [{primitivetest, {f32, neg_inf}}],
         {ok, O} = erlang_test:encode(I),
         ?assertEqual({ok, I}, erlang_test:decode(list_to_binary(O), primitivetest))
        end},
       {"f32", fun() ->
         I = [{primitivetest, {f32, 3.1449999809265137}}],
         {ok, O} = erlang_test:encode(I),
         ?assertEqual({ok, I}, erlang_test:decode(list_to_binary(O), primitivetest))
        end},
       {"f64 nan", fun() ->
         I = [{primitivetest, {f64, nan}}],
         {ok, O} = erlang_test:encode(I),
         ?assertEqual({ok, I}, erlang_test:decode(list_to_binary(O), primitivetest))
        end},
       {"f64 pos_inf", fun() ->
         I = [{primitivetest, {f64, pos_inf}}],
         {ok, O} = erlang_test:encode(I),
         ?assertEqual({ok, I}, erlang_test:decode(list_to_binary(O), primitivetest))
        end},
       {"f64 neg_inf", fun() ->
         I = [{primitivetest, {f64, neg_inf}}],
         {ok, O} = erlang_test:encode(I),
         ?assertEqual({ok, I}, erlang_test:decode(list_to_binary(O), primitivetest))
        end},
       {"f64", fun() ->
         I = [{primitivetest, {f64, 3.142857142857143}}],
         {ok, O} = erlang_test:encode(I),
         ?assertEqual({ok, I}, erlang_test:decode(list_to_binary(O), primitivetest))
        end}].

coalesce_test() ->
    I0 = [{somearray, [1, 2, 3, 4, 5, 6, 7, 8]}],
    {ok, O0} = erlang_test:encode(I0),
    I1 = [{somevector, [1, 2, 3, 4, 5]}],
    {ok, O1} = erlang_test:encode(I1),
    I2 = [{arecord, [{z, [10, 11, 12, 13]}, {a, -1}, {d, [{a, 1}, {d, [{a, 0}, {b, 1}]}]}]}],
    {ok, O2} = erlang_test:encode(I2),
    Header = [{header, [somearray, somevector, arecord]}],
    {ok, H} = erlang_test:encode(Header),
    Coalesced = list_to_binary([H, O0, O1, O2]),
    {ok, Coalesced2} = erlang_test:encode(Header++I0++I1++I2),
    %% assert manually coalescing types is the same as passing several
    %% types to encode()
    ?assertEqual(Coalesced, list_to_binary(Coalesced2)),
    %% check we can decode just one type and get back a remainder
    {ok, DecodedHeader, Rem} = erlang_test:decode(Coalesced, header),
    ?assertEqual(Header, DecodedHeader),
    [{header, HeaderFields}] = DecodedHeader,
    %% check we can decode several types at once
    {ok, DecodedFields} = erlang_test:decode(Rem, HeaderFields),
    ?assertEqual(I0++I1++I2, DecodedFields),
    ok.

