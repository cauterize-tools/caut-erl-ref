-module(primitive_test).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

primitive_test_() ->
    [
       {"u8", fun() ->
         I = [{primitivetest, [{u8, 255}]}],
         {ok, O} = erlang_test:encode(I),
         ?assertEqual({ok, I}, erlang_test:decode(list_to_binary(O), primitivetest))
        end},
       {"u16", fun() ->
         I = [{primitivetest, [{u16, 65535}]}],
         {ok, O} = erlang_test:encode(I),
         ?assertEqual({ok, I}, erlang_test:decode(list_to_binary(O), primitivetest))
        end},
       {"u32", fun() ->
         I = [{primitivetest, [{u32, 4294967295}]}],
         {ok, O} = erlang_test:encode(I),
         ?assertEqual({ok, I}, erlang_test:decode(list_to_binary(O), primitivetest))
        end},
       {"u64", fun() ->
         I = [{primitivetest, [{u64, 18446744073709551615}]}],
         {ok, O} = erlang_test:encode(I),
         ?assertEqual({ok, I}, erlang_test:decode(list_to_binary(O), primitivetest))
        end},
       {"s16", fun() ->
         I = [{primitivetest, [{s16, 32767}]}],
         {ok, O} = erlang_test:encode(I),
         ?assertEqual({ok, I}, erlang_test:decode(list_to_binary(O), primitivetest))
        end},
       {"s32", fun() ->
         I = [{primitivetest, [{s32, 2147483647}]}],
         {ok, O} = erlang_test:encode(I),
         ?assertEqual({ok, I}, erlang_test:decode(list_to_binary(O), primitivetest))
        end},
       {"true", fun() ->
         I = [{primitivetest, [{bool, true}]}],
         {ok, O} = erlang_test:encode(I),
         ?assertEqual({ok, I}, erlang_test:decode(list_to_binary(O), primitivetest))
        end},
       {"false", fun() ->
         I = [{primitivetest, [{bool, false}]}],
         {ok, O} = erlang_test:encode(I),
         ?assertEqual({ok, I}, erlang_test:decode(list_to_binary(O), primitivetest))
        end},
       {"f32 nan", fun() ->
         I = [{primitivetest, [{f32, nan}]}],
         {ok, O} = erlang_test:encode(I),
         ?assertEqual({ok, I}, erlang_test:decode(list_to_binary(O), primitivetest))
        end},
       {"f32 pos_inf", fun() ->
         I = [{primitivetest, [{f32, pos_inf}]}],
         {ok, O} = erlang_test:encode(I),
         ?assertEqual({ok, I}, erlang_test:decode(list_to_binary(O), primitivetest))
        end},
       {"f32 neg_inf", fun() ->
         I = [{primitivetest, [{f32, neg_inf}]}],
         {ok, O} = erlang_test:encode(I),
         ?assertEqual({ok, I}, erlang_test:decode(list_to_binary(O), primitivetest))
        end},
       {"f32", fun() ->
         I = [{primitivetest, [{f32, 3.1449999809265137}]}],
         {ok, O} = erlang_test:encode(I),
         ?assertEqual({ok, I}, erlang_test:decode(list_to_binary(O), primitivetest))
        end},
       {"f64 nan", fun() ->
         I = [{primitivetest, [{f64, nan}]}],
         {ok, O} = erlang_test:encode(I),
         ?assertEqual({ok, I}, erlang_test:decode(list_to_binary(O), primitivetest))
        end},
       {"f64 pos_inf", fun() ->
         I = [{primitivetest, [{f64, pos_inf}]}],
         {ok, O} = erlang_test:encode(I),
         ?assertEqual({ok, I}, erlang_test:decode(list_to_binary(O), primitivetest))
        end},
       {"f64 neg_inf", fun() ->
         I = [{primitivetest, [{f64, neg_inf}]}],
         {ok, O} = erlang_test:encode(I),
         ?assertEqual({ok, I}, erlang_test:decode(list_to_binary(O), primitivetest))
        end},
       {"f64", fun() ->
         I = [{primitivetest, [{f64, 3.142857142857143}]}],
         {ok, O} = erlang_test:encode(I),
         ?assertEqual({ok, I}, erlang_test:decode(list_to_binary(O), primitivetest))
        end}].

oversize_primitive_test_() ->
    [
       {"u8", fun() ->
         I = [{primitivetest, [{u8, 256}]}],
         ?assertEqual({error, {integer_too_large, u8, 256}}, erlang_test:encode(I))
        end},
       {"u16", fun() ->
         I = [{primitivetest, [{u16, 65536}]}],
         ?assertEqual({error, {integer_too_large, u16, 65536}}, erlang_test:encode(I))
        end},
       {"u32", fun() ->
         I = [{primitivetest, [{u32, 4294967296}]}],
         ?assertEqual({error, {integer_too_large, u32, 4294967296}}, erlang_test:encode(I))
        end},
       {"u64", fun() ->
         I = [{primitivetest, [{u64, 18446744073709551616}]}],
         ?assertEqual({error, {integer_too_large, u64, 18446744073709551616}}, erlang_test:encode(I))
        end},
       {"s8", fun() ->
         I = [{primitivetest, [{s8, 128}]}],
         ?assertEqual({error, {integer_too_large, s8, 128}}, erlang_test:encode(I)),
         I2 = [{primitivetest, [{s8, -128}]}],
         {ok, O} = erlang_test:encode(I2),
         ?assertEqual({ok, I2}, erlang_test:decode(list_to_binary(O), primitivetest)),
         I3 = [{primitivetest, [{s8, -129}]}],
         ?assertEqual({error, {integer_too_large, s8, -129}}, erlang_test:encode(I3))
        end},
       {"s16", fun() ->
         I = [{primitivetest, [{s16, 32768}]}],
         ?assertEqual({error, {integer_too_large, s16, 32768}}, erlang_test:encode(I)),
         I2 = [{primitivetest, [{s16, -32768}]}],
         {ok, O} = erlang_test:encode(I2),
         ?assertEqual({ok, I2}, erlang_test:decode(list_to_binary(O), primitivetest)),
         I3 = [{primitivetest, [{s16, -32769}]}],
         ?assertEqual({error, {integer_too_large, s16, -32769}}, erlang_test:encode(I3))
        end},
       {"u32", fun() ->
         I = [{primitivetest, [{s32, 2147483648}]}],
         ?assertEqual({error, {integer_too_large, s32, 2147483648}}, erlang_test:encode(I)),
         I2 = [{primitivetest, [{s32, -2147483648}]}],
         {ok, O} = erlang_test:encode(I2),
         ?assertEqual({ok, I2}, erlang_test:decode(list_to_binary(O), primitivetest)),
         I3 = [{primitivetest, [{s32, -2147483649}]}],
         ?assertEqual({error, {integer_too_large, s32, -2147483649}}, erlang_test:encode(I3))
        end},
       {"u64", fun() ->
         I = [{primitivetest, [{s64, 9223372036854775808}]}],
         ?assertEqual({error, {integer_too_large, s64, 9223372036854775808}}, erlang_test:encode(I)),
         I2 = [{primitivetest, [{s64, -9223372036854775808}]}],
         {ok, O} = erlang_test:encode(I2),
         ?assertEqual({ok, I2}, erlang_test:decode(list_to_binary(O), primitivetest)),
         I3 = [{primitivetest, [{s64, -9223372036854775809}]}],
         ?assertEqual({error, {integer_too_large, s64, -9223372036854775809}}, erlang_test:encode(I3))
        end}
    ].
