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
    I3 = [{a_union, [{c, 99}]}],
    {ok, O3} = erlang_test:encode(I3),
    ?assertEqual({ok, I3}, erlang_test:decode(list_to_binary(O3), a_union)),
    %% non-KVC form
    I3a = [{a_union, {c, 99}}],
    {ok, O3a} = erlang_test:encode(I3a),
    %% decodes to KVC form
    ?assertEqual({ok, I3}, erlang_test:decode(list_to_binary(O3a), a_union)),
    I4 = [{a_union, [{e, true}]}],
    {ok, O4} = erlang_test:encode(I4),
    ?assertEqual({ok, I4}, erlang_test:decode(list_to_binary(O4), a_union)),
    %% non-KVC form
    I4a = [{a_union, e}],
    {ok, O4a} = erlang_test:encode(I4a),
    %% decodes to KVC form
    ?assertEqual({ok, I4}, erlang_test:decode(list_to_binary(O4a), a_union)),
    %% non-KVC form
    I4b = [{a_union, {e, true}}],
    {ok, O4b} = erlang_test:encode(I4b),
    %% decodes to KVC form
    ?assertEqual({ok, I4}, erlang_test:decode(list_to_binary(O4b), a_union)),
    I5 = [{a_combination, [{a, 223372036854775808}, {b, -99}, d]}],
    {ok, O5} = erlang_test:encode(I5),
    ?assertEqual({ok, I5}, erlang_test:decode(list_to_binary(O5), a_combination)),
    I5a = [{a_combination, [{a, 223372036854775808}, {b, -99}]}],
    {ok, O5a} = erlang_test:encode(I5a),
    ?assertEqual({ok, I5a}, erlang_test:decode(list_to_binary(O5a), a_combination)),
    I5b = [{a_combination, [{a, 223372036854775808}, {b, -99}, {c, [{a,[{z, [10, 11, 12, 13]}, {a, -1}, {d, [{a, 1}, {d, [{a, 0}, {b, 1}]}]}]}] }]}],
    {ok, O5b} = erlang_test:encode(I5b),
    ?assertEqual({ok, I5b}, erlang_test:decode(list_to_binary(O5b), a_combination)),
    I6 = [{someenum, green}],
    {ok, O6} = erlang_test:encode(I6),
    ?assertEqual({ok, I6}, erlang_test:decode(list_to_binary(O6), someenum)),
    I7 = [{some_range, 1005}],
    {ok, O7} = erlang_test:encode(I7),
    ?assertEqual({ok, I7}, erlang_test:decode(list_to_binary(O7), some_range)),
    I8 = [{somearray, [1, 2, 3, 4, 5, 6, 7]}],
    ?assertEqual({error, {incorrect_array_size, somearray, 7, 8}}, erlang_test:encode(I8)),
    ok.

truncate(B, I) ->
    binary:part(B, {0, byte_size(B) - I}).

decode_test_() ->
    [
     {"somearray truncation", fun() ->
        ?assertMatch({error, {no_input, []}}, erlang_test:decode(<<>> , somearray)),
        ok
    end},
     {"somearray truncation", fun() ->
        I0 = [{somearray, [1, 2, 3, 4, 5, 6, 7, 8]}],
        {ok, O0} = erlang_test:encode(I0),
        C = list_to_binary(O0),
        ?assertMatch({error, {{unexpected_end_of_input, _, _, _}, [{somearray, [1, 2, 3, 4, 5, 6, 7]}]}}, erlang_test:decode(truncate(C, 8), somearray)),
        ?assertMatch({error, {{unexpected_end_of_input, _, _, _}, [{somearray, [1, 2, 3, 4, 5, 6]}]}}, erlang_test:decode(truncate(C, 9), somearray)),
        ok
    end},
     {"range truncation", fun() ->
        I0 = [{some_range, 1005}],
        {ok, O0} = erlang_test:encode(I0),
        C = list_to_binary(O0),
        ?assertMatch({error, {{unexpected_end_of_input, _, _, _}, [{some_range, []}]}}, erlang_test:decode(truncate(C, 1), some_range)),
        ok
    end},
     {"invalid range value", fun() ->
        C = <<2011:16/integer-unsigned-little>>,
        ?assertMatch({error, {{invalid_range_value, _, _, _}, [{some_range, []}]}}, erlang_test:decode(C, some_range)),
        ok
    end},
     {"somevector truncation", fun() ->
        I0 = [{somevector, [1, 2, 3, 4, 5]}],
        {ok, O0} = erlang_test:encode(I0),
        C = list_to_binary(O0),
        ?assertMatch({error, {{unexpected_end_of_input, _, _, _}, [{somevector, [1, 2, 3, 4]}]}}, erlang_test:decode(truncate(C, 7), somevector)),
        ?assertMatch({error, {{unexpected_end_of_input, _, _, _}, [{somevector, [1, 2, 3, 4]}]}}, erlang_test:decode(truncate(C, 8), somevector)),
        ok
    end},
     {"oversize vector", fun() ->
        I0 = [{somevector, [1, 2, 3, 4, 5]}],
        {ok, O0} = erlang_test:encode(I0),
        <<5:8/integer, C0/binary>> = list_to_binary(O0),
        C = <<10:8/integer, C0/binary, 6:64/integer-signed-little, 7:64/integer-signed-little, 8:64/integer-signed-little, 9:64/integer-signed-little, 10:64/integer-signed-little>>,
        ?assertMatch({error, {{oversize_vector, somevector, 10, 8}, [{somevector, []}]}}, erlang_test:decode(C, somevector)),
        ok
    end},
    {"someenum invalid value", fun() ->
        ?assertMatch({error, {{out_of_range_enumeration_value, someenum, 9}, [{someenum, [] }]}}, erlang_test:decode(<<9>>, someenum)),
        ok
    end},
    {"union truncation", fun() ->
        I0= [{a_union, [{d, 223372036854775808}]}],
        {ok, O0} = erlang_test:encode(I0),
        C = list_to_binary(O0),
        ?assertMatch({error, {{unexpected_end_of_input, _, _, _}, [{a_union, [{d,'?'}] }]}}, erlang_test:decode(truncate(C, 1), a_union)),
        ?assertMatch({error, {{unexpected_end_of_input, _, _, _}, [{a_union, [{d, '?'}] }]}}, erlang_test:decode(truncate(C, 8), a_union)),
        ok
    end},
    {"union bad index", fun() ->
        I0= [{a_union, [{d, 223372036854775808}]}],
        {ok, O0} = erlang_test:encode(I0),
        <<3:8/integer, C0/binary>> = list_to_binary(O0),
        C = <<10:8/integer, C0/binary>>,
        ?assertMatch({error, {{bad_union_index, _, _}, [{a_union, []}]}}, erlang_test:decode(truncate(C, 1), a_union)),
        ok
    end},
    {"record truncation", fun() ->
        I0 = [{arecord, [{z, [10, 11, 12, 13]}, {a, 1}, {d, [{a, 2}, {d, [{a, 3}, {b, 1}]}]}]}],
        {ok, O0} = erlang_test:encode(I0),
        C = list_to_binary(O0),
        ?assertMatch({error, {{unexpected_end_of_input, _, _, _}, [{arecord, [{z, [10, 11, 12, 13]}, {a, 1}, {d, [{a, 2}, {d, [{a, 3}, {b, '?'}]}]}]}]}}, erlang_test:decode(truncate(C, 1), arecord)),
        ?assertMatch({error, {{unexpected_end_of_input, _, _, _}, [{arecord, [{z, [10, 11, 12, 13]}, {a, 1}, {d, [{a, 2}, {d, '?'}]}]}]}}, erlang_test:decode(truncate(C, 2), arecord)),
        ?assertMatch({error, {{unexpected_end_of_input, _, _, _}, [{arecord, [{z, [10, 11, 12, 13]}, {a, 1}, {d, '?'}]}]}}, erlang_test:decode(truncate(C, 3), arecord)),
        ok
    end},
    {"oversize vector in record", fun() ->
        I0 = [{arecord, [{z, [10, 11, 12, 13]}, {a, 1}, {d, [{a, 2}, {d, [{a, 3}, {b, 1}]}]}]}],
        {ok, O0} = erlang_test:encode(I0),
        <<4:8/integer, C0/binary>> = list_to_binary(O0),
        C = <<10:8/integer, C0/binary>>,
        ?assertMatch({error, {{oversize_vector, _, _, _}, [{arecord, [{z, '?'}]}]}}, erlang_test:decode(C, arecord)),
        ok
    end},
    {"combination truncation", fun() ->
        I0 = [{a_combination, [{a, 223372036854775808}, {b, -99}, d]}],
        {ok, O0} = erlang_test:encode(I0),
        C = list_to_binary(O0),
        ?assertMatch({error, {{unexpected_end_of_input, _, _, _}, [{a_combination, [{a, 223372036854775808}, {b, '?'}]}]}}, erlang_test:decode(truncate(C, 1), a_combination)),
        ?assertMatch({error, {{unexpected_end_of_input, _, _, _}, [{a_combination, [{a, '?'}]}]}}, erlang_test:decode(truncate(C, 2), a_combination)),
        ok
    end},
    {"union unknown field", fun() ->
        I0= [{a_union, [{f, 1}]}],
        ?assertMatch({error, {unknown_union_member, a_union, f}}, erlang_test:encode(I0)),
        ok
    end},
    {"union unknown field", fun() ->
        I0= [{a_union, [{e, 1}]}],
        ?assertMatch({error, {data_supplied_for_empty_union_member, a_union, e}}, erlang_test:encode(I0)),
        ok
    end}
    ].

encode_test_() ->
    [
     {"oversize vector", fun() ->
        I0 = [{somevector, lists:seq(1, 20)}],
        ?assertMatch({error, {oversize_vector, somevector, 20, 8}}, erlang_test:encode(I0)),
        ok
    end},
     {"out-of-range range value", fun() ->
        I1 = [{some_range, 2011}],
        ?assertMatch({error, {invalid_range_value, some_range, 2011}}, erlang_test:encode(I1)),
        ok
    end},
     {"invalid enum value", fun() ->
        I1 = [{someenum, purple}],
        ?assertMatch({error, {unknown_enumeration_field, someenum, purple}}, erlang_test:encode(I1)),
        ok
    end},
     {"invalid record field", fun() ->
        I0 = [{arecord, [{z, [10, 11, 12, 13]}, {d, [{a, 2}, {d, [{a, 3}, {b, 1}]}]}]}],
        ?assertMatch({error, {missing_record_field, arecord, a}}, erlang_test:encode(I0)),
        ok
    end}
    ].


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

char_vector_test() ->
    I0 = [{charvector, [$h, $e, $l, $l, $o]}],
    {ok, O0} = cauterize:encode(I0, [{descriptor, vector, charvector, { u8, 10, tag8 }}]),
    I1 = [{charvector, <<"hello">>}],
    ?assertEqual({ok, O0}, cauterize:encode(I1, [{descriptor, vector, charvector, { char, 10, tag8}}])),
    ?assertEqual({ok, I0}, cauterize:decode(list_to_binary(O0), charvector, [{descriptor, vector, charvector, { u8, 10, tag8 }}])),
    ?assertEqual({ok, [{charvector, <<"hello">>}]}, cauterize:decode(list_to_binary(O0), charvector, [{descriptor, vector, charvector, { char, 10, tag8 }}])),
    ok.


char_array_test() ->
    I0 = [{chararray, [$h, $e, $l, $l, $o, $w, $o, $r, $l, $d]}],
    {ok, O0} = cauterize:encode(I0, [{descriptor, array, chararray, { u8, 10 }}]),
    ?assertEqual({ok, O0}, cauterize:encode(I0, [{descriptor, array, chararray, { char, 10 }}])),
    ?assertEqual({ok, I0}, cauterize:decode(list_to_binary(O0), chararray, [{descriptor, array, chararray, { u8, 10 }}])),
    ?assertEqual({ok, [{chararray, <<"helloworld">>}]}, cauterize:decode(list_to_binary(O0), chararray, [{descriptor, array, chararray, { char, 10 }}])),
    ok.

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

