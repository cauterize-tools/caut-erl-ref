-module(cauterize).
-export([decode/3, encode/2]).

-type cauterize_tag() :: tag8 | tag16 | tag32 | tag64.
-type cauterize_primitive() :: u8 | u16 | u32 | u64 | s8 | s16 | s32 | s64 | f32 | f64 | bool.
-type cauterize_type() :: cauterize_primitive() | 'range' | 'array' | 'vector' | 'enumeration' | 'record' | 'union' | 'combination'.

-type cauterize_synonym() :: {'descriptor', 'synonym', Name :: atom(), Type :: cauterize_type()}.
-type cauterize_enumeration() :: {'descriptor', 'enumeration', Name :: atom(), {Tag :: cauterize_tag(), [{Field:: atom(), Index :: non_neg_integer()}, ...]}}.
-type cauterize_range() :: {'descriptor', 'range', Name :: atom(), {Min :: integer(), Max :: integer(), Tag :: cauterize_tag()}}.
-type cauterize_union() :: {'descriptor', 'union', Name :: atom(), {Tag :: cauterize_tag(), [{'data', FieldName :: atom(), Index :: non_neg_integer(), FieldType :: atom()} | {'empty', FieldName :: atom(), Index :: non_neg_integer()}, ...]}}.
-type cauterize_combination() :: {'descriptor', 'combination', Name :: atom(), {Tag :: cauterize_tag(), [{'data', FieldName :: atom(), Index :: non_neg_integer(), FieldType :: atom()} | {'empty', FieldName :: atom(), Index :: non_neg_integer()}, ...]}}.
-type cauterize_array() :: {'descriptor', 'array', Name :: atom(), { Type :: atom(), Length :: pos_integer()}}.
-type cauterize_vector() :: {'descriptor', 'vector', Name :: atom(), { Type :: atom(), MaxLength :: pos_integer(), Tag :: cauterize_tag()}}.
-type cauterize_record() :: {'descriptor', 'record', Name :: atom(), [{'data', FieldName :: atom(), Index :: non_neg_integer(), Type :: atom()}, ...]}.

-type cauterize_spec() :: list(
                        cauterize_synonym() |
                        cauterize_enumeration() |
                        cauterize_range() |
                        cauterize_union() |
                        cauterize_array() |
                        cauterize_vector() |
                        cauterize_record() |
                        cauterize_combination()).

-type decode_reason() :: {'unexpected_end_of_input', Type :: atom(), Name :: atom(), Remainder :: binary()} |
                         {'oversize_vector', Name :: atom(), Length :: pos_integer(), MaxLen :: pos_integer()} |
                         {'invalid_range_value', Name :: atom(), Value :: any(), Length :: pos_integer()} |
                         {'out_of_range_enumeration_value', Name :: atom(), Index :: non_neg_integer()} |
                         {'bad_union_index', Name :: atom(), Index :: non_neg_integer()}.

-type encode_reason() :: {'oversize_vector', Name :: atom(), Size :: pos_integer(), MaxSize :: pos_integer()} |
                         {'incorrect_array_size', Name :: atom(), non_neg_integer(), Size :: pos_integer()} |
                         {'invalid_range_value', Name :: atom(), Value :: non_neg_integer()} |
                         {'unknown_enumeration_field', Name :: atom(), Field :: atom()} |
                         {'missing_record_field', Name :: atom(), Field :: atom()} |
                         {'unknown_union_member', Name :: atom(), Field :: atom()} |
                         {'data_supplied_for_empty_union_member', Name :: atom(), Field :: atom()}.

-type decode_stack() :: [] | [{TypeName :: atom(), [{FieldName :: atom(), Value :: any()}]}].

-type decode_result() :: {'ok', any()} | {'ok', any(), binary()} | {'error', {decode_reason(), decode_stack()}}.
-type encode_result() :: {'ok', binary()} | {'error', encode_reason()}.

-export_type([encode_result/0, decode_result/0]).

-spec decode(Bin :: binary(), Name :: atom() | [atom(),...], Spec :: cauterize_spec()) -> decode_result().
decode(<<>>, _, _) ->
    {error, {no_input, []}};
decode(Bin, Name, Spec) when is_atom(Name) ->
    decode(Bin, [Name], Spec);
decode(Bin, Names, Spec) ->
    try decode_int(Bin, Names, [], Spec) of
        {Decoded, Rem} -> {ok, Decoded, Rem};
        R -> {ok, R}
    catch
        throw:{Reason, [TopType|[Stack]]} ->
            {error, {Reason, [{TopType, fixup_stacktrace(Stack)}]}};
        throw:{Reason, [TopType|Stack]} ->
            R = fixup_stacktrace(Stack),
            {error, {Reason, [{TopType, lists:flatten(R)}]}}

    end.

fixup_stacktrace([]) -> [];
fixup_stacktrace(X) when is_list(X) ->
    replace(lists:reverse(fixup_stacktrace_int(lists:reverse(X))), '?');
fixup_stacktrace(X) ->
    replace(X, '?').

fixup_stacktrace_int([A]) ->
    [replace(A, '?')];
fixup_stacktrace_int([A,B|C]) ->
    case last(B) of
      '__duct_tape__' ->
            fixup_stacktrace_int([replace(B, fixup_stacktrace(A))|C]);
        _ ->
            R = fixup_stacktrace_int([B|C]),
            [A|R]
    end;
fixup_stacktrace_int(X) ->
    X.

last(X) when is_list(X) ->
    last(lists:last(X));
last(X) when is_tuple(X) ->
    last(element(tuple_size(X), X));
last(X) ->
    X.

replace(X, Rep) when is_list(X) ->
    R = lists:droplast(X) ++ [replace(lists:last(X), Rep)],
    R;
replace(X, Rep) when is_tuple(X) ->
    R = setelement(tuple_size(X), X, replace(element(tuple_size(X), X), Rep)),
    R;
replace('__duct_tape__', Rep) ->
    Rep;
replace(X, _Rep) ->
    X.

decode_int(<<>>, _, Acc, _) ->
    lists:reverse(Acc);
decode_int(Rem, [], Acc, _) ->
    {lists:reverse(Acc), Rem};
decode_int(Bin, [Name|T], Acc, Spec) ->
    {descriptor, Prototype, Name, Desc} = lookup_type(Name, Spec),
    {Decoded,Rem} = decode_internal(Bin, Prototype, Name, Desc, Spec, [Name]),
    decode_int(Rem, T, [{Name, Decoded}|Acc], Spec).

decode_internal(<<>>, Type, Name, _, _, Stack) ->
  throw({{unexpected_end_of_input, Type, Name, <<>>}, lists:reverse(Stack)});
decode_internal(<<Val:8/integer-unsigned-little,Rem/binary>>, primitive, _, u8, _Spec, _Stack) -> {Val,Rem};
decode_internal(<<Val:16/integer-unsigned-little,Rem/binary>>, primitive, _, u16, _Spec, _Stack) -> {Val,Rem};
decode_internal(<<Val:32/integer-unsigned-little,Rem/binary>>, primitive, _, u32, _Spec, _Stack) -> {Val,Rem};
decode_internal(<<Val:64/integer-unsigned-little,Rem/binary>>, primitive, _, u64, _Spec, _Stack) -> {Val,Rem};
decode_internal(<<Val:8/integer-signed-little,Rem/binary>>, primitive, _, s8, _Spec, _Stack) -> {Val,Rem};
decode_internal(<<Val:16/integer-signed-little,Rem/binary>>, primitive, _, s16, _Spec, _Stack) -> {Val,Rem};
decode_internal(<<Val:32/integer-signed-little,Rem/binary>>, primitive, _, s32, _Spec, _Stack) -> {Val,Rem};
decode_internal(<<Val:64/integer-signed-little,Rem/binary>>, primitive, _, s64, _Spec, _Stack) -> {Val,Rem};
decode_internal(<<1:8/integer-unsigned-little,Rem/binary>>, primitive, _, bool, _Spec, _Stack) -> {true,Rem};
decode_internal(<<0:8/integer-unsigned-little,Rem/binary>>, primitive, _, bool, _Spec, _Stack) -> {false,Rem};

decode_internal(<<0,0,128,127,Rem/binary>>, primitive, _, f32, _Spec, _Stack) -> {pos_inf,Rem};
decode_internal(<<0,0,128,255,Rem/binary>>, primitive, _, f32, _Spec, _Stack) -> {neg_inf,Rem};
decode_internal(<<0,0,192,255,Rem/binary>>, primitive, _, f32, _Spec, _Stack) -> {nan,Rem};
decode_internal(<<Value:32/float-unsigned-little,Rem/binary>>, primitive, _, f32, _Spec, _Stack) -> {Value,Rem};
decode_internal(<<0,0,0,0,0,0,240,127,Rem/binary>>, primitive, _, f64, _Spec, _Stack) -> {pos_inf,Rem};
decode_internal(<<0,0,0,0,0,0,240,255,Rem/binary>>, primitive, _, f64, _Spec, _Stack) -> {neg_inf,Rem};
decode_internal(<<0,0,0,0,0,0,248,255,Rem/binary>>, primitive, _, f64, _Spec, _Stack) -> {nan,Rem};
decode_internal(<<Value:64/float-unsigned-little,Rem/binary>>, primitive, _, f64, _Spec, _Stack) -> {Value,Rem};
decode_internal(Bin, primitive, _, Name, _Spec, _Stack) -> throw({{unexpected_end_of_input, primitive, Name, Bin}, lists:reverse(_Stack)});

decode_internal(Bin, synonym, _Name, RefName, Spec, _Stack) ->
    {descriptor, RefProto, RefName, _Desc} = lookup_type(RefName, Spec),
    decode_internal(Bin, RefProto, RefName, _Desc, Spec, _Stack);

decode_internal(Bin, vector, Name, {RefName, MaxLen, Tag}, Spec, _Stack) ->
    {descriptor, RefProto, RefName, _Desc} = lookup_type(RefName, Spec),
    {Length, Rem} = decode_tag(Bin, Tag, Spec, _Stack),
    case Length > MaxLen of
        true -> throw({{oversize_vector, Name, Length, MaxLen}, lists:reverse(_Stack)});
        _ -> ok
    end,
    {FinalRem, Vals} = lists:foldl(fun(_, {FoldRem, Acc}) ->
                        {Val, NextRem} = decode_internal(FoldRem, RefProto, RefName, _Desc, Spec, [lists:reverse(Acc)|_Stack]),
                        {NextRem, [Val|Acc]}
                end, {Rem, []}, lists:seq(0, Length - 1)),
    Res = case RefName of
              char -> list_to_binary(lists:reverse(Vals));
              _ -> lists:reverse(Vals)
          end,
    {Res, FinalRem};
decode_internal(Bin, array, _Name, {RefName, Length}, Spec, _Stack) ->
    {descriptor, RefProto, RefName, _Desc} = lookup_type(RefName, Spec),
    {FinalRem, Vals} = lists:foldl(fun(_, {FoldRem, Acc}) ->
                        {Val, NextRem} = decode_internal(FoldRem, RefProto, RefName, _Desc, Spec, [lists:reverse(Acc)|_Stack]),
                        {NextRem, [Val|Acc]}
                end, {Bin, []}, lists:seq(0, Length - 1)),
    {lists:reverse(Vals), FinalRem};
decode_internal(Bin, range, Name, {Offset, Length, Tag}, Spec, _Stack) ->
    {Value, Rem} = decode_tag(Bin, Tag, Spec, _Stack),
    case Value > Length of
        true -> throw({{invalid_range_value, Name, Value, Length}, lists:reverse(_Stack)});
        _ -> ok
    end,
    {Value + Offset, Rem};
decode_internal(Bin, enumeration, _Name, {Tag, States}, Spec, _Stack) ->
    {Index, Rem} = decode_tag(Bin, Tag, Spec, _Stack),
    {Value, Index} = case lists:keyfind(Index, 2, States) of
                         false ->
                             throw({{out_of_range_enumeration_value, _Name, Index}, lists:reverse(_Stack)});
                         R -> R
                     end,
    {Value, Rem};
decode_internal(Bin, record, _Name, InstFields, Spec, _Stack) ->
    {FinalBin, Vals} = lists:foldl(fun({data, FieldName, _, RefName}, {FoldBin, Acc}) ->
                                           {descriptor, RefProto, RefName, _Desc} = lookup_type(RefName, Spec),
                                           {Val, NextRem} = decode_internal(FoldBin, RefProto, RefName, _Desc, Spec, [lists:reverse([{FieldName, '__duct_tape__'}|Acc])|_Stack]),
                                           {NextRem,[{FieldName, Val}|Acc]}
                                   end, {Bin, []}, InstFields),
    {lists:reverse(Vals), FinalBin};
decode_internal(Bin, union, _Name, {Tag, InstFields}, Spec, _Stack) ->
    {Index, Rem} = decode_tag(Bin, Tag, Spec, _Stack),
    case lists:keyfind(Index, 3, InstFields) of
        {data, FieldName, Index, RefName} ->
            {descriptor, RefProto, RefName, _Desc} = lookup_type(RefName, Spec),
            {Value, FinalRem} = decode_internal(Rem, RefProto, RefName, _Desc, Spec, [[{FieldName, '__duct_tape__'}]|_Stack]),
            {[{FieldName, Value}], FinalRem};
        {empty, FieldName, Index} ->
            {[{FieldName, true}], Rem};
        false ->
            throw({{bad_union_index, _Name, Index}, lists:reverse(_Stack)})
    end;

decode_internal(Bin, combination, _Name, {Tag, InstFields}, Spec, _Stack) ->
    {Flags, Rem} = decode_tag(Bin, Tag, Spec, _Stack),
    FieldCount = length(InstFields),
    BitFlags = lists:reverse([ X || <<X:1>> <= <<Flags:FieldCount/integer-unsigned-little>> ]),
    {FinalRem, Values} = lists:foldl(fun({1, {empty, FieldName, _}}, {FoldBin,Acc}) ->
                                             {FoldBin, [FieldName|Acc]};
                                        ({1, {data, FieldName, _, RefName}}, {FoldBin,Acc}) ->
                                             {descriptor, RefProto, RefName, _Desc} = lookup_type(RefName, Spec),
                                             {Value, NextRem} = decode_internal(FoldBin, RefProto, RefName, _Desc, Spec, [lists:reverse([{FieldName, '__duct_tape__'}|Acc])|_Stack]),
                                             {NextRem, [{FieldName, Value}|Acc]};
                                        ({0, _}, Acc) -> Acc
                                     end, {Rem, []}, lists:zip(BitFlags, InstFields)),
    {lists:reverse(Values), FinalRem}.

decode_tag(Bin, Tag, Spec, _Stack) ->
    Prim = tag_to_prim(Tag),
    decode_internal(Bin, primitive, Prim, Prim, Spec, _Stack).

-spec encode([{TypeName :: atom(), Value :: any()}, ...], Spec :: cauterize_spec()) -> encode_result().
encode([{_TypeName, _Value}|_T]=List, Spec) ->
    try [encode(TypeName, Value, Spec) || {TypeName, Value} <- List] of
        R -> {ok, R}
    catch
        throw:Reason ->
            {error, Reason}
    end.

encode(TypeName, Value, Spec) ->
    {descriptor, Prototype, Name, _Desc} = lookup_type(TypeName, Spec),
    encode_int({instance, Prototype, Name, Value}, Spec).

encode_int({instance, primitive, u8, Value}, _Spec) ->
    <<Value:8/integer-unsigned-little>>;
encode_int({instance, primitive, char, Value}, _Spec) ->
    <<Value:8/integer-unsigned-little>>;
encode_int({instance, primitive, u16, Value}, _Spec) ->
    <<Value:16/integer-unsigned-little>>;
encode_int({instance, primitive, u32, Value}, _Spec) ->
    <<Value:32/integer-unsigned-little>>;
encode_int({instance, primitive, u64, Value}, _Spec) ->
    <<Value:64/integer-unsigned-little>>;
encode_int({instance, primitive, s8, Value}, _Spec) ->
    <<Value:8/integer-signed-little>>;
encode_int({instance, primitive, s16, Value}, _Spec) ->
    <<Value:16/integer-signed-little>>;
encode_int({instance, primitive, s32, Value}, _Spec) ->
    <<Value:32/integer-signed-little>>;
encode_int({instance, primitive, s64, Value}, _Spec) ->
    <<Value:64/integer-signed-little>>;
encode_int({instance, primitive, bool, true}, _Spec) ->
    <<1:8/integer-unsigned-little>>;
encode_int({instance, primitive, bool, false}, _Spec) ->
    <<0:8/integer-unsigned-little>>;
encode_int({instance, primitive, f32, pos_inf}, _Spec) -> <<0,0,128,127>>;
encode_int({instance, primitive, f32, neg_inf}, _Spec) -> <<0,0,128,255>>;
encode_int({instance, primitive, f32, nan}, _Spec) -> <<0,0,192,255>>;
encode_int({instance, primitive, f32, Value}, _Spec) -> <<Value:32/float-signed-little>>;
encode_int({instance, primitive, f64, pos_inf}, _Spec) -> <<0,0,0,0,0,0,240,127>>;
encode_int({instance, primitive, f64, neg_inf}, _Spec) -> <<0,0,0,0,0,0,240,255>>;
encode_int({instance, primitive, f64, nan}, _Spec) -> <<0,0,0,0,0,0,248,255>>;
encode_int({instance, primitive, f64, Value}, _Spec) -> <<Value:64/float-signed-little>>;

encode_int({instance, synonym, Name, Value}, Spec) ->
    {descriptor, synonym, Name, RefName} = lookup_type(Name, synonym, Spec),
    {descriptor, Prototype, RefName, _Desc} = lookup_type(RefName, Spec),
    encode_int({instance, Prototype, RefName, Value}, Spec);

encode_int({instance, vector, Name, Values}, Spec) when is_list(Values) ->
    {descriptor, vector, Name, {RefName, MaxSize, Tag}} = lookup_type(Name, vector, Spec),
    case length(Values) > MaxSize of
        true -> throw({oversize_vector, Name, length(Values), MaxSize});
        _ -> ok
    end,
    {descriptor, Prototype, RefName, _Desc} = lookup_type(RefName, Spec),
    [encode_int({instance, primitive, tag_to_prim(Tag), length(Values)}, Spec)|
     [encode_int({instance, Prototype, RefName, V}, Spec)||V <- Values]];

encode_int({instance, vector, Name, Values}, Spec) when is_binary(Values) ->
    encode_int({instance, vector, Name, binary_to_list(Values)}, Spec);

encode_int({instance, array, Name, Values}, Spec) when is_list(Values) ->
    {descriptor, array, Name, {RefName, Size}} = lookup_type(Name, array, Spec),
    case length(Values) /= Size of
        true -> throw({incorrect_array_size, Name, length(Values), Size});
        _ -> ok
    end,
    {descriptor, Prototype, RefName, _Desc} = lookup_type(RefName, Spec),
    [encode_int({instance, Prototype, RefName, V}, Spec)||V <- Values];

encode_int({instance, range, Name, Value}, Spec) when is_integer(Value) ->
    {descriptor, range, Name, {Offset, Length, Tag}} = lookup_type(Name, range, Spec),
    NewValue = Value - Offset,
    case NewValue of
        0 ->
            throw({invalid_range_value, Name, Value});
        N when N > Length ->
            throw({invalid_range_value, Name, Value});
        _ -> ok
    end,
    [encode_int({instance, primitive, tag_to_prim(Tag), NewValue}, Spec)];

encode_int({instance, enumeration, Name, Value}, Spec) when is_atom(Value) ->
    {descriptor, enumeration, Name, {Tag, States}} = lookup_type(Name, enumeration, Spec),
    {Value, Index} = case lists:keyfind(Value, 1, States) of
                         false ->
                             throw({unknown_enumeration_field, Name, Value});
                         R -> R
                     end,
    [encode_int({instance, primitive, tag_to_prim(Tag), Index}, Spec)];

encode_int({instance, record, Name, InstFields}, Spec) ->
    {descriptor, record, Name, DescFields} = lookup_type(Name, record, Spec),
    RecData = lists:foldl(fun({data, FieldName, _, RefName}, Acc) ->
                                  {FieldName, Value} = case lists:keyfind(FieldName, 1, InstFields) of
                                                           false ->
                                                               throw({missing_record_field, Name, FieldName});
                                                           R -> R
                                                       end,
                                  {descriptor, Prototype, RefName, _Desc} = lookup_type(RefName, Spec),
                                  [encode_int({instance, Prototype, RefName, Value}, Spec)|Acc]
                end, [], DescFields),
    lists:reverse(RecData);

encode_int({instance, union, Name, [{FieldName, Value}]}, Spec) when is_atom(FieldName) ->
    {descriptor, union, Name, {Tag, Fields}} = lookup_type(Name, union, Spec),
    case lists:keyfind(FieldName, 2, Fields) of
        false ->
            throw({unknown_union_member, Name, FieldName});
        {empty, FieldName, Index} when Value == true ->
            [encode_int({instance, primitive, tag_to_prim(Tag), Index}, Spec)];
        {empty, FieldName, _Index} ->
            throw({data_supplied_for_empty_union_member, Name, FieldName});
        {data, FieldName, Index, RefName} ->
            {descriptor, Prototype, RefName, _Desc} = lookup_type(RefName, Spec),
            [encode_int({instance, primitive, tag_to_prim(Tag), Index}, Spec),
             encode_int({instance, Prototype, RefName, Value}, Spec)]
    end;
encode_int({instance, combination, Name, InstFields}, Spec) ->
    {descriptor, combination, Name, {Tag, DescFields}} = lookup_type(Name, combination, Spec),
    {BitTag,RecData} = lists:foldl(fun({data, FieldName, Index, RefName}, {BitField,Acc}) ->
                                           case lists:keyfind(FieldName, 1, InstFields) of
                                               {FieldName, Value} ->
                                                   {descriptor, Prototype, RefName, _Desc} = lookup_type(RefName, Spec),
                                                   {BitField bor (1 bsl Index),
                                                    [encode_int({instance, Prototype, RefName, Value}, Spec)|Acc]};
                                               false -> {BitField, Acc}
                                           end;
                                      ({empty, FieldName, Index}, {BitField,Acc}) ->
                                           case lists:member(FieldName, InstFields) of
                                               true -> {BitField bor (1 bsl Index), Acc};
                                               false -> {BitField, Acc}
                                           end
                                   end, {0,[]}, DescFields),
    [encode_int({instance, primitive, tag_to_prim(Tag), BitTag},Spec)|lists:reverse(RecData)];
encode_int(Instance, _Spec) ->
    throw({invalid_instance, Instance}).

tag_to_prim(tag8) -> u8;
tag_to_prim(tag16) -> u16;
tag_to_prim(tag32) -> u32;
tag_to_prim(tag64) -> u64;
tag_to_prim(Tag) -> throw({invalid_tag, Tag}).


lookup_type(u8, _Spec) -> {descriptor, primitive, u8, u8};
lookup_type(u16, _Spec) -> {descriptor, primitive, u16, u16};
lookup_type(u32, _Spec) -> {descriptor, primitive, u32, u32};
lookup_type(u64, _Spec) -> {descriptor, primitive, u64, u64};
lookup_type(s8, _Spec) -> {descriptor, primitive, s8, s8};
lookup_type(s16, _Spec) -> {descriptor, primitive, s16, s16};
lookup_type(s32, _Spec) -> {descriptor, primitive, s32, s32};
lookup_type(s64, _Spec) -> {descriptor, primitive, s64, s64};
lookup_type(f32, _Spec) -> {descriptor, primitive, f32, f32};
lookup_type(f64, _Spec) -> {descriptor, primitive, f64, f64};
lookup_type(bool, _Spec) -> {descriptor, primitive, bool, bool};
lookup_type(char, _Spec) -> {descriptor, primitive, char, u8};
lookup_type(Name, Spec) ->
    case lists:keyfind(Name, 3, Spec) of
        false ->
            throw({unknown_type, Name});
        R -> R
    end.

lookup_type(Name, Type, Spec) ->
    case lookup_type(Name, Spec) of
        false ->
          throw({unknown_type, Name});
        R ->
            case element(2, R) of
                Type ->
                    R;
                OtherType ->
                    throw({type_mismatch, Name, Type, OtherType})
            end
    end.
