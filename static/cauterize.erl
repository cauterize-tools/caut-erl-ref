-module(cauterize).
-export([decode/3, encode/2]).

decode(Bin, Name, Spec) ->
    {descriptor, Prototype, Name, Desc} = lookup_type(Name, Spec),
    {Decoded,Rem} = decode_internal(Bin, Prototype, Desc, Spec),
    {{instance, Prototype, Name, Decoded}, Rem}.

decode_internal(<<Val:8/integer-unsigned-little,Rem/binary>>, primitive, u8, _Spec) -> {Val,Rem};
decode_internal(<<Val:16/integer-unsigned-little,Rem/binary>>, primitive, u16, _Spec) -> {Val,Rem};
decode_internal(<<Val:32/integer-unsigned-little,Rem/binary>>, primitive, u32, _Spec) -> {Val,Rem};
decode_internal(<<Val:64/integer-unsigned-little,Rem/binary>>, primitive, u64, _Spec) -> {Val,Rem};
decode_internal(<<Val:8/integer-signed-little,Rem/binary>>, primitive, s8, _Spec) -> {Val,Rem};
decode_internal(<<Val:16/integer-signed-little,Rem/binary>>, primitive, s16, _Spec) -> {Val,Rem};
decode_internal(<<Val:32/integer-signed-little,Rem/binary>>, primitive, s32, _Spec) -> {Val,Rem};
decode_internal(<<Val:64/integer-signed-little,Rem/binary>>, primitive, s64, _Spec) -> {Val,Rem};
decode_internal(<<1:8/integer-unsigned-little,Rem/binary>>, primitive, bool, _Spec) -> {true,Rem};
decode_internal(<<0:8/integer-unsigned-little,Rem/binary>>, primitive, bool, _Spec) -> {false,Rem};

decode_internal(Bin, synonym, RefName, Spec) ->
    {descriptor, RefProto, RefName, _Desc} = lookup_type(RefName, Spec),
    decode_internal(Bin, RefProto, RefName, Spec);

decode_internal(Bin, vector, {RefName, MaxLen, Tag}, Spec) ->
    {descriptor, RefProto, RefName, _Desc} = lookup_type(RefName, Spec),
    {Length, Rem} = decode_internal(Bin, primitive, tag_to_prim(Tag), Spec),
    true = Length =< MaxLen,
    {FinalRem, Vals} = lists:foldl(fun(_, {FoldRem, Acc}) ->
                        {Val, NextRem} = decode_internal(FoldRem, RefProto, RefName, Spec),
                        {NextRem, [Val|Acc]}
                end, {Rem, []}, lists:seq(0, Length - 1)),
    {lists:reverse(Vals), FinalRem};
decode_internal(Bin, array, {RefName, Length}, Spec) ->
    {descriptor, RefProto, RefName, _Desc} = lookup_type(RefName, Spec),
    {FinalRem, Vals} = lists:foldl(fun(_, {FoldRem, Acc}) ->
                        {Val, NextRem} = decode_internal(FoldRem, RefProto, RefName, Spec),
                        {NextRem, [Val|Acc]}
                end, {Bin, []}, lists:seq(0, Length - 1)),
    {lists:reverse(Vals), FinalRem};
decode_internal(Bin, range, {Offset, Length, Tag}, Spec) ->
    {Value,Rem} = decode_internal(Bin, primitive, tag_to_prim(Tag), Spec),
    true = Value =< Length,
    {Value + Offset, Rem};
decode_internal(Bin, enumeration, {Tag, States}, Spec) ->
    {Index,Rem} = decode_internal(Bin, primitive, tag_to_prim(Tag), Spec),
    {Value, Index} = lists:keyfind(Index, 2, States),
    {Value, Rem};
decode_internal(Bin, record, InstFields, Spec) ->
    {FinalBin, Vals} = lists:foldl(fun({data, FieldName, _, RefName}, {FoldBin, Acc}) ->
                                           {descriptor, RefProto, RefName, _Desc} = lookup_type(RefName, Spec),
                                           {Val, NextRem} = decode_internal(FoldBin, RefProto, RefName, Spec),
                                           {NextRem,[{FieldName, Val}|Acc]}
                                   end, {Bin, []}, InstFields),
    {lists:reverse(Vals), FinalBin};
decode_internal(Bin, union, {Tag, InstFields}, Spec) ->
    {Index,Rem} = decode_internal(Bin, primitive, tag_to_prim(Tag), Spec),
    case lists:keyfind(Index, 3, InstFields) of
        {data, FieldName, Index, RefName} ->
            {descriptor, RefProto, RefName, _Desc} = lookup_type(RefName, Spec),
            {Value, FinalRem} = decode_internal(Rem, RefProto, RefName, Spec),
            {{FieldName, Value}, FinalRem};
        {empty, FieldName, Index} ->
            {FieldName, Rem}
    end;

decode_internal(Bin, combination, {Tag, InstFields}, Spec) ->
    {Flags,Rem} = decode_internal(Bin, primitive, tag_to_prim(Tag), Spec),
    FieldCount = length(InstFields),
    BitFlags = lists:reverse([ X || <<X:1>> <= <<Flags:FieldCount/integer-unsigned-little>> ]),
    {FinalRem, Values} = lists:foldl(fun({1, {empty, FieldName, _}}, {FoldBin,Acc}) ->
                                             {FoldBin, [FieldName|Acc]};
                                        ({1, {data, FieldName, _, RefName}}, {FoldBin,Acc}) ->
                                             {descriptor, RefProto, RefName, _Desc} = lookup_type(RefName, Spec),
                                             {Value, NextRem} = decode_internal(FoldBin, RefProto, RefName, Spec),
                                             {NextRem, [{FieldName, Value}|Acc]};
                                        ({0, _}, Acc) -> Acc
                                     end, {Rem, []}, lists:zip(BitFlags, InstFields)),
    {lists:reverse(Values), FinalRem}.

encode({instance, primitive, u8, Value}, _Spec) ->
    <<Value:8/integer-unsigned-little>>;
encode({instance, primitive, u16, Value}, _Spec) ->
    <<Value:16/integer-unsigned-little>>;
encode({instance, primitive, u32, Value}, _Spec) ->
    <<Value:32/integer-unsigned-little>>;
encode({instance, primitive, u64, Value}, _Spec) ->
    <<Value:64/integer-unsigned-little>>;
encode({instance, primitive, s8, Value}, _Spec) ->
    <<Value:8/integer-signed-little>>;
encode({instance, primitive, s16, Value}, _Spec) ->
    <<Value:16/integer-signed-little>>;
encode({instance, primitive, s32, Value}, _Spec) ->
    <<Value:32/integer-signed-little>>;
encode({instance, primitive, s64, Value}, _Spec) ->
    <<Value:64/integer-signed-little>>;
encode({instance, primitive, bool, true}, _Spec) ->
    <<1:8/integer-unsigned-little>>;
encode({instance, primitive, bool, false}, _Spec) ->
    <<0:8/integer-unsigned-little>>;

encode({instance, synonym, Name, Value}, Spec) ->
    {descriptor, synonym, Name, RefName} = lookup_type(Name, Spec),
    {descriptor, Prototype, RefName, _Desc} = lookup_type(RefName, Spec),
    encode({instance, Prototype, RefName, Value}, Spec);

encode({instance, vector, Name, Values}, Spec) when is_list(Values) ->
    {descriptor, vector, Name, {RefName, MaxSize, Tag}} = lookup_type(Name, Spec),
    true = length(Values) =< MaxSize,
    {descriptor, Prototype, RefName, _Desc} = lookup_type(RefName, Spec),
    [encode({instance, primitive, tag_to_prim(Tag), length(Values)}, Spec)|
     [encode({instance, Prototype, RefName, V}, Spec)||V <- Values]];

encode({instance, array, Name, Values}, Spec) when is_list(Values) ->
    {descriptor, array, Name, {RefName, Size}} = lookup_type(Name, Spec),
    true = length(Values) == Size,
    {descriptor, Prototype, RefName, _Desc} = lookup_type(RefName, Spec),
    [encode({instance, Prototype, RefName, V}, Spec)||V <- Values];

encode({instance, range, Name, Value}, Spec) when is_integer(Value) ->
    {descriptor, range, Name, {Offset, Length, Tag}} = lookup_type(Name, Spec),
    NewValue = Value - Offset,
    true = NewValue =< Length andalso NewValue >= 0,
    [encode({instance, primitive, tag_to_prim(Tag), NewValue}, Spec)];

encode({instance, enumeration, Name, Value}, Spec) when is_atom(Value) ->
    {descriptor, enumeration, Name, {Tag, States}} = lookup_type(Name, Spec),
    {Value, Index} = lists:keyfind(Value, 1, States),
    [encode({instance, primitive, tag_to_prim(Tag), Index}, Spec)];

encode({instance, record, Name, InstFields}, Spec) ->
    {descriptor, record, Name, DescFields} = lookup_type(Name, Spec),
    RecData = lists:foldl(fun({data, FieldName, _, RefName}, Acc) ->
                                  {FieldName, Value} = lists:keyfind(FieldName, 1, InstFields),
                                  {descriptor, Prototype, RefName, _Desc} = lookup_type(RefName, Spec),
                                  [encode({instance, Prototype, RefName, Value}, Spec)|Acc]
                end, [], DescFields),
    lists:reverse(RecData);

encode({instance, union, Name, {FieldName, Value}}, Spec) when is_atom(FieldName) ->
    {descriptor, union, Name, {Tag, Fields}} = lookup_type(Name, Spec),
    {data, FieldName, Index, RefName} = lists:keyfind(FieldName, 2, Fields),
    {descriptor, Prototype, RefName, _Desc} = lookup_type(RefName, Spec),
    [encode({instance, primitive, tag_to_prim(Tag), Index}, Spec),
     encode({instance, Prototype, RefName, Value}, Spec)];

encode({instance, union, Name, FieldName}, Spec) when is_atom(FieldName) ->
    {descriptor, union, Name, {Tag, Fields}} = lookup_type(Name, Spec),
    {empty, FieldName, Index} = lists:keyfind(FieldName, 2, Fields),
    [encode({instance, primitive, tag_to_prim(Tag), Index}, Spec)];

encode({instance, combination, Name, InstFields}, Spec) ->
    {descriptor, combination, Name, {Tag, DescFields}} = lookup_type(Name, Spec),
    {BitTag,RecData} = lists:foldl(fun({data, FieldName, Index, RefName}, {BitField,Acc}) ->
                                           case lists:keyfind(FieldName, 1, InstFields) of
                                               {FieldName, Value} ->
                                                   {descriptor, Prototype, RefName, _Desc} = lookup_type(RefName, Spec),
                                                   {BitField bor (1 bsl Index),
                                                    [encode({instance, Prototype, RefName, Value}, Spec)|Acc]};
                                               false -> {BitField, Acc}
                                           end;
                                      ({empty, FieldName, Index}, {BitField,Acc}) ->
                                           case lists:member(FieldName, InstFields) of
                                               true -> {BitField bor (1 bsl Index), Acc};
                                               false -> {BitField, Acc}
                                           end
                                   end, {0,[]}, DescFields),
    [encode({instance, primitive, tag_to_prim(Tag), BitTag},Spec)|lists:reverse(RecData)].

tag_to_prim(tag8) -> u8;
tag_to_prim(tag16) -> u16;
tag_to_prim(tag32) -> u32;
tag_to_prim(tag64) -> u64.


lookup_type(u8, _Spec) -> {descriptor, primitive, u8, u8};
lookup_type(u16, _Spec) -> {descriptor, primitive, u16, u16};
lookup_type(u32, _Spec) -> {descriptor, primitive, u32, u32};
lookup_type(u64, _Spec) -> {descriptor, primitive, u64, u64};
lookup_type(s8, _Spec) -> {descriptor, primitive, s8, s8};
lookup_type(s16, _Spec) -> {descriptor, primitive, s16, s16};
lookup_type(s32, _Spec) -> {descriptor, primitive, s32, s32};
lookup_type(s64, _Spec) -> {descriptor, primitive, s64, s64};
lookup_type(f32, _Spec) -> {descriptor, primitive, f32, s32};
lookup_type(f64, _Spec) -> {descriptor, primitive, f64, s64};
lookup_type(bool, _Spec) -> {descriptor, primitive, bool, bool};
lookup_type(Name, Spec) -> lists:keyfind(Name, 3, Spec).


%% encode({instance, synonym, Name, Value}) ->
