-module(cauterize).
-export([decode/2, encode/2]).

decode(_, _) ->
     ok.

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
    [encode({instance, primitive, tag_to_prim(Tag), Index}, Spec)].


tag_to_prim(tag8) -> u8;
tag_to_prim(tag16) -> u16;
tag_to_prim(tag32) -> u32;
tag_to_prim(tag64) -> u64.


lookup_type(u8, _Spec) -> {descriptor, primitive, u8, 1};
lookup_type(u16, _Spec) -> {descriptor, primitive, u16, 2};
lookup_type(u32, _Spec) -> {descriptor, primitive, u32, 4};
lookup_type(u64, _Spec) -> {descriptor, primitive, u64, 8};
lookup_type(Name, Spec) -> lists:keyfind(Name, 3, Spec).


%% encode({instance, synonym, Name, Value}) ->
