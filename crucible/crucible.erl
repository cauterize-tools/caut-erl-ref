#!/usr/bin/env escript

main([]) ->
    %% make sure cauterize.beam is in the code path
    true = code:add_pathz(filename:dirname(escript:script_name()) 
                          ++ "/../_build/default/lib/cauterize/ebin"),
    [Beam] = filelib:wildcard("*.beam"),
   %io:format(standard_error, "beam ~p~n", [Beam]),
    ModuleStr = filename:basename(Beam, ".beam"),
    {module, Module} = code:load_abs(ModuleStr),
    Fingerprints = Module:fingerprints(),
    TagSize = Module:tag_size(),
    MaxSize = Module:max_size(),
   %io:format(standard_error, "tag size ~p max size ~p~n", [TagSize, MaxSize]),
   %io:format(standard_error, "fingerprints ~p~n", [Fingerprints]),
    LengthBytes = calc_length_bytes(MaxSize),
    X0 = io:get_chars("", LengthBytes),
    ByteCount = LengthBytes*8,
    <<Len:ByteCount/integer-unsigned-little>> = list_to_binary(X0),
   %io:format(standard_error, "length is ~p ~p~n", [list_to_binary(X0), Len]),
    X1 = io:get_chars("", TagSize),
    <<Fingerprint:TagSize/binary>> = list_to_binary(X1),
   %io:format(standard_error, "fingerprint ~p~n", [Fingerprint]),
    Type = lookup_fingerprint(Fingerprint, Fingerprints),
   %io:format(standard_error, "type ~p~n", [Type]),
    X2 = io:get_chars("", Len),
    Payload = list_to_binary(X2),
    %io:format(standard_error, "Payload ~p~n", [Payload]),
    case Module:decode(Payload, Type) of
        {ok, Decoded} ->
           %io:format(standard_error, "Decoded ~p~n", [Decoded]),
            %% decoded OK, no trailing bytes
            {ok, Encoded0} = Module:encode(Decoded),
            Encoded = list_to_binary(Encoded0),
            EncodedLen = byte_size(Encoded),
            Output = <<EncodedLen:ByteCount/integer-unsigned-little, Fingerprint/binary, Encoded/binary>>,
            %io:fwrite(binary_to_list(Output)),
            Stdout = erlang:open_port({fd, 0, 1}, [out, binary, stream]),
            port_command(Stdout, Output),
           %io:format(standard_error, "wrote ~p to stdout", [Output]),
            port_close(Stdout);
        {ok, _Decoded, _Rem} ->
            io:fwrite("error"),
           %io:format(standard_error, "WARNING trailing bytes found ~p~n", [Rem]);
            halt(1);
        {error, _Reason} ->
            io:fwrite("error"),
           %io:format(standard_error, "ERROR decode failed ~p~n", [Reason])
            halt(1)
    end.

calc_length_bytes(MaxSize) ->
    case MaxSize of
        X when X < 16#ff ->
            1;
        X when X < 16#ffff ->
            2;
        X when X < 16#ffffffff ->
            4
    end.

lookup_fingerprint(FP, FPs) ->
    Len = byte_size(FP),
    [X] = [X || {X, Y} <- FPs, binary:part(Y, {0, Len}) == FP ],
    X.
