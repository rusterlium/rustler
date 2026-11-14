-module(rustler).

-export([
    get_path/2,
    get_signatures/2,
    signature_path/1,
    write_signatures/2,
    read_signatures/1
]).

-define(CHUNK_SIZE, 65536). % 64 KB chunks

get_path(Application, Crate) ->
    filename:join([
        code:priv_dir(Application),
        binary:join([Crate, platform_ext()], <<".">>)
    ]).

get_signatures(Application, Crate) ->
    Path = get_path(Application, Crate),
    extract_signatures(Path, 1).

signature_path(LibraryPath) ->
    RootPath = filename:rootname(LibraryPath, filename:extension(LibraryPath)),
    <<RootPath/binary, ".signature-v1.txt">>.

write_signatures(LibraryPath, SignaturePath) ->
    case extract_signatures(LibraryPath, 1) of
        Signatures when is_list(Signatures) ->
            Lines = [signature_line(Signature) || Signature <- Signatures],
            file:write_file(SignaturePath, Lines);
        {error, Reason} ->
            {error, Reason}
    end.

read_signatures(SignaturePath) ->
    case file:read_file(SignaturePath) of
        {ok, Content} ->
            {ok, [parse_signature_line(Line) ||
                Line <- binary:split(Content, <<"\n">>, [global]),
                Line =/= <<>>]};
        {error, Reason} ->
            {error, Reason}
    end.

signature_line(Signature) ->
    {Name, Arity} = parse_signature_line(Signature),
    [Name, "/", integer_to_binary(Arity), "\n"].

parse_signature_line(Signature) ->
    [ArityBin | ReversedName] =
        lists:reverse(binary:split(Signature, <<"/">>, [global])),
    {iolist_to_binary(lists:join(<<"/">>, lists:reverse(ReversedName))),
        binary_to_integer(ArityBin)}.

platform_ext() ->
    platform_ext(os:type()).

platform_ext(Platform) ->
    case Platform of
        {unix, _} -> <<".so">>;
        {win32, _} -> <<".dll">>
    end.

%% Public API: Pass the target version (e.g., 1)
extract_signatures(FilePath, Version) when is_integer(Version) ->
    VBin = integer_to_binary(Version),
    StartTag = iolist_to_binary([<<"<?RUSTLER-NIF-SIG-V">>, VBin, <<">">>]),
    EndTag = iolist_to_binary([<<"</?RUSTLER-NIF-SIG-V">>, VBin, <<">">>]),

    case file:open(FilePath, [read, binary, raw, {read_ahead, ?CHUNK_SIZE}]) of
        {ok, IoDevice} ->
            try
                read_chunks(IoDevice, StartTag, EndTag, <<>>, [])
            after
                file:close(IoDevice)
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% Internal: Stream chunks through file:read/2
read_chunks(IoDevice, StartTag, EndTag, AccBin, Signatures) ->
    case file:read(IoDevice, ?CHUNK_SIZE) of
        {ok, Data} ->
            {NewAcc, NewSigs} = parse_binary(<<AccBin/binary, Data/binary>>, StartTag, EndTag, Signatures),
            read_chunks(IoDevice, StartTag, EndTag, NewAcc, NewSigs);
        eof ->
            {_, FinalSigs} = parse_binary(AccBin, StartTag, EndTag, Signatures),
            lists:reverse(FinalSigs);
        {error, Reason} ->
            {error, Reason}
    end.

%% Internal: Dynamic matching with memory cleanup & boundary protection
parse_binary(Bin, StartTag, EndTag, Signatures) ->
    case binary:match(Bin, StartTag) of
        {StartPos, StartLen} ->
            <<_:StartPos/binary, _:StartLen/binary, Rest/binary>> = Bin,
            case binary:match(Rest, EndTag) of
                {EndPos, EndTagLen} ->
                    <<Sig:EndPos/binary, _Rest2/binary>> = Rest,
                    <<_:EndPos/binary, _:EndTagLen/binary, Remaining/binary>> = Rest,

                    % Copy signature to local heap, releasing the massive chunk buffer
                    CopiedSig = binary:copy(Sig),
                    parse_binary(Remaining, StartTag, EndTag, [CopiedSig | Signatures]);
                nomatch ->
                    % End tag is cut off by chunk boundary. Keep from StartTag onwards.
                    <<_:StartPos/binary, KeepBin/binary>> = Bin,
                    {KeepBin, Signatures}
            end;
        nomatch ->
            % No start tag found. Keep only the trailing tail (TagLen - 1 bytes) 
            % in case a start tag was split right across the chunk boundary.
            TagLen = byte_size(StartTag),
            BinLen = byte_size(Bin),
            if 
                BinLen >= TagLen ->
                    KeepLen = TagLen - 1,
                    SkipLen = BinLen - KeepLen,
                    <<_:SkipLen/binary, KeepBin/binary>> = Bin,
                    {KeepBin, Signatures};
                true ->
                    {Bin, Signatures}
            end
    end.
