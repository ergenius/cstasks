%% This is an exercise to create a uuid.
%% The uuid is based on:
%% - local time in microseconds starting from "CraftingSoftware epoch"
%% - some random bytes
%% - a namespace (phash2 of the erlang node atom)
%%
%% When I wanted to insert the tasks job in database I said to myself let's use an uuid instead
%% of that sqlite integer auto incremented primary key and let's have fun and 
%% create our own uuid instead of just using a 3rd party uuid erlang library.
-module(cs_uuid).
-author("Madalin Grigore-Enescu").

-include("cstasks.hrl").

%% Microseconds coresponding to the "CraftingSoftware epoch"
%% wich for me starts now... on 2024-10-05T13:29:38+00:00 :)
%% This allow us to generate shorter time based uuids.
-define(CS_TIME_CS_EPOCH_MICROSECONDS, 1728134978000000).
-define(CS_UUID_NAMESPACE_MAX_EXCLUSIVE, 65536).

-export([new/0]).

%% @doc Creates a new binary Uuid
-spec new() -> Uuid when
    Uuid :: binary().
new() ->
    new(
        erlang:system_time(microsecond),
        binary:encode_unsigned(erlang:phash2(erlang:node(), ?CS_UUID_NAMESPACE_MAX_EXCLUSIVE))
    ).

-spec new(Time, Namespace) -> Uuid when
    Time :: integer(),
    Namespace :: binary(),
    Uuid :: binary().
new(Time, Namespace) ->
    %% 315360000000000 microseconds - 7 bytes in 10 years
    %% Reduce the length of any time uuid by using our own epoch wich starts later than UNIX epoch
    MicrosecondsBinary = binary:encode_unsigned(Time - ?CS_TIME_CS_EPOCH_MICROSECONDS, big),
    NamespaceWithPadding = pseudo_padding(Namespace, 8, left),
    %% Finaly starting with 24 we have hex encoding functions :)
    binary:encode_hex(<<MicrosecondsBinary/binary, NamespaceWithPadding/binary>>, lowercase).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% random
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Read a specific number of random bytes from /dev/random
%% It blocks (pauses) when there is insufficient entropy in the system's entropy pool.
%% If we request more random data than is available from the entropy pool,
%% /dev/random will wait until enough entropy is gathered.
%% We can use /dev/random when we require high-quality entropy,
%% and we are willing to wait for the system to gather enough randomness.
crypto_bytes_dev_random(N) ->
    case file:open("/dev/random", [read, raw, binary]) of
        {ok, File} ->
            case file:read(File, N) of
                {ok, Data} ->
                    file:close(File),
                    Data;
                Error ->
                    file:close(File),
                    ?CS_LOG_ERROR(#{m => read_dev_random_error, error => Error}),
                    erlang:error(read_dev_random_error)
            end;
        Error ->
            ?CS_LOG_ERROR(#{m => open_dev_random_error, error => Error}),
            erlang:error(open_dev_random_error)
    end.

%% @doc Returns a default seed for the pseudo random generator
pseudo_default_seed() ->
    try
        <<I1:32/unsigned-integer, I2:32/unsigned-integer, I3:32/unsigned-integer>> = crypto_bytes_dev_random(
            12
        ),
        {I1, I2, I3}
    catch
        _ ->
            %% If we fail create a less unpredictable random seed
            {
                erlang:phash2([{node(), self()}]),
                erlang:system_time(),
                erlang:unique_integer()
            }
    end.

%% @doc Automatically seed the pseudo number generator
pseudo_seed() ->
    rand:seed(exsss, pseudo_default_seed()),
    put(cs_ransom_seed_time, erlang:system_time()).

%% @doc Generate random bytes
-spec pseudo_bytes(N :: non_neg_integer()) -> Bytes :: binary().
pseudo_bytes(N) ->
    case get(cs_ransom_seed_time) of
        undefined -> pseudo_seed();
        _ -> ok
    end,
    rand:bytes(N).

%% @doc Add random padding to the specfied input or cut the input to the specified length
-spec pseudo_padding(Input, Length, Position) -> Result when
    Input :: binary(),
    Length :: non_neg_integer(),
    Position :: left | right,
    Result :: binary().
pseudo_padding(Input, Length, Position) when
    erlang:is_binary(Input), erlang:is_integer(Length), Length >= 0
->
    pseudo_padding(Input, erlang:byte_size(Input), Length, Position).

pseudo_padding(Input, Length, Length, _Position) ->
    Input;
pseudo_padding(Input, InputLength, Length, Position) when InputLength < Length ->
    Padding = pseudo_bytes(Length - InputLength),
    case Position of
        left -> <<Padding/binary, Input/binary>>;
        right -> <<Input/binary, Padding/binary>>
    end;
pseudo_padding(Input, InputLength, Length, Position) when InputLength > Length ->
    binary_cut(Input, Length, Position).

%% @doc Cut a binary to the specified size from either the left or right
binary_cut(Bin, ByteSize, Position) when
    is_binary(Bin),
    is_integer(ByteSize),
    ByteSize >= 0
->
    binary_cut(Bin, erlang:byte_size(Bin), ByteSize, Position).
binary_cut(Bin, BinSize, MaxSize, _Position) when BinSize =< MaxSize -> Bin;
binary_cut(Bin, _BinSize, MaxSize, left) ->
    <<TrimmedBin:MaxSize/binary, _/binary>> = Bin,
    TrimmedBin;
binary_cut(Bin, BinSize, MaxSize, right) ->
    SizeToTrim = BinSize - MaxSize,
    <<_:SizeToTrim/binary, TrimmedBin:MaxSize/binary>> = Bin,
    TrimmedBin.
