%% This is nothing else but a wrapper around jiffy.
%% I added it to the solution to be able to easily change jiffy options
-module(cs_json).
-author("Madalin Grigore-Enescu").

-include("cstasks.hrl").

-export([encode/1]).
-export([decode/1]).

%% @doc Convert the specfieid map to a JSON binary
-spec encode(Decoded :: map()) -> Encoded :: binary().
encode(Decoded) when is_map(Decoded) -> 
    erlang:iolist_to_binary(jiffy:encode(Decoded, [pretty, force_utf8, uescape])).

%% @doc Decode the specified JSON binary to a map
-spec decode(Encoded :: binary()) -> Decoded :: map().
decode(Encoded) when is_binary(Encoded) -> 
    jiffy:decode(Encoded, [return_maps, {null_term, null}, dedupe_keys, copy_strings]).