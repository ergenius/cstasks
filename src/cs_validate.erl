%% Some paranoic validation done on various binaries comming from http requests.
%% "Features":
%% - No conversion to lists is done. 
%% - Appending data to a binary is efficient because it is specially optimized by the 
%% runtime system to avoid copying the binary every time.
-module(cs_validate).

-include("cstasks.hrl").

-export([safe_http_www_path/1]).
-export([safe_sub_domain_name/1]).
-export([safe_domain_extension/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% safe_http_www_path
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec safe_http_www_path(Path) -> {ok, SanitizedPath, Extension, LastCharacter} | {error, Reason} when
	Path :: binary(),
	SanitizedPath :: binary(),
    Extension :: undefined | binary(),
    LastCharacter :: undefined | integer(),
	Reason :: atom().

%% @doc Check and sanitize the specified request path.
%% Returns the lower case path, the extension if any and the last character of the path.
%% Those are very usefull to the caller when resolving the path.
%% It is interesting that i couldn't find any scenario when a path 
%% using other validation rules is nedeed except in well known exploits
%% and crappy php. With those simple rules we can quickly filter out a huge ammount of
%% exploits scans.
safe_http_www_path(Path) when Path =:= <<>>; Path =:= <<$/>> ->
    {ok, <<"">>, undefined, undefined};
safe_http_www_path(<<$/, Rest/binary>>) ->
    safe_http_www_path(Rest);
safe_http_www_path(Path) when erlang:is_binary(Path) -> safe_http_www_path(Path, undefined, <<>>, undefined).

%% valid: . after and before any alpha numeric characters
safe_http_www_path(<<$., After, Rest/binary>>, $a, Acum, _ExtAcum) when
    After >= $a, After =< $z;
    After >= $0, After =< $9;
    After >= $A, After =< $Z
->
    LowerAfter = char_to_lowercase(After),
    safe_http_www_path(Rest, $a, <<Acum/binary, $., LowerAfter>>, <<LowerAfter>>);

%% valid: / at end after alpha numeric
safe_http_www_path(<<$/>>, $a, Acum, _ExtAcum) ->
    {ok, <<$/, Acum/binary>>, undefined, $/};
%% valid: / after and before any alpha numeric characters
safe_http_www_path(<<$/, After, Rest/binary>>, $a, Acum, _ExtAcum) when
    After >= $a, After =< $z;
    After >= $0, After =< $9;
    After >= $A, After =< $Z
->
    LowerAfter = char_to_lowercase(After),
    safe_http_www_path(Rest, $a, <<Acum/binary, $/, LowerAfter>>, undefined);

%% valid: - after and before any alpha numeric characters
safe_http_www_path(<<$-, After, Rest/binary>>, $a, Acum, _ExtAcum) when
    After >= $a, After =< $z;
    After >= $0, After =< $9;
    After >= $A, After =< $Z
->
    LowerAfter = char_to_lowercase(After),
    safe_http_www_path(Rest, $a, <<Acum/binary, $-, LowerAfter>>, undefined);

%% valid: _ after and before any alpha numeric characters
safe_http_www_path(<<$_, After, Rest/binary>>, $a, Acum, _ExtAcum) when
    After >= $a, After =< $z;
    After >= $0, After =< $9;
    After >= $A, After =< $Z
->
    LowerAfter = char_to_lowercase(After),
    safe_http_www_path(Rest, $a, <<Acum/binary, $_, LowerAfter>>, undefined);

%% valid: alpha & numeric characters
safe_http_www_path(<<H, Rest/binary>>, _Before, Acum, ExtAcum) when
    H >= $a, H =< $z;
    H >= $0, H =< $9;
    H >= $A, H =< $Z
->
    LowerH = char_to_lowercase(H),
    NewExtAcum = safe_http_www_path_extension(LowerH, ExtAcum),
    safe_http_www_path(Rest, $a, <<Acum/binary, LowerH>>, NewExtAcum);
%% ERROR: anything else remaining
safe_http_www_path(<<_, _Rest/binary>>, _Before, _Acum, _ExtAcum) ->
    {error, invalid_character};
%% valid: end of path
safe_http_www_path(<<>>, Before, Acum, ExtAcum) ->
    {ok, Acum, ExtAcum, Before}.

safe_http_www_path_extension(_Char, undefined) -> undefined;
safe_http_www_path_extension(Char, Ext) when erlang:is_binary(Ext) -> <<Ext/binary, Char>>.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% safe_domain_extension
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Check domain extension
%% Extension must contain at least one alpha numeric character.
%% Characters are converted from upper case to lower case in the same iteration
%% and sanitized binary is returned.
safe_domain_extension(<<>>) -> {error, invalid_extension};
safe_domain_extension(Ext) -> safe_domain_extension_iterate(Ext, <<>>).
%% valid: end of extension
safe_domain_extension_iterate(<<>>, Acum) ->
    {ok, Acum};
%% valid: alpha & numeric characters
safe_domain_extension_iterate(<<H, Rest/binary>>, Acum) when
    H >= $a, H =< $z;
    H >= $0, H =< $9;
    H >= $A, H =< $Z
->
    %% Appending data to a binary is efficient because it is specially optimized
    %% by the runtime system to avoid copying the Acum binary every time.
    H1 = char_to_lowercase(H),
    safe_domain_extension_iterate(Rest, <<Acum/binary, H1>>);
%% ERROR: anything else
safe_domain_extension_iterate(<<_, _Rest/binary>>, _Acum) ->
    {error, invalid_domain_extension}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% safe_sub_domain_name
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Check the specified domain name for injection
%% So called "safe domain names" must contain only ASCII alpha-numeric characters, -, and one .
%% before the alphanumeric extension.
%%
%% A safe domain name example: sub-domain.domain-name.extension
%%
%% Invalid domain names:
%% name
%% invalid-name
%% -invalid-name
%% invalid--name
%% invalid-name.
%% invalid..extension
%% .invalid
safe_sub_domain_name(<<>>) ->
    {error, empty};
safe_sub_domain_name(Hostname) when erlang:is_binary(Hostname) ->
    safe_sub_domain_name(Hostname, undefined, <<>>);
safe_sub_domain_name(Hostname) when erlang:is_list(Hostname) ->
    safe_sub_domain_name(erlang:list_to_binary(Hostname)).

%% valid: . after and before any alpha numeric characters
safe_sub_domain_name(<<$., After, Rest/binary>>, $a, Acum) when
    After >= $a, After =< $z;
    After >= $0, After =< $9;
    After >= $A, After =< $Z
->
    Acum1 = <<Acum/binary, $.>>,
    After1 = char_to_lowercase(After),
    safe_sub_domain_name(Rest, $., <<Acum1/binary, After1>>);
%% ERROR: . after or before invalid character such as ., - or _
safe_sub_domain_name(<<$., _Rest/binary>>, _Before, _Acum) ->
    {error, invalid_dot};
%% valid: - after and before any alpha numeric characters
safe_sub_domain_name(<<$-, After, Rest/binary>>, $a, Acum) when
    After >= $a, After =< $z;
    After >= $0, After =< $9;
    After >= $A, After =< $Z
->
    Acum1 = <<Acum/binary, $->>,
    After1 = char_to_lowercase(After),
    safe_sub_domain_name(Rest, $-, <<Acum1/binary, After1>>);
%% ERROR: - after or before invalid character such as ., - or _
safe_sub_domain_name(<<$-, _Rest/binary>>, _Before, _Acum) ->
    {error, invalid_dash};
%% valid: alpha & numeric characters
safe_sub_domain_name(<<H, Rest/binary>>, _Before, Acum) when
    H >= $a, H =< $z;
    H >= $0, H =< $9;
    H >= $A, H =< $Z
->
    H1 = char_to_lowercase(H),
    safe_sub_domain_name(Rest, $a, <<Acum/binary, H1>>);
%% valid: ending up in alpha numeric character
safe_sub_domain_name(<<>>, $a, Acum) ->
    {ok, Acum};
%% ERROR: anything else
safe_sub_domain_name(<<_, _Rest/binary>>, _Before, _Acum) ->
    {error, invalid_character}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% char_
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
char_to_lowercase(Char) when erlang:is_integer(Char), Char >= $A, Char =< $Z ->
    Char + 32;
char_to_lowercase(Char) when erlang:is_integer(Char) -> Char.