%% @doc Help functions.
%% @private
-module(edocmermaid).

-export([get_mermaid_url/3]).

get_mermaid_url(Key, Default, Opts) ->
    case proplists:get_value(Key, Opts, Default) of
        {file, M} ->
            M;
        M when is_list(M) ->
            M;
        Other ->
            edoc_report:report("bad value for option '~w': ~tP.", [Key, Other, 10]),
            exit(error)
    end.
