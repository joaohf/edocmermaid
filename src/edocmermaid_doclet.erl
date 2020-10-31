%% @doc Copy mermaid js file to the correct place.
%%
%% This module will call {@link edoc_doclet} and copy the js file (if the option
%% `mermaid_url' is not passed) to the output document directory.
%% If the option mermaid_url is passed, then no files will be copied.

-module(edocmermaid_doclet).

-export([run/2]).

-include_lib("edoc/include/edoc_doclet.hrl").

-include("edocmermaid.hrl").

%% @doc Entry point of doclet. Copy mermaid javascript file to the target application.
run(#doclet_gen{} = Cmd, #context{opts = Opts} = Ctxt) ->
    ok = edoc_doclet:run(Cmd, Ctxt),

    Url = edocmermaid:get_mermaid_url(mermaid_url, {file, ?MERMAID_JS}, Opts),
    maybe_copy_mermaid_js(Url, Ctxt),
    ok.

%% @doc Copy mermaid js file to `Dst'
maybe_copy_mermaid_js(?DEFAULT_MERMAID_JS, #context{dir = Dst}) ->
    Priv = code:priv_dir(edocmermaid),
    MermaidDst = filename:join([Dst, ?MERMAID_JS]),
    MermaidFile = filename:join([Priv, ?MERMAID_JS]),
    ok = filelib:ensure_dir(Dst),
    {ok, _} = file:copy(MermaidFile, MermaidDst),
    ok;
maybe_copy_mermaid_js(_, _) ->
    ok.
