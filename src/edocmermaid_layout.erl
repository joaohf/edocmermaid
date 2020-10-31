%% @doc Patches the output of
%% <a href="http://erlang.org/doc/man/edoc_layout.html">edoc_layout</a>.
%%
%% This module append the reference and configuration for mermaid
%% javascript. For each html file created by <a href="http://erlang.org/doc/man/edoc_layout.html#module-2">
%% edoc_layout:module/2</a> and <a href="http://erlang.org/doc/man/edoc_layout.html#overview-2">edoc_layout:overview/2</a>,
%% this module will append the following html snippet:
%%
%% ```
%% <script src="https://cdn.jsdelivr.net/npm/mermaid/dist/mermaid.min.js"></script>
%% <script>mermaid.initialize({startOnLoad:true});</script>
%% '''
-module(edocmermaid_layout).

-export([module/2, overview/2, type/1]).

-include("edocmermaid.hrl").

%% @doc Calls edoc_layout:module/2 and append mermaid javascript snippet.
%% Called from edoc_doclet module.
module(Element, Options) ->
    Url = edocmermaid:get_mermaid_url(?MERMAID_URL_KEY, {file, ?MERMAID_JS}, Options),

    SimpleXml = edoc_layout:module(Element, Options),

    patch_layout(Url, SimpleXml).

type(E) ->
    edoc_layout:type(E).

%% @doc Calls edoc_layout:overview/2 and append mermaid javascript snippet.
%% Called from edoc_doclet module.
overview(Element, Options) ->
    Url = edocmermaid:get_mermaid_url(?MERMAID_URL_KEY, {file, ?MERMAID_JS}, Options),
    X = edoc_layout:overview(Element, Options),
    patch_layout(Url, X).

%% @doc Unwrap, patch and wrap the HTML as io list.
%% edoc_layout is the module responsible for creating HTML files based on XML description.
%% patch_layout finds the body html element and append the mermaid snippet. Then
%% the HTML list is created again.
patch_layout(MermaidUrl, Term) ->
    Body0 = unwrap(Term) ++ source_mermaid(MermaidUrl),
    wrap(Term, Body0).

unwrap([_DOCTYPE, _W3C, _, _, _, [[_HTML0, [_, _Head, _, [_, Body, _], _], _HTML1], _]]) ->
    Body.

wrap(
    [_DOCTYPE, _W3C, _3, _4, _5, [[_HTML0, [_6, _Head, _7, [_8, _, _9], _10], _HTML1], _11]],
    Body
) ->
    [_DOCTYPE, _W3C, _3, _4, _5, [[_HTML0, [_6, _Head, _7, [_8, Body, _9], _10], _HTML1], _11]].

source_mermaid(MermaidUrl) ->
    SourceJs =
        case MermaidUrl of
            {file, File} ->
                File;
            Url ->
                Url
        end,

    [
        "<script src=\"",
        SourceJs,
        "\"></script>",
        "\n",
        "<script>mermaid.initialize({startOnLoad:true});</script>",
        "\n"
    ].
