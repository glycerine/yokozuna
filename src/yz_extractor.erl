%% -------------------------------------------------------------------
%%
%% Copyright (c) 2012 Basho Technologies, Inc.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

-module(yz_extractor).
-compile(export_all).
-include("yokozuna.hrl").

-type mime_type() :: binary() | default.
-type extractor_def() :: module() | {module(), proplist()}.
-type extractor_map() :: orddict(mime_type(), extractor_def()).

-define(DEFAULT_MAP, [{default, yz_text_extractor},
                      {<<"text/plain">>, yz_text_extractor},
                      {<<"application/xml">>, yz_xml_extractor},
                      {<<"text/xml">>, yz_xml_extractor}]).
-define(META_EXTRACTOR_MAP, yokozuna_extractor_map).

%% @doc Get the extractor definition for the given mime type.  Throw
%%      an error if no entry can be found.
-spec get_def(mime_type()) -> extractor_def().
get_def(MimeType) ->
    Map = get_map(),
    case orddict:find(MimeType, Map) of
        {ok, {_, _}=ModuleOpts} -> ModuleOpts;
        {ok, Module} -> Module;
        error ->
            throw({no_extractor_def, MimeType, Map})
    end.

-spec get_map() -> extractor_map().
get_map() ->
    {ok, Ring} = riak_core_ring_manager:get_my_ring(),
    case riak_core_ring:get_meta(?META_EXTRACTOR_MAP, Ring) of
        {ok, Map} -> Map;
        undefined -> ?DEFAULT_MAP
    end.
