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

-module(yz_riakdoc_extractor).
-compile(export_all).
-include("yokozuna.hrl").
-export([extract/2,
	     extract_value/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% @doc Extracts middleman frontmatter into Solr index.

-type opts() :: proplists:proplist().
% -type field() :: binary().
% -type value() :: binary().
% -type obj() :: riak_object:riak_object().

-spec extract(riak_object:riak_object(), opts()) -> [{field(), value()}] | {error, any()}.
extract(RiakObject, DefaultField) ->
    try
        Values = riak_object:get_values(RiakObject),
        lists:flatten([extract_value(V, DefaultField) || V <- Values])
    catch
        _:Err ->
            {fail, {cannot_parse_yaml,Err}}
    end.

% extract_value(Data, DefaultField) ->
%     [{DefaultField, Data}].
extract_value(Data, DefaultField) ->
    [[[], Header]|_] = re:split(Data3, "^\-{3}$(.+?)^\-{3}$", [{return,list},group,dotall,multiline]),
    Lines = string:tokens(Header, "\n"),
    Yaml = parse(Lines),
    % Json = mochijson2:decode(Data),
    Fields = lists:reverse(lists:flatten(make_search_fields(undefined, Yaml, DefaultField, []))),
    [{to_utf8(FieldName), to_utf8(FieldValue)} || {FieldName, FieldValue} <- Fields].


% better to use this:
% -include("riak_search.hrl").
% -import(riak_search_utils, [to_utf8/1]).
to_utf8(A) when is_atom(A) -> atom_to_binary(A, utf8);
to_utf8(B) when is_binary(B) -> B;
to_utf8(I) when is_integer(I) -> to_utf8(integer_to_list(I));
to_utf8(F) when is_float(F) -> to_utf8(mochinum:digits(F));
to_utf8(L) when is_list(L) -> unicode:characters_to_binary(L).


make_search_fields(NamePrefix, {struct, Fields}, DefaultField, Output) ->
    F = fun({Name, Val}, Acc) ->
                Name2 = append_fieldname(NamePrefix, Name),
                [make_search_fields(Name2, Val, DefaultField, []) | Acc]
        end,
    lists:foldl(F, Output, Fields);
make_search_fields(Name, List, DefaultField, Output) when is_list(List) ->
    F = fun(El, Acc) ->
                [make_search_fields(Name, El, DefaultField, []) | Acc]
        end,
    lists:foldl(F, Output, List);
make_search_fields(_Name, null, _DefaultField, Output) ->
    Output;
make_search_fields(Name, Term, DefaultField, Output) ->
    [{search_fieldname(Name, DefaultField), to_data(Term)} | Output].


to_data(String) when is_binary(String) ->
    String;
to_data(Atom) when is_atom(Atom) ->
    atom_to_binary(Atom, utf8);
to_data(Term) ->
    iolist_to_binary(io_lib:format("~p", [Term])).


append_fieldname(undefined, Name) ->
    binary_to_list(Name);
append_fieldname(FieldPrefix, Name) ->
    FieldPrefix ++ [$_ | binary_to_list(Name)].
    
%% Make a search field name - if no names encountered yet use the
%% default field, otherwise make sure the field name does not
%% contain . or : by substituting with _
search_fieldname(undefined, DefaultField) ->
    DefaultField;
search_fieldname(Name, _) ->
    riak_search_kv_extractor:clean_name(Name).


% parse a yaml file, represented as a list of lines by Oliver Mason.
% returns: a data structure as stored in the yaml file.
% version 0.1
% - able to parse basic lists and mappings, but not yet
%   shortcut expressions.


parse([]) -> [];
parse(Lines) ->
	Indented = indent_transform(Lines,[]),
	{Tree, []} = parse_tree(0,Indented,[]),
	parse(Tree,[]).



parse_tree(_I, [], Acc) -> {lists:reverse(Acc), []};
parse_tree(Indent, [{Indent, Line}|Rest], Acc) ->
	parse_tree(Indent, Rest, [Line|Acc]);
parse_tree(Indent, [{Ind,Line}|Rest], Acc) when Ind > Indent ->
	{Tree, Remains} = parse_tree(Ind, Rest, [Line]),
	parse_tree(Indent, Remains, [Tree|Acc]);
parse_tree(Indent, [{Ind,Line}|Rest], Acc) when Ind < Indent ->
	{lists:reverse(Acc), [{Ind, Line}|Rest]}.

parse([], Acc) -> lists:reverse(Acc);
parse([Node|Rest], Acc) ->
	case Node of
		[$-] ->	
			[Block|Remains] = Rest,
			parse(Remains,[parse(Block,[])|Acc]);
		[$-,32|Line] ->
			parse(Rest,[Line|Acc]);
		_ ->
			case map_split(Node,[],[]) of
				{Key, Value} -> 
					parse(Rest,[{Key, Value}|Acc]);
				{Key} ->
					[Block|Remains] = Rest,
					parse(Remains,[{Key,parse(Block,[])}|Acc])
			end
	end.

map_split([], Key, []) -> {lists:reverse(Key)};
map_split([], Key, Value) -> {Key, Value};
map_split([$:,32|Value], Key, []) -> {lists:reverse(Key), Value};
map_split([$:|Rest], Key, []) -> map_split(Rest, Key, []);
map_split([Char|Rest], Key, []) -> map_split(Rest, [Char|Key],[]).

%
% count and remove leading spaces from lines.
% each line is replaced by a tuple of the number of spaces
% and the rest of the line
%
indent_transform([],Acc) ->
	lists:reverse(Acc);
indent_transform([Line|Rest],Acc) ->
	NewLine = indent_transform_line(Line,0),
	indent_transform(Rest,[NewLine|Acc]).

indent_transform_line([32|Rest],Count) ->
	indent_transform_line(Rest,Count+1);
indent_transform_line(Line, Count) ->
	{Count, Line}.



-ifdef(TEST).

bad_yaml_test() ->
    Data = <<"<?xml><surprise>this is not yaml</surprise>">>, 
    Object = riak_object:new(<<"b">>, <<"k">>, Data, "text/yaml"),
    ?assertMatch({fail, {cannot_parse_yaml,_}}, extract(Object, <<"value">>)).

-endif. % TEST
