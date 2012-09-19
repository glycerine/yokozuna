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

-module(yz_xml_extractor).
-compile(export_all).
-record(state, {
          name_stack = [],
          fields = [],
          field_separator = <<"_">>
         }).
-define(NO_OPTIONS, []).

extract(Values) ->
    extract(Values, ?NO_OPTIONS).

extract(Values, _Opts) ->
    try
        %% TODO: What to do for multiple values of the same field?
        %% Perhaps merge them?  What does it mean to index an object
        %% with siblings?
        lists:append([extract_value(V) || V <- Values])
    catch _:Err ->
            Trace = erlang:get_stacktrace(),
            {error, {cannot_parse_xml, Err, Trace}}
    end.

extract_value(Data) ->
    Options = [{event_fun, fun sax_cb/3}, {event_state, #state{}}],
    %% TODO: warn if there is leftover data?
    {ok, State, _Rest} = xmerl_sax_parser:stream(Data, Options),
    State#state.fields.

%% @private
%% xmerl_sax_parser callback to parse an XML document into search
%% fields.
sax_cb({startElement, _Uri, Name, _QualName, Attrs}, _Location, S) ->
    NewNameStack = [Name | S#state.name_stack],
    Separator = S#state.field_separator,
    AttrFields = case Attrs of
                     [] ->
                         [];
                     _ ->
                         make_attr_fields(make_name(Separator, NewNameStack),
                                          Attrs, [])
                 end,
    S#state{name_stack=NewNameStack,
            fields=AttrFields ++ S#state.fields};

%% End of an element, collapse it into the previous item on the stack...
sax_cb({endElement, _Uri, _Name, _QualName}, _Location, S) ->
    S#state{name_stack=tl(S#state.name_stack)};

%% Got a value, set it to the value of the topmost element in the stack...
sax_cb({characters, Value}, _Location, S) ->
    Name = make_name(S#state.field_separator, S#state.name_stack),
    Field = {Name, list_to_binary(Value)},
    S#state{fields = [Field|S#state.fields]};

sax_cb(_Event, _Location, State) ->
    State.

-spec make_attr_fields(binary(), list(), list()) -> [{binary(), binary()}].
make_attr_fields(_BaseName, [], Fields) ->
    Fields;
make_attr_fields(BaseName, [{_Uri, _Prefix, AttrName, Value}|Attrs], Fields) ->
    AttrNameB = list_to_binary(AttrName),
    FieldName = <<BaseName/binary,$@,AttrNameB/binary>>,
    Field = {FieldName, Value},
    make_attr_fields(BaseName, Attrs, [Field | Fields]).

make_name(Seperator, Stack) ->
    make_name(Seperator, Stack, <<>>).

%% Make a name from a stack of visted tags (innermost tag at head of list)
-spec make_name(binary(), [string()], binary()) -> binary().
make_name(Seperator, [Inner,Outter|Rest], Name) ->
    OutterB = list_to_binary(Outter),
    InnerB = list_to_binary(Inner),
    Name2 = <<OutterB/binary,Seperator/binary,InnerB/binary,Name/binary>>,
    make_name(Seperator, Rest, Name2);
make_name(Seperator, [Outter], Name) ->
    OutterB = list_to_binary(Outter),
    case Name of
        <<>> -> OutterB;
        _ -> <<OutterB/binary,Seperator/binary,Name/binary>>
    end;
make_name(_, [], Name) ->
    Name.
