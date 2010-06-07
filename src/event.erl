-module (event).
-compile(export_all).
-include_lib("nitrogen/include/wf.hrl").

main() -> #template { file="./site/templates/bare.html" }.

title() -> "dayfindr.com".

layout() ->
    #container_12 { 
	body=[
	      #grid_6 {alpha=true, prefix=3, suffix=3, omega=true, body=description()},
	      #grid_8 {alpha=true, prefix=2, suffix=2, omega=true, body=participants_panel()},
	      #grid_8 {alpha=true, prefix=2, suffix=2, omega=true, body=body()},
	      #grid_6 {alpha=true, prefix=3, suffix=3, omega=true, body=comments()}
	     ]}.

description() ->
    Client = riak_client(),
    {ok, O1} = Client:get(bucket(), <<"_description">>, 1),
    {ok, O2} = Client:get(bucket(), <<"_organiser_email">>, 1),
    Description = riak_object:get_value(O1),
    OrganiserEmail = riak_object:get_value(O2),
    [
     #panel {class="event-details",
	     body=
	     [
	      #panel{ class="description", body=
		      #span{text=Description, html_encode=true}},
	      #panel{ class="organiser", body="organised by " ++ OrganiserEmail}
	     ]}
    ].

participants_panel() ->
    #panel {id=participantPanel,
	    body=participant_body()}.

participant_body() ->
    participant_body(undefined).

participant_body(SelectedName) ->
    Participants = participants(),
    wf:wire(addButton, newParticipant, 
	    #validate { validators=[
				    #is_required { text="Please add your name" },
				    #custom { text="Please choose a unique name", function=fun validate_unique/2 },
				    #custom { text="Tags ar not allowed", function=fun validate_tags/2 }
				   ]}),
    [
     #grid_4 {alpha=true, 
	      body=
	      [
	       #h3 {text="My name is..."},
	       #radiogroup { body=
			     %% Need to flatten the list for the correct radio grouping
			     lists:flatten(
			       lists:map(
				 fun({Id, Name}) ->
					 ElementId = list_to_atom("participat_" ++ integer_to_list(Id)),
					 [
					  #radio { id=ElementId, text=Name, 
						   value=io_lib:format("~p", [Id]), 
						   checked=SelectedName =:= Name,
						   postback={user_selected, Id, Name}}, 
					  #br{}
					 ]
				 end,
				 Participants))}
	      ]},
     #grid_4 {omega=true,
	      body=
	      [
	       #h3{text="not on the list yet:"},
	       #panel {body=
		       [
			#label { text="" },
			#textbox { id=newParticipant, text="", next=addButton },
			#br{},
			#button { id=addButton, text="Add my name", postback=add_user }
		       ]}
	      ]}
    ].

participants() ->
    %% The participants are the keys in the bucket, but
    %% disregarding any metadata values that start with "_"
    Client = riak_client(),
    {ok, Keys} = Client:list_keys(bucket()),
    StringKeys = lists:map(fun binary_to_list/1, Keys),
    Filtered = [X || X <- StringKeys, hd(X) /= $_],
    Participants = to_participants(Filtered),
    wf:state(participants, Participants),
    Participants.

to_participants(Keys) ->
    to_participants(Keys, [], 1).

to_participants([Hd|Rest], Participants, Index) ->
    to_participants(Rest, [{Index, Hd}|Participants], Index + 1);
to_participants([], Participants, _) ->
    Participants.


body() -> 
    wf:wire(#api { name=save }),
    [
     #panel{id="calendar_container", body="placeholder"}
    ].

comments() ->
    #panel {class="comments",
	    body=[
		  #h3 {text="Comments"},
		  #panel {id=existingComments,
			  body=existing_comments()},
		  #panel {id=addCommentPanel, 
			  body=add_comment_panel()
			 }
		 ]}.

existing_comments() ->
    Client = riak_client(),
    ExistingComments = case Client:get(bucket(), <<"_comments">>, 1) of
			   {ok, O1} ->
			       mochijson2:decode(riak_object:get_value(O1));
			   _ -> 
			       []
		       end,
    lists:map(fun render_comment/1, ExistingComments).

add_comment_panel() ->
    wf:wire(addCommentButton, commentArea, 
     	    #validate { validators=
     			[
     			 #is_required { text="Empty comments are not allowed" }, 
     			 #custom { text="Tags are not allowed", function=fun validate_tags/2 },
			 #custom { text="Please choose your name at the top", function=fun validate_user_selected/2 }
     			]}),
    [
     #label {text="Your comment:"},
     #textarea {class="commentArea", id=commentArea},
     #br{},
     #button {id=addCommentButton, text="Create comment", postback=create_comment }
    ].
    
%% Stored in the form
%% {Author, Timestamp, Text}
render_comment({struct, Props}) ->
    Author = proplists:get_value(<<"name">>, Props),
    Timestamp = list_to_tuple(proplists:get_value(<<"timestamp">>, Props)),
    CommentText = proplists:get_value(<<"commentText">>, Props),
    #panel {class="comment",
	    body=
	    [
	     #panel{class="author", body=Author},

	     #panel{body=#span{text=CommentText, html_encode=true}},
	     #panel{class="timeago", body=httpd_util:rfc1123_date(calendar:now_to_local_time(Timestamp))}
	    ]}.

script() ->
    Client = riak_client(),
    {ok, Keys} = Client:list_keys(bucket()),
    StateJS = lists:foldl(fun(Key, Acc0) ->
				  {ok, O1} = Client:get(bucket(), Key, 1),
				  Value = riak_object:get_value(O1),
				  Acc0 ++ io_lib:format("wave.getState().submitDelta({'~s':'~s'});", [Key, escape_quotes(Value)])
			  end,
			  "",
			  Keys),
    
    "$(document).ready(function() {
        updateUI('.wfid_calendar_container');"
        ++ StateJS ++ "
        $('.day_link').addClass('hidden_link');
        $('.add-remove-month').hide();
    });".

escape_quotes(String) ->
    escape_quotes(String, "").

escape_quotes([Hd|Rest], Acc) ->
    case Hd of
	%% Use 39 instead of $' because it screws up syntax highlighting in emacs
	39 -> escape_quotes(Rest, Acc ++ "\\'"); 
        C -> escape_quotes(Rest, Acc ++ [C])
    end;
escape_quotes([], Acc) ->
    Acc.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------- Events ---------- %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

event(add_user) ->
    Name = wf:q(newParticipant),
    save(riak_client(), {list_to_atom(Name), "[]"}),
    wf:state(user, Name),
    wf:update(participantPanel, participant_body(Name)),
    wf:wire( #script{ script="
      $('.day_link').removeClass('hidden_link');
      wave.setViewerId('" ++ Name ++ "');
    "}),
    ok;
event({user_selected, _Id, Name}) ->
    %% Show the links when a participant is defined
    wf:state(user, Name),
    wf:wire( #script{ script="
      $('.day_link').removeClass('hidden_link');
      $('.add-remove-month').show();
      wave.setViewerId('" ++ Name ++ "');
    "});
event(create_comment) ->
    Name = wf:state(user),
    CommentText = wf:q(commentArea), 
    NewComment = {struct, [{name, Name}, 
			   {timestamp, tuple_to_list(now())}, 
			   {commentText, CommentText}]},
    Client = riak_client(),
    case Client:get(bucket(), <<"_comments">>, 1) of
	{ok, R0} ->  
	    JSONArray = riak_object:get_value(R0),
	    Comments = mochijson2:decode(JSONArray),
	    NewComments = Comments ++ [NewComment],
	    Encoded = mochijson2:encode(NewComments),
	    R1 = riak_object:update_value(R0, Encoded),
	    ok = Client:put(R1, 1);
	{error, notfound} ->
	    R0 = riak_object:new(bucket(), <<"_comments">>, mochijson2:encode([NewComment])),
	    ok = Client:put(R0, 1)
    end,
     wf:update(existingComments, existing_comments());
event(Event) ->
    io:format("~p~n", [Event]).


api_event(save, _, [Hash]) ->
    %% Update each of the keys using the bucket for the
    %% event
    Client = riak_client(),
    lists:map(fun (KV) ->
		      save(Client, KV)
	      end,
	      Hash).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------- Validators ---------- %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


validate_unique(_Tag, Value) ->
    Participants = wf:state_default(participants, []),
    case lists:keyfind(Value, 2, Participants) of
	false -> true;
	_ -> false
    end.

validate_tags(_Tag, Value) ->
    ContainsLT = lists:member($<, Value),
    ContainsGT = lists:member($>, Value),
    if
	ContainsLT or ContainsGT -> false;
	true -> true
    end.

validate_user_selected(_Tag, _Value) ->
    wf:state(user) /= undefined.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------- Utils ---------- %%
%%%%%%%%%%%%%%%%%%%%%%d%%%%%%%%%%%
	     

save(Client, {Key, Value}) ->
    RiakKey = list_to_binary(atom_to_list(Key)),
    case Client:get(bucket(), RiakKey, 1) of
	{ok, R0} ->  
	    R1 = riak_object:update_value(R0, Value),
	    ok = Client:put(R1, 1);
	{error, notfound} ->
	    R0 = riak_object:new(bucket(), RiakKey, Value),
	    ok = Client:put(R0, 1)
    end.

bucket() ->
    list_to_binary(wf:path_info()).

riak_client() ->
    case wf:state_default(riak_client, undefined) of
	undefined ->
	    {ok, Client} = riak:client_connect('riak@127.0.0.1'),
	    wf:state(riak_client, Client),
	    Client;
	Client ->
	    Client
    end.


