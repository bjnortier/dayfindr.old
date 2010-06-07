-module (event).
-compile(export_all).
-include_lib("nitrogen/include/wf.hrl").

main() -> #template { file="./site/templates/bare.html" }.

title() -> "dayfindr.com".

layout() ->
    #container_12 { 
	body=[
	      #grid_10 {alpha=true, prefix=1, suffix=1, body=participants_panel()},
	      #grid_10 {prefix=1, suffix=1, omega=true, body=body()}
	     ]}.



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
				    #custom { text="Please choose a unique name", function=fun validate_unique/2 }
				   ]}),
    [
     case length(Participants) of
	 0 -> [];
	 _ -> #label { text="I've been here before:" }
     end,
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
		       Participants))},
     #panel {body=
	     [
	      #label { text="I'm new:" },
	      #textbox { id=newParticipant, text="" },
	      #br{},
	      #button { id=addButton, text="Ok", postback=add_user }
	     ]}
    ].

participants() ->
    %% The participants are the keys in the bucket, but
    %% disregarding any metadata values that start with "_"
    Client = riak_client(),
    {ok, Keys} = Client:list_keys(bucket()),
    StringKeys = lists:map(fun binary_to_list/1, Keys),
    Filtered = [X || X <- StringKeys, hd(X) /= $_],
    to_participants(Filtered).

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

script() ->
    Client = riak_client(),
    {ok, Keys} = Client:list_keys(bucket()),
    StateJS = lists:foldl(fun(Key, Acc0) ->
				  {ok, O1} = Client:get(bucket(), Key, 1),
				  Value = riak_object:get_value(O1),
				  Acc0 ++ io_lib:format("wave.getState().submitDelta({'~s':'~s'});", [Key, Value])
			  end,
			  "",
			  Keys),
    
    "$(document).ready(function() {
        updateUI('.wfid_calendar_container');"
        ++ StateJS ++ "
        $('.day_link').addClass('hidden_link');
    });".
	
event(add_user) ->
    Name = wf:q(newParticipant),
    save(riak_client(), {list_to_atom(Name), "[]"}),
    wf:replace(participantPanel, participant_body(Name)),
    wf:wire( #script{ script="
      $('.day_link').removeClass('hidden_link');
      wave.setViewerId('" ++ Name ++ "');
    "}),
    ok;
event({user_selected, _Id, Name}) ->
    %% Show the links when a participant is defined
    wf:wire( #script{ script="
      $('.day_link').removeClass('hidden_link');
      wave.setViewerId('" ++ Name ++ "');
    "});
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

validate_unique(_Tag, Value) ->
    case lists:keyfind(Value, 2, participants()) of
	false -> true;
	_ -> false
    end.
	     

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
