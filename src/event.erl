-module (event).
-compile(export_all).
-include_lib("nitrogen/include/wf.hrl").

main() -> #template { file="./site/templates/bare.html" }.

title() -> "dayfindr.com".

layout() ->
    #container_12 { 
	body=[
	      #grid_10 {alpha=true, prefix=1, suffix=1, body=participants()},
	      #grid_10 {prefix=1, suffix=1, omega=true, body=body()}
	     ]}.


participants() ->
    Participants = [{0, "Benjamin"}, {1, "Annie"}, {2, "John"}],
    #radiogroup { body=
		  %% Need to flatten the list for the correct radio grouping
		  lists:flatten(
		    lists:map(
		      fun({Id, Name}) ->
			      ElementId = list_to_atom("participat_" ++ integer_to_list(Id)),
			      [
			       #radio { id=ElementId, text=Name, value=io_lib:format("~p", [Id]), checked=false, postback={user_selected, Id, Name}}, 
			       #br{}
			      ]
		      end,
		      Participants))}.
		   

body() -> 
    wf:wire(#api { name=save }),
    [
     #panel{id="calendar_container", body="placeholder"}
    ].

script() ->
    "$(document).ready(function() {
        updateUI('.wfid_calendar_container');
        $('.day_link').addClass('hidden_link');
    });".
	
event(click) ->
    wf:replace(button, #panel { 
        body="You clicked the button!", 
        actions=#effect { effect=highlight }
    });
event({user_selected, Id, Name}) ->
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
    {ok, Client} = riak:client_connect('riak@127.0.0.1'),
    lists:map(fun (KV) ->
		      save(Client, KV)
	      end,
	      Hash).

save(Client, {Key, Value}) ->
    io:format("~p:~p~n", [Key, Value]),
    RiakKey = list_to_binary(atom_to_list(Key)),
    Bucket = <<"event123">>,
    case Client:get(Bucket, RiakKey, 1) of
	{ok, R0} ->  
	    R1 = riak_object:update_value(R0, Value),
	    ok = Client:put(R1, 1);
	{error, notfound} ->
	    R0 = riak_object:new(Bucket, RiakKey, Value),
	    ok = Client:put(R0, 1)
    end.
