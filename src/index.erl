-module (index).
-compile(export_all).
-include_lib("nitrogen/include/wf.hrl").

main() -> #template { file="./site/templates/bare.html" }.

title() -> "dayfindr.com".

layout() ->
    #container_12 { 
	body=[
	      #grid_10 {alpha=true, prefix=1, suffix=1, omega=true, body=body()}
	     ]}.


body() -> 

    wf:wire(createButton, description, 
	    #validate { validators=[
				    #is_required { text="Please describe your event" }
				   ]}),

    wf:wire(createButton, emailAddress, 
	    #validate { validators=[
				    #is_email { text="Not a valid email address." }
				   ]}),
    [
     #panel{body=
	    [
	     #h2 {text="Create a new event"},
	     #label {text="Your email address: (only participants will see this)"}, 
	     #textbox {id=emailAddress},
	     #br{},
	     #label {text="Describe your event:"},
	     #textarea {id=description},
	     #br{},
	     #button { id=createButton, text="Create", postback=create }
	    ]}
    ].


-define(URLSAFE_CHARS, "abcdefghijklmnopqrstuvwxyzuABCDEFGHIJKLMNOPQRSTUVWXYZU1234567890-_").
	
event(create) ->
    {A1,A2,A3} = now(),
    random:seed(A1, A2, A3),
    EventId = lists:map(fun(_X) ->
				lists:nth(random:uniform(64), ?URLSAFE_CHARS)
			end,
			lists:seq(1, 20)),
    {ok, Client} = riak:client_connect('riak@127.0.0.1'),
    OrganiserEmail = wf:q(emailAddress),
    ok = Client:put(riak_object:new(list_to_binary(EventId), <<"_description">>, wf:q(description)), 1),
    ok = Client:put(riak_object:new(list_to_binary(EventId), <<"_organiser_email">>, OrganiserEmail), 1),
    %% Notify the creator of the event
    URL = "http://www.dayfindr.com/event/" ++ EventId,
    smtp:send(OrganiserEmail, create_message(OrganiserEmail, URL)),
    wf:redirect("/event/" ++ EventId).

create_message(Organiser, URL) ->
    "
Dear " ++ Organiser ++ "

Your event on dayfindr.com has been created! Send this link to the people you want to invite:

" ++ URL ++ "

Enjoy!
Dayfindr.com".
