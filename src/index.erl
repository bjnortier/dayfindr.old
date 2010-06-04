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
    [
     #panel{id="calendar_container", body="placeholder"}
    ].

script() ->
    "$(document).ready(function() {
        updateUI('.wfid_calendar_container');
    });".
	
event(click) ->
    wf:replace(button, #panel { 
        body="You clicked the button!", 
        actions=#effect { effect=highlight }
    }).
