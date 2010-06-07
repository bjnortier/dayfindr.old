-module (dayfindr_app).
-export ([start/2, stop/1]).
-behavior(application).

start(_, _) -> 
    dayfindr_sup:start_link().

stop(_) -> 
    dayfindr_sup:stop().

