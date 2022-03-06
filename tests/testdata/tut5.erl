%% From: https://erlang.org/doc/getting_started/seq_prog.html#a-larger-example
%% This module is in file tut5.erl
-module(tut5).
-export([format_temps/1]).


%% Only this function is exported
format_temps([]) ->  % No output for an empty list
    ok;
format_temps([City | Rest]) ->
    print_temp(convert_to_celsius(City)),
    format_temps(Rest).


convert_to_celsius({Name, {c, Temp}}) ->  % No conversion needed
    {Name, {c, Temp}};
convert_to_celsius({Name, {f, Temp}}) ->  % Do the conversion
    {Name, {c, (Temp - 32) * 5 / 9}}.


print_temp({Name, {c, Temp}}) ->
    io:format("~-15w ~w c~n", [Name, Temp]).
