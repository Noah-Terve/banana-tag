-module(input).
-export([start/0, get_char/0]).

get_char() ->
    case binary_to_list(io:get_chars("Enter a character: ", 1)) of
        [Char] when Char == $q -> io:format("TERMINATING");
        [Char] -> io:format("You entered: ~c~n", [Char]),
                          get_char();
        [] -> io:format("No input received~n");
        _ -> io:format("Too many characters entered~n")
    end.

start() ->
    io:setopts([{binary, true}]),
    get_char().