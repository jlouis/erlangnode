-module(test).

-export([run/0]).

run() ->
    P = open_port({spawn, "./main.native"}, [{packet, 4}, binary, exit_status]),
    true = port_command(P, term_to_binary(3.14)),
    receive
      {P, {data, B}} ->
         47 = binary_to_term(B),
         ok
    after 300 ->
      exit(argh)
    end.
