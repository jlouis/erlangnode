-module(term_eqc).
-compile(export_all).
-include_lib("eqc/include/eqc.hrl").

int_32bit() ->
    choose(-2147483648, 2147483647).

atom() ->
    elements([a,b,c,d,e,f,g,h,i,j]).

string() ->
    ?LET(Bin, oneof([binary(), eqc_gen:largebinary()]),
        binary_to_list(Bin)).

float() ->
    real().

g_list(Gen) ->
    list(Gen).

g_tuple(Gen) ->
    ?LET(L, list(Gen), list_to_tuple(L)).

g_pid() ->
    ?LET({X,Y,Z}, {0, nat(), nat()},
         begin
             B = iolist_to_binary(["<",
                                   integer_to_list(X), ".",
                                   integer_to_list(Y), ".",
                                   integer_to_list(Z),
                                   ">"]),
             erlang:list_to_pid(binary_to_list(B))
         end).

term() ->
    ?SIZED(Sz, term(Sz)).

term(0) ->
    oneof(
      [int_32bit(),
       largeint(),
       atom(),
       non_empty(string()),
       binary(),
       bitstring(),
       g_pid(),
       %% eqc_gen:largebinary(), %% Size problem detected
       float()]);
term(N) ->
    frequency(
      [{5, ?LAZY(term(0))},
       {1, ?LAZY(
              ?LETSHRINK(L, g_list(term(N div 4)), L))},
       {1, ?LAZY(g_tuple(term(N div 4)))}]).

start_port() ->
    Self = self(),
    spawn_link(fun() ->
        register(output_port, self()),
        Self ! go,
        loop(cycle(undefined))
    end),
    receive go -> ok end, 
    ok.

loop(P) ->
    receive
        {s, From, Term} ->
            true = port_command(P, Term),
            receive
              {P, {data, B}} ->
                  From ! {r, B}
            after 50 ->
                  loop(cycle(P))
            end,
            loop(P)
    end.

roundtrip(T) ->
    output_port ! {s, self(), T},
    receive
       {r, Res} -> Res
    after 60 ->
       exit(argh)
    end.

cycle(P) when is_port(P) ->
    catch port_close(P),
    cycle(undefined);
cycle(undefined) ->
    open_port({spawn, "./main.native"}, [{packet, 4}, binary, exit_status]).

stop_port() ->
   Pid = whereis(output_port),
   case Pid of
       undefined -> ok;
       P when is_pid(P) -> exit(P, kill)
   end.

prop_port_correct() ->
    ?SETUP(fun () ->
                   start_port(),
                   fun() ->
                           stop_port()
                   end
    end,
    ?FORALL(T, term(),
            begin
                I = term_to_binary(T),
                O = roundtrip(I),
                conjunction(
                  [{term, equals(T, binary_to_term(O))},
                   {binary, equals(I, O)}])
            end)).

pipe_test(Sz) ->
    P = cycle(undefined),
    pipe_loop(Sz, P),
    pipe_drain(P),
    port_close(P),
    ok.

pipe_loop(0, _) -> ok;
pipe_loop(K, P) ->
    true = port_command(P, term_to_binary(K)),
    pipe_loop(K-1, P).

pipe_drain(Port) ->
   receive
      {Port, {data, B}} ->
          _ = binary_to_term(B),
          pipe_drain(Port)
  after 0 ->
      ok
  end.
