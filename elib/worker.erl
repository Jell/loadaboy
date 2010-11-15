-module(worker).
-export([code_change/3, handle_cast/2, handle_call/3, handle_info/2, init/1, terminate/2]).
-behavior(gen_server).

init([Parent, ProfileName, Domain, Urls]) ->
  inets:start(httpc, [{profile, ProfileName}]),
  {ok, [Parent, ProfileName, Domain, Urls]}.

handle_call(_Message, _From, [Parent, ProfileName, Domain, Urls]) ->
  fetch_urls([ProfileName, Domain, Urls]),
  {reply, done, [Parent, ProfileName, Domain, Urls]}.

handle_cast(run, [Parent, ProfileName, Domain, Urls]) ->
  fetch_urls([ProfileName, Domain, Urls]),
  Parent!done,
  {noreply, [Parent, ProfileName, Domain, Urls]}.

handle_info(Msg, State) ->
  io:format("Unexpected message: ~p~n",[Msg]),
  {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
  %% No change planned. The function is there for the behaviour,
  %% but will not be used. Only a version on the next
  {ok, State}.

terminate(_, _) -> ok.

fetch_urls([_ProfileName, _Domain, []]) ->
  done;
fetch_urls([ProfileName, Domain, [{Name, Url}|Tail]]) ->
  [Header, Time] = fetch_url(Domain ++ binary_to_list(Url), ProfileName),
  forecast_result(Header, Name, Time, Url),
  fetch_urls([ProfileName, Domain, Tail]).

forecast_result(Header, Name, Time, Url) ->
  io:format("~p ~p ~p ~s~n", [Header, Name, Time, Url]).

fetch_url(Url, ProfileName) ->
  try
    Tic = erlang:now(),
    Result = get_over_http(Url, ProfileName),
    Tac = erlang:now(),
    Time = time_to_ms(Tac) - time_to_ms(Tic),
    case Result of
      timeout ->
        [{"HTTP/1.1", 408, "Request Timeout"}, Time];
      {Header, _Params, _Body} ->
        [Header, Time]
    end
  catch
    _:_ ->
      [{"HTTP/1.1", 400, "Bad Request"}, 0]
  end.

time_to_ms({Mega, Sec, Micro})->
  Mega * 1000000000 + Sec * 1000 + Micro / 1000.

get_over_http(Url, ProfileName) ->
  {ok, RequestId} = http:request(get, {Url, []}, [], [{sync, false}], ProfileName),
  receive
    {http, {RequestId, Result}} ->
      Result
  after
    10000 ->
      timeout
  end.
