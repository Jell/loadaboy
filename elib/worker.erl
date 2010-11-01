-module(worker, [Parent, ProfileName, Domain, Urls]).
-compile(export_all).

run() ->
  spawn(
    fun() ->
      inets:start(httpc, [{profile, ProfileName}]),
      fetch_urls(Urls)
    end
    ).

fetch_urls([]) ->
  Parent!done;
fetch_urls([{Name, Url}|Tail]) ->
  [Header, Time] = fetch_url(Domain ++ binary_to_list(Url)),
  forecast_result(Header, Name, Time, Url),
  fetch_urls(Tail).

forecast_result(Header, Name, Time, Url) ->
  io:format("~p ~p ~p ~s~n", [Header, Name, Time, Url]).

fetch_url(Url) ->
  try
    Tic = erlang:now(),
    Result = get_over_http(Url),
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

get_over_http(Url) ->
  {ok, RequestId} = httpc:request(get, {Url, []}, [], [{sync, false}], ProfileName),
  receive
    {http, {RequestId, Result}} ->
      Result
  after
    10000 ->
      timeout
  end.
