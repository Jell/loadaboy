-module(worker, [Parent, ProfileName, Domain, Urls]).
-compile(export_all).

run() ->
	spawn(
		fun() ->
			inets:start(httpc, [{profile, ProfileName}]),
			fetch_urls(Urls)
		end
		).

fetch_urls(UrlList) ->
	case UrlList of
		[] ->
			Parent!done;
		[Url|Tail] ->
			{Name, Request} = Url,
			[Header, Time] = fetch_url(Domain ++ binary_to_list(Request)),
			io:format("~p ~p ~p~n", [Header, Name, Time]),
			fetch_urls(Tail)
	end.

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
