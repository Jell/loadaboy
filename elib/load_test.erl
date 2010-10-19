-module(load_test, [Domain, WorkerCount, RequestCount]).
%-export([start/0, stop/0, run/0, generate_services/1, get_services/0, get_workers/1, get_jobs/2, get_urls/3, set_urls/4]).
-compile(export_all).
-record(load_entry, {key, urls}).

start() ->
	inets:start(),
	mnesia:start().

stop() ->
	mnesia:stop().

prepare() ->
	mnesia:delete_table(load_entry),
	mnesia:create_table(load_entry, [{attributes, record_info(fields, load_entry)}]),
	Cmd = "ruby ../lib/erlang_interface.rb",
	Port = open_port({spawn, Cmd}, [{packet, 4}, nouse_stdio, exit_status, binary]),
	Payload = term_to_binary([prepare, [WorkerCount, RequestCount]]),
	port_command(Port, Payload),
	receive
		{Port, {data, Data}} ->
			{result, Text} = binary_to_term(Data),
			[set_urls(test, WorkerName, tuple_to_list(Urls)) || {WorkerName, Urls}<- tuple_to_list(Text)]
	end.
	
run() ->
	{atomic, Services} = get_services(),
	generate_services(lists:usort(Services)),
	wait_for_workers(WorkerCount).

generate_services(Services) ->
	case Services of
		[] ->
			done;
		[Service | Remaining] ->
			{atomic, Workers} = get_workers(Service),
			generate_workers(Service, Workers),
			generate_services(Remaining)
  end.

generate_workers(Service, Workers) ->
	case Workers of
		[] ->
			done;
		[WorkerName | Remaining] ->
			{atomic, [Urls]} = get_urls(Service, WorkerName),
			Worker = worker:new(self(), WorkerName, Domain, Urls),
			Worker:run(),
			generate_workers(Service, Remaining)
  end.

wait_for_workers(RemainingWorkers) ->
	receive
		done ->
			case RemainingWorkers - 1 of
				0 ->
					done;
				_ ->
					wait_for_workers(RemainingWorkers - 1)
			end
		end.

get_services() ->
	F = fun() ->
			LoadEntry = #load_entry{key = {'$1', '_'}, urls = '_'},
			mnesia:select(load_entry, [{LoadEntry, [], ['$1']}])
		end,
	mnesia:transaction(F).

get_workers(Service) ->
	F = fun() ->
			LoadEntry = #load_entry{key = {Service, '$1'}, urls = '_'},
			mnesia:select(load_entry, [{LoadEntry, [], ['$1']}])
		end,
	mnesia:transaction(F).


get_urls(Service, Worker) ->
	F = fun() ->
			LoadEntry = #load_entry{key = {Service, Worker}, urls = '$1'},
			mnesia:select(load_entry, [{LoadEntry, [], ['$1']}])
		end,
	mnesia:transaction(F).

set_urls(Service, Worker, Urls) ->
	LoadEntry  = #load_entry{key = {Service, Worker}, urls = Urls},
	F = fun() ->
			mnesia:write(LoadEntry)
		end,
	mnesia:transaction(F).