#!/usr/bin/env escript
%% -*- erlang -*-

main(_) ->
  io:format("Started~n"),
  error_logger:tty(false),
  Tests = [
    fun (Action) -> test_load_test_new(Action) end,
    fun (Action) -> test_load_test_start(Action) end,
    fun (Action) -> test_load_test_stop(Action) end,
    fun (Action) -> test_load_test_set_urls(Action) end
  ],
  TestSuite = minitest:new(Tests),
  TestSuite:run().

new_load_test() ->
  Domain = io_lib:format("http://localhost:3000", []),
  load_test:new(Domain, 1, 1, []).

% ---------------------------------------------------------------------
%   load_test:new
% ---------------------------------------------------------------------
test_load_test_new(up) ->
  ok;
test_load_test_new(down) ->
  ok;
test_load_test_new(assertions) ->
  [
    fun (Action) -> test_load_test_new1(Action) end
  ].

test_load_test_new1(desc) ->
  "load_test:new() should instanciate a new module";
test_load_test_new1(run) ->
  new_load_test().

% ---------------------------------------------------------------------
%   load_test:start
% ---------------------------------------------------------------------
test_load_test_start(up) ->
  inets:stop(),
  mnesia:stop();
test_load_test_start(down) ->
  inets:stop(),
  mnesia:stop();
test_load_test_start(assertions) ->
  [
    fun (Action) -> test_load_test_start1(Action) end,
    fun (Action) -> test_load_test_start2(Action) end
  ].

test_load_test_start1(desc) ->
  "load_test:start() should start inets";
test_load_test_start1(run) ->
  LoadTest = new_load_test(),
  LoadTest:start(),
  {error,{already_started,inets}} = inets:start().

test_load_test_start2(desc) ->
  "load_test:start() should start mnesia";
test_load_test_start2(run) ->
  LoadTest = new_load_test(),
  LoadTest:start(),
  yes = mnesia:system_info(is_running).

% ---------------------------------------------------------------------
%   load_test:stop
% ---------------------------------------------------------------------
test_load_test_stop(up) ->
  inets:start(),
  mnesia:start();
test_load_test_stop(down) ->
  inets:stop(),
  mnesia:stop();
test_load_test_stop(assertions) ->
  [
    fun (Action) -> test_load_test_stop1(Action) end,
    fun (Action) -> test_load_test_stop2(Action) end
  ].

test_load_test_stop1(desc) ->
  "load_test:start() should stop inets";
test_load_test_stop1(run) ->
  LoadTest = new_load_test(),
  LoadTest:stop(),
  {error,{not_started,inets}} = inets:stop().

test_load_test_stop2(desc) ->
  "load_test:start() should stop mnesia";
test_load_test_stop2(run) ->
  LoadTest = new_load_test(),
  LoadTest:stop(),
  no = mnesia:system_info(is_running).

% ---------------------------------------------------------------------
%   load_test:set_urls
% ---------------------------------------------------------------------
test_load_test_set_urls(up) ->
  inets:start(),
  mnesia:start(),
  LoadTest = new_load_test(),
  LoadTest:prepare_mnesia();
test_load_test_set_urls(down) ->
  inets:stop(),
  mnesia:stop();
test_load_test_set_urls(assertions) ->
  [
    fun (Action) -> test_load_test_set_urls1(Action) end
  ].

test_load_test_set_urls1(desc) ->
  "load_test:set_urls() should set urls in mnesia";
test_load_test_set_urls1(run) ->
  LoadTest = new_load_test(),
  LoadTest:set_urls("service", "worker", "urls"),
  {atomic, ["urls"]} = LoadTest:get_urls("service", "worker").