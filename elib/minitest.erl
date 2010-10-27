-module(minitest, [Tests]).
-export([run/0]).

run() ->
  Results = ["\e[0;31mFAIL: " ++ Assertion(desc) ++ " => " ++ io_lib:format("~p", [Reason]) ++ "\e[0;30m~n"||
              Test            <- Tests,
              _               <- [Test(up)],
              Assertion       <- Test(assertions),
              Result          <- [run_test (Assertion)],
              _               <- [Test(down)],
              {fail, Reason}  <- [Result]
            ],
  AssertionCount = lists:foldl(fun(Test, Sum) -> length(Test(assertions)) + Sum end, 0, Tests),
  io:format("~nFinished~n~n"),
  io:format("~p test(s), ~p assertion(s), ~p failure(s)~n", [length(Tests), AssertionCount, length(Results)]),
  [io:format(Explanation) || Explanation <- Results].

run_test(Test) ->
  try
    Test(run),
    io:format("\e[0;32m.\e[0;30m"),
    ok
  catch
    _:Reason ->
      io:format("\e[0;31mF\e[0;30m"),
      {fail, Reason}
  end.