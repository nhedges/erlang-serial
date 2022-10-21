-module(serial_tests).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(TARMOD, serial).

match_message_test() ->
  ?assertEqual({[], <<"abcdef">>, 4},
               ?TARMOD:match_message(<<"abcdef">>, 0, "gh")),
  ?assertEqual({[<<"abcdefgh">>], <<"xyz">>, 1},
               ?TARMOD:match_message(<<"abcdefghxyz">>, 0, "gh")),
  ?assertEqual({[<<"abcdefgh">>,<<"qrsgh">>], <<"xyz">>, 1},
               ?TARMOD:match_message(<<"abcdefghqrsghxyz">>, 6, "gh")).

-endif. %TEST
