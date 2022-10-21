%% Copyright (c) 1996, 1999 Johan Bevemyr
%% Copyright (c) 2007, 2009 Tony Garnock-Jones
%% 
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%% 
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%% 
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%%
%    -*- Erlang -*- 
%    File:	serial.erl  (~jb/serialport/serial.erl)
%    Author:	Johan Bevemyr
%    Created:	Tue Oct 22 14:07:24 1996
%    Purpose:   

-module(serial).
-author('jb@erix.ericsson.se').

-behaviour(gen_server).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-export([start_link/1]).

-ifdef(TEST).
-export([match_message/3]).
-endif. %TEST

-include("serial.hrl").

-record(state,
        {port::any(),
         owner_pid::pid(),
         data_buffer= <<>>::binary(),
         message_delimiter::binary()|undefined,
         scan_index=0::binary()}).

priv_dir() ->
    case code:priv_dir(serial) of
	{error, bad_name} ->
	    "./priv";
	D ->
	    D
    end.

-spec start_link(Options::[tuple()]) -> {ok, pid()}.
start_link(Options) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [self(), Options], []).

init([OwnerPid, _Options]) ->
  process_flag(trap_exit,true),
  Port = open_port({spawn,priv_dir()++"/bin/serial -erlang"},[binary,{packet,2}]),
  {ok, #state{owner_pid=OwnerPid, port=Port}}.

handle_options([], State) ->
  State;
handle_options([{message_delimiter, Pattern} | Rest], State) ->
  handle_options(Rest, State#state{message_delimiter=Pattern});
handle_options([{open, TtyPath} | Rest], State) ->
  gen_server:cast(self(), {open, TtyPath}),
  handle_options(Rest, State);
handle_options([{speed, Baud} | Rest], State) ->
  gen_server:cast(self(), {speed, Baud}),
  handle_options(Rest, State).

handle_info(timeout, #state{}=State) ->
  {noreply, State};

handle_info({Port, {data, Bytes}}, #state{port=Port,
                                          data_buffer=Buf,
                                          owner_pid=Owner,
                                          scan_index=Idx,
                                          message_delimiter=Pattern}=State) ->
  ExpandedBuf = <<Buf/binary, Bytes>>,
  case match_message(ExpandedBuf, Idx, Pattern) of
    {[], NewBuf, NewIdx} ->
      {noreply, State#state{data_buffer=NewBuf, scan_index=NewIdx}};
    {[Messages], NewBuf, NewIdx} ->
      lists:foreach(fun(Msg) ->
                        Owner ! {serial_rx_data, Msg}
                    end,
                    Messages),
      {noreply, State#state{data_buffer=NewBuf, scan_index=NewIdx}}
  end;

handle_info({'EXIT', Port, Why}, #state{port=Port}=State)->
  {stop, Why, State};

handle_info({'EXIT', Linked, Why}, #state{}=State) ->
  {stop, Why, State}.


handle_cast({send, Bytes}, #state{port=Port}=State) ->
  send_serial(Port,[?SEND, Bytes]),
  {noreply, State};

handle_cast({connect}, #state{port=Port}=State) ->
  send_serial(Port,[?CONNECT]),
  {noreply, State};

handle_cast({disconnect}, #state{port=Port}=State) ->
  send_serial(Port,[?DISCONNECT]),
  {noreply, State};

handle_cast({open, TtyPath}, #state{port=Port}=State) ->
  send_serial(Port,[?OPEN, TtyPath]),
  {noreply, State};

handle_cast({close}, #state{port=Port}=State) ->
  send_serial(Port,[?CLOSE]),
  {noreply, State};

handle_cast({speed, NewInSpeed, NewOutSpeed}, #state{port=Port}=State) ->
  send_serial(Port,[?SPEED,integer_to_list(NewInSpeed)," ",
        integer_to_list(NewOutSpeed),0]),
  {noreply, State};

handle_cast({speed, NewSpeed}, #state{port=Port}=State) ->
  send_serial(Port,[?SPEED,integer_to_list(NewSpeed)," ",
        integer_to_list(NewSpeed),0]),
  {noreply, State};

handle_cast({parity_odd}, #state{port=Port}=State) ->
  send_serial(Port,[?PARITY_ODD]),
  {noreply, State};

handle_cast({parity_even}, #state{port=Port}=State) ->
  send_serial(Port,[?PARITY_EVEN]),
  {noreply, State};

handle_cast({break}, #state{port=Port}=State) ->
  send_serial(Port,[?BREAK]),
  {noreply, State}.

handle_call(_Message, _From, State) ->
  {stop, unknown_call, State}.

%% Business logic

send_serial(Port,Message) ->
    Port ! {self(),{command,Message}}.

-spec match_message(Buffer::binary(), ContinueIdx::integer(), Pattern::string()) -> {[binary()], binary(), integer()}.
match_message(Buffer, ContinueIdx, Pattern) ->
  <<_PreSearch:ContinueIdx/binary, Unsearched/binary>> = Buffer,
  case re:run(Unsearched, Pattern) of
    nomatch ->
      {[], Buffer, max(0,byte_size(Buffer) - length(Pattern))};
    {match, [{Begin, Len}]} ->
      MessageSize = ContinueIdx + Begin + Len,
      <<Message:MessageSize/binary, RestBuf/binary>> = Buffer,
      {RestMessages, NewBuf, NewIdx} = match_message(RestBuf, 0, Pattern),
      {[Message | RestMessages], NewBuf, NewIdx}
  end.



