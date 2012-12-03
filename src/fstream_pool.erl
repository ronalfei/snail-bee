-module(fstream_pool).
-compile(export_all).

-behaviour(gen_server).

-export([
    start/0,
    stop/0,
    curry/2,
    i/0,
    tick/0
]).

%% Internal exports - gen_server callbacks
-export([init/1,
     handle_call/3,
     handle_cast/2,
     handle_info/2,
     terminate/2,
     code_change/3
    ]).

-include_lib("kernel/include/file.hrl").
-include("snail.hrl").

-define(LIFE_SPAN, 10*1000).
-define(TICK_INTERVAL, 2*1000).

-record(state, {pool}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:cast(stop).

curry(append, Path) ->
    gen_server:call(?MODULE, {curry, append, Path});

curry(pwrite, Path) ->
    gen_server:call(?MODULE, {curry, pwrite, Path});

curry(pread, Path) ->
    gen_server:call(?MODULE, {curry, pread, Path});

curry(preadv, Path) ->
    gen_server:call(?MODULE, {curry, preadv, Path});

curry(info, Path) ->
    gen_server:call(?MODULE, {curry, info, Path});

curry(truncate, Path) ->
    gen_server:call(?MODULE, {curry, truncate, Path});

curry(close, Path) ->
    gen_server:cast(?MODULE, {close, Path}).

get_file_handle(Path) ->
	gen_server:call(?MODULE, {get_file_handle, Path}).

i() ->
    gen_server:call(?MODULE, {i}).
tick() ->
    gen_server:cast(?MODULE, {tick}).

%%
init([]) ->
    State = #state{pool=ets:new(pool, [set, private])},
    timer:apply_interval(?TICK_INTERVAL, ?MODULE, tick, []),
    {ok, State}.

handle_call({curry, append, Path}, _From, State) ->
    try
        Pool = State#state.pool,
        {ok, IoDevice} = get_device(Pool, Path, append),

        Curry = fun(Bytes) ->
            case file:write(IoDevice, Bytes) of
                ok -> {ok, true};
                _ -> {error, false}
            end
        end,

        {reply, {ok, Curry}, State}
    catch _:_ ->
        lager:debug("fail to get append curry"),
        {reply, {error, "fail to get append curry"}, State}
    end;

handle_call({curry, pwrite, Path}, _From, State) ->
    try
        Pool = State#state.pool,
        {ok, IoDevice} = get_device(Pool, Path, write),

        Curry = fun(Location, Bytes) ->
            case file:pwrite(IoDevice, Location, Bytes) of
        		ok -> {ok, true};
        		_Any -> 
					lager:error("!!!!!!!!!!pwrite return ~p !!!!!!!!!!!!!!!!1", [_Any]),
					{error, false}
        	end
        end,

        {reply, {ok, Curry}, State}
    catch _:_ ->
        lager:debug("fail to get pwrite curry"),
        {reply, {error, "fail to get pwrite curry"}, State}
    end;

handle_call({curry, pread, Path}, _From, State) ->
    try
        Pool = State#state.pool,
        {ok, IoDevice} = get_device(Pool, Path, read),

        Curry = fun(Location, Number) ->
            file:pread(IoDevice, Location, Number)
        end,

        {reply, {ok, Curry}, State}
    catch _:_ ->
        {reply, {error, "fail to get pread curry"}, State}
    end;

handle_call({curry, preadv, Path}, _From, State) ->
    try
        Pool = State#state.pool,
        {ok, IoDevice} = get_device(Pool, Path, read),

        Curry = fun(LocNums) ->
            file:pread(IoDevice, LocNums)
        end,
        {reply, {ok, Curry}, State}
    catch _:_ ->
        lager:debug("fail to get preadv curry"),
        {reply, {error, "fail to get preadv curry"}, State}
    end;

handle_call({curry, info, Path}, _From, State) ->
    try
        Pool = State#state.pool,
        {ok, IoDevice} = get_device(Pool, Path, read),

        Curry = fun(Atom) ->
            case Atom of
                size ->
                    {ok, Size} = file:position(IoDevice, eof),
                    {ok, 0} = file:position(IoDevice, bof),
                    {ok, Size};
                _ ->
                    {error, "unknown key"}
            end
        end,

        {reply, {ok, Curry}, State}
    catch _:_ ->
        {reply, {error, "fail to get pwrite curry"}, State}
    end;

handle_call({curry, truncate, Path}, _From, State) ->
    try
        Pool = State#state.pool,
        {ok, IoDevice} = get_device(Pool, Path, write),

		Curry = fun(Position) ->
		%	file:position(IoDevice, Position),
		%	file:truncate(IoDevice)
		%-----------------------------------
            case file:position(IoDevice, {cur, Position}) of
        		{ok, _NewPos} ->
					lager:debug("Position====================~p, new position is ~p", [Position, _NewPos]),
					file:truncate(IoDevice),
					?MODULE:curry(close, Path);
        		_Any ->
					lager:debug("error any====================~p", [_Any]),
					{error, false}
        	end
        end,
        {reply, {ok, Curry}, State}

    catch _A:_B ->
		lager:error("handle_call truncate error ~p-----------------------~p~n", [_A, _B]),
        {reply, {error, "fail to get pwrite curry"}, State}
    end;

handle_call({i}, _From, State) ->
    Info = ets:tab2list(State#state.pool),
    {reply, Info, State}.

handle_cast({tick}, State) ->
    Pool = State#state.pool,
    L = ets:tab2list(Pool),
    Fun = fun({Path, {Device, Life}}) ->
        NextLifeVal = Life - ?TICK_INTERVAL,
        case NextLifeVal < 0 of
            true ->
                file:close(Device),
                ets:delete(Pool, Path);
            false ->
                ets:insert(Pool, {Path, {Device, NextLifeVal}})
        end
    end,
    lists:map(Fun, L),
    {noreply, State};

handle_cast({close, Path}, State) ->
    Pool = State#state.pool,
	case ets:lookup(Pool, Path) of
		[{_Path, {PooledDevice, _Life}}] ->
		    file:close(PooledDevice),
    		ets:delete(Pool, Path);
		[] ->
			[]
	end,
    {noreply, State};


handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

get_device(Pool, Path, Atom) ->
	lager:debug("Path............... : ~p", [Path]),
    try
        Device = case ets:lookup(Pool, Path) of
            [] ->
                {ok, IoDevice} = open_file(Path, Atom),
                IoDevice;
            [{_Path, {PooledDevice, _Life}}] ->
                PooledDevice
        end,
        ets:insert( Pool, {Path, {Device, ?LIFE_SPAN}} ),
        {ok, Device}
    catch _:_ ->
        lager:debug("failed to get device"),
        {error, "failed to get device"}
    end.

open_file(Path, read) ->
    file:open(Path,[
          read,  {read_ahead, ?READ_AHEAD_SIZE},
          write, {delayed_write, ?DELAY_WRITE_SIZE, ?DELAY_WRITE_TIME},
          binary
    ]);
open_file(Path, write) ->
    file:open(Path,[
          read,  {read_ahead, ?READ_AHEAD_SIZE},
          write, {delayed_write, ?DELAY_WRITE_SIZE, ?DELAY_WRITE_TIME},
          binary
    ]);

open_file(Path, append) ->
    file:open(Path,[
          %read,  {read_ahead, ?READ_AHEAD_SIZE},
          write, {delayed_write, ?DELAY_WRITE_SIZE, ?DELAY_WRITE_TIME},
          binary 
    ]).
