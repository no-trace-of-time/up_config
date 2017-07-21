%%%-------------------------------------------------------------------
%%% @author simon
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%   read unionpay channel related merchant parameters:
%%%     1.merlist
%%%     2.each mer config: channelType/certId/privateKey/...
%%%     2.channel list: xiamen/shanghai/...
%%% @end
%%% Created : 20. 七月 2017 9:02
%%%-------------------------------------------------------------------
-module(gws_up_config).
-include_lib("eunit/include/eunit.hrl").
-author("simon").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

%% API
-export([
  get_mer_id/1
  , get_mer_id_default/1
  , get_mer_prop/2
  , get_config/1
  , check_bank_id/1
  , get_all_mer/0

]).

-define(SERVER, ?MODULE).
-define(MER_ID_TEST, <<"test">>).

-record(state, {bank_id_dict, mer_router_map, mer_list_map, public_key}).

-compile(export_all).

%%%===================================================================
%%% API
%%%===================================================================
get_mer_id(PaymentType) when is_atom(PaymentType) ->
  gen_server:call(?SERVER, {get_mer_id, PaymentType}).

get_mer_id_default(PaymentType) when is_atom(PaymentType) ->
  gen_server:call(?SERVER, {get_mer_id_default, PaymentType}).

get_mer_prop(MerId, Key) when is_atom(MerId),
  (Key =:= channelType orelse
    Key =:= certId orelse
    Key =:= privateKey
  )
  ->
  gen_server:call(?SERVER, {get_mer_prop, MerId, Key}).

get_config(Key) when is_atom(Key) ->
  gen_server:call(?SERVER, {get_config, Key}).

check_bank_id(BankId) when is_atom(BankId) ->
  gen_server:call(?SERVER, {check_bank_id, BankId}).

get_all_mer() ->
  gen_server:call(?SERVER, {get_all_mer}).
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  State = #state{
    bank_id_dict = get_bank_dict()
    , mer_router_map = get_route()
    , mer_list_map = get_mer_list()
    , public_key = get_up_public_key()
  },
  lager:debug("~p get env config = ~p", [?SERVER, State]),
  {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_call({get_mer_id, PaymentType}, _From, #state{mer_router_map = MerRouteMap} = State)
  when is_atom(PaymentType) ->
  MerList = maps:get(PaymentType, MerRouteMap),
  UpMerId = lists:nth(rand:uniform(length(MerList)), MerList),
  {reply, UpMerId, State};
handle_call({get_mer_id_default, PaymentType}, _From, #state{mer_router_map = MerRouteMap} = State)
  when is_atom(PaymentType) ->
  %% get the first mer in the list
  [UpMerIdDefault | _] = maps:get(PaymentType, MerRouteMap),
  {reply, UpMerIdDefault, State};
handle_call({get_mer_prop, MerId, Key}, _From, #state{mer_list_map = MerListMap} = State)
  when is_atom(Key) ->
  MerPropsMap = maps:get(MerId, MerListMap),
  Value = maps:get(Key, MerPropsMap, undefined),
  {reply, Value, State};
handle_call({get_config, Key}, _From, State) ->
  Return = do_get_config(Key, State),
  {reply, Return, State};
handle_call({check_bank_id, BankId}, _From, #state{bank_id_dict = BankIdDict} = State) ->
  Return = case dict:is_key(BankId, BankIdDict) of
             true ->
               ok;
             false ->
               error

           end,
  {reply, Return, State};
handle_call({get_all_mer}, _From, #state{mer_list_map = MerListMap} = State) ->
  MerAtomList = maps:keys(MerListMap),
  MerList = lists:map(fun(MerIdAtom) -> atom_to_binary(MerIdAtom, utf8) end, MerAtomList),
  {reply, lists:delete(?MER_ID_TEST, MerList), State};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% get all env from resource file
%% and convert to map
get_env() ->
  EnvList = application:get_all_env(),
  maps:from_list(EnvList).

get_route() ->
  {ok, UpMerList} = application:get_env(up_mer_list),
  maps:from_list(UpMerList).


get_mer_list() ->
  {ok, MerPropsMap} = application:get_env(up_mer_props),
%%  lager:debug("maps origal = ~p", [MerPropsMap]),
  F = fun
        (MerId, PropMap) when is_atom(MerId), is_map(PropMap) ->
%%          lager:debug("maps origal = ~p", [PropMap]),
          PrivateKey = load_private_key(MerId),
          MapsRet = maps:put(privateKey, PrivateKey, PropMap),
%%          lager:debug("maps with pk = ~p", [MapsRet]),
          MapsRet
      end,
  MerPropsMapWithPK = maps:map(F, MerPropsMap),
  MerPropsMapWithPK.


get_up_public_key() ->
  PublicKeyFileName = xfutils:get_filename([home, priv_dir, up_keys_dir, up_public_key_file]),
  lager:debug("PublicKeyFileName = ~p", [PublicKeyFileName]),
  {ok, PublicKey} = xfutils:load_public_key(PublicKeyFileName),
  PublicKey.

do_get_config(public_key, #state{public_key = PublicKey} = State) when is_record(State, state) ->
  PublicKey.

load_private_key(MerId) when is_atom(MerId) ->
  MerIdBin = atom_to_binary(MerId, utf8),
  KeyPath = xfutils:get_path([home, priv_dir, up_keys_dir]),
  KeyFileName = list_to_binary([KeyPath, MerIdBin, ".key"]),
  lager:debug("private key file name = ~p", [KeyFileName]),
  {ok, Pwd} = application:get_env(private_key_default_pwd),
  {ok, PrivateKey} = xfutils:load_private_key(KeyFileName, Pwd),
  PrivateKey.

get_bank_dict() ->
  {ok, BankIdList} = application:get_env(netbank_only_list_all),
  BankIdDict = new_bankid_dict(BankIdList),
  BankIdDict.

new_bankid_dict(BankIdList) when is_list(BankIdList) ->
  F = fun
        (BankId, Acc) ->
          [{BankId, ok} | Acc]
      end,

  BankIdPairList = lists:foldl(F, [], BankIdList),

  dict:from_list(BankIdPairList).

new_bankid_dict_test() ->
  BankIdList = ['AAA', 'BBB'],
  Dict = new_bankid_dict(BankIdList),
  List = dict:to_list(Dict),
  ?assertEqual([{'AAA', ok}, {'BBB', ok}], List),
  ok.