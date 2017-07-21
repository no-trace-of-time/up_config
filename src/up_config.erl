%%%-------------------------------------------------------------------
%% @doc up_config public API
%% @end
%%%-------------------------------------------------------------------

-module(up_config).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================
-export([
  get_mer_id/1
  , get_mer_id_default/1
  , get_mer_prop/2
  , get_config/1
  , check_bank_id/2
  , get_mer_list/0

]).

-type mer_prop_keys() :: certId|channelType | privateKey.
-export_type([mer_prop_keys/0]).

%%--------------------------------------------------------------------
start(_StartType, _StartArgs) ->
  up_config_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
  ok.

get_mer_id(PaymentType) when is_atom(PaymentType) ->
  gws_up_config:get_mer_id(PaymentType).

get_mer_id_default(PaymentType) when is_atom(PaymentType) ->
  gws_up_config:get_mer_id_default(PaymentType).

%%--------------------------------------------------------------------
-spec get_mer_prop(MerId, Key) -> any() when
  MerId :: atom(),
  Key :: mer_prop_keys().

get_mer_prop(MerId, Key) when is_atom(MerId),
  (Key =:= channelType orelse
    Key =:= certId orelse
    Key =:= privateKey
  )
  ->
  gws_up_config:get_mer_prop(MerId, Key).

get_config(Key) when is_atom(Key) ->
  gws_up_config:get_config(Key).

check_bank_id(gw_netbank, undefined) ->
  ok;
check_bank_id(gw_netbank, <<>>) ->
  ok;
check_bank_id(gw_netbank, _) ->
  %% not allow bank_id for gw_netbank
  %% tempory for minsheng huidong merchant
  %% only allow minsheng
  error;
check_bank_id(gw_wap, _BankId) ->
  ok;
check_bank_id(gw_netbank_only, BankIdBin) when is_binary(BankIdBin) ->
  BankId = binary_to_existing_atom(BankIdBin, utf8),
  gws_up_config:check_bank_id(BankId).

get_mer_list() ->
  gws_up_config:get_all_mer().

%%====================================================================
%% Internal functions
%%====================================================================
