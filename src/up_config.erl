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

%%====================================================================
%% Internal functions
%%====================================================================
