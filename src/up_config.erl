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
  , check_payment_method/3
  , get_mer_list/0
  , get_mer_id_map/0

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
  MerId :: atom()| binary(),
  Key :: mer_prop_keys().

get_mer_prop(MerId, Key) when is_binary(MerId) ->
  get_mer_prop(binary_to_atom(MerId, utf8), Key);
get_mer_prop(MerId, Key) when is_atom(MerId),
  (Key =:= channelType orelse
    Key =:= certId orelse
    Key =:= privateKey orelse
  Key =:= publicKey
  )
  ->
  gws_up_config:get_mer_prop(MerId, Key).

get_config(Key) when is_atom(Key) ->
  gws_up_config:get_config(Key).

%%--------------------------------------------------------------------
-spec check_payment_method(PaymentType, BankIdBin, CardNo) -> Result when
  PaymentType :: atom(),
  BankIdBin :: binary()| undefined,
  CardNo :: binary()| undefined,
  Result :: ok| error.

check_payment_method(PaymentType, BankIdBin, CardNo) when is_atom(PaymentType) ->
  gws_up_config:check_payment_method(PaymentType, BankIdBin, CardNo).

%%--------------------------------------------------------------------
get_mer_list() ->
  gws_up_config:get_all_mer().

%%--------------------------------------------------------------------
get_mer_id_map() ->
  gws_up_config:get_mer_id_map().


%%====================================================================
%% Internal functions
%%====================================================================
