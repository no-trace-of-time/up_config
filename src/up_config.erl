%%%-------------------------------------------------------------------
%% @doc up_config public API
%% @end
%%%-------------------------------------------------------------------
-module(up_config).
-include_lib("eunit/include/eunit.hrl").

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


-export([
  get_config_test_1/0

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

%%--------------------------------------------------------------------
get_config(Key) when is_atom(Key) ->
  gws_up_config:get_config(Key).


get_config_test_1() ->
  ?assertEqual({'RSAPublicKey',
    26498026181949615086704577492793793837096612371985151086321328135922819361109340628655420170959038464619546023425402467728494575424306644941762193674523038253496443066500762638016208157503693033722201345670060030154552022430306033379156134032259876237571695399953075720784810879206128658921823604775469799580057927887903699759530869046017778223001727580248700229036435343237290475081383723336378046334266674778452008148955243850576767610192103008571774092703213471102192079603521811979763951695945896880111642868487339003629341023335340884665844452777420486215897391950795441041012780384297732635820627058446052697737,
    65537}, get_config(sens_public_key)),
  ok.
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
