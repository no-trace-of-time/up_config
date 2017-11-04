%%%-------------------------------------------------------------------
%%% @author simon
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. 十月 2017 14:41
%%%-------------------------------------------------------------------
-module(up_config_SUITE).
-include_lib("eunit/include/eunit.hrl").
-author("simon").

%% API
-export([]).

setup() ->
  ok = application:start(compiler),
  ok = application:start(syntax_tools),
  ok = application:start(goldrush),
  ok = application:start(lager),
  ok = application:start(up_config),

  env_init(),
  ok.


env_init() ->
  Cfgs = [
    {up_config,
      [
        {priv_dir, "/priv"}
        , {up_keys_dir_sub, "/keys"}
        , {up_keys_dir, [priv, up_keys_dir_sub]}
        , {up_public_key_file, "acp.pem"}
        , {up_senstive_info_key_file, "sens_enc.cer"}
      ]
    }
  ],
  pg_test_utils:env_init(Cfgs),
  ok.

my_test_() ->
  {
    setup,
    fun setup/0,
    {
      inorder,
      [
        fun get_mer_prop_test_1/0,
        fun up_config_svr:get_up_sens_public_key_test_1/0,
%%        , fun up_config:get_config_test_1/0
        fun sign_verify_test_1/0
      ]
    }
  }.

get_mer_prop_test_1() ->
  ?assertEqual(<<"07">>, up_config:get_mer_prop('777290058110097', channelType)),
  ok.

sign_verify_test_1() ->
  ?assertEqual(true, sign_verify_mer('777290058110097')),
  ?assertEqual(true, sign_verify_mer('898319849000017')),
  ok.

sign_verify_mer(MerId) when is_atom(MerId) ->
  PK = up_config:get_mer_prop(MerId, privateKey),
  PubK = up_config:get_mer_prop(MerId, publicKey),
  Msg = <<"aaa">>,
  %% digest
  DigestBin = crypto:hash('sha',Msg),
  DigestHex = xfutils:bin_to_hex(DigestBin),
  DigestString = list_to_binary(string:to_lower(binary_to_list(DigestHex))),
  Sig = public_key:sign(DigestString, 'sha', PK),
  public_key:verify(DigestString, sha, Sig, PubK).
