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
  application:start(up_config),

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
        fun up_config_gen_server:get_up_sens_public_key_test_1/0
%%        , fun up_config:get_config_test_1/0
      ]
    }
  }.