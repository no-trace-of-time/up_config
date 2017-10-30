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
-module(up_config_svr).
-include_lib("public_key/include/public_key.hrl").
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
  , check_payment_method/3
  , get_all_mer/0
  , get_mer_id_map/0

]).

-define(SERVER, ?MODULE).
-define(MER_ID_TEST, <<"test">>).

-record(state, {
  bank_id_dict
  , mer_router_map
  , mer_list_map
  , public_key
  , sens_public_key
  , encrypt_cert_id
  , sign_version
}).

%% test func
-export([
  get_up_sens_public_key_test_1/0
]).
%%-compile(export_all).

-define(APP, up_config).
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
    Key =:= privateKey orelse
    Key =:= publicKey
  )
  ->
  gen_server:call(?SERVER, {get_mer_prop, MerId, Key}).

get_config(Key) when is_atom(Key) ->
  gen_server:call(?SERVER, {get_config, Key}).


check_payment_method(PaymentType, BankId, CardNo) when is_atom(PaymentType) ->
  gen_server:call(?SERVER, {check_payment_method, PaymentType, BankId, CardNo}).

get_all_mer() ->
  gen_server:call(?SERVER, {get_all_mer}).


get_mer_id_map() ->
  gen_server:call(?SERVER, {get_mer_id_map}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  io:format("===================================", []),
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
    , sens_public_key = get_up_sens_public_key()
    , encrypt_cert_id = get_up_encrypt_cert_id()
    , sign_version = get_up_sign_version()
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
  {_, MerList} = maps:get(PaymentType, MerRouteMap),
  UpMerId = lists:nth(rand:uniform(length(MerList)), MerList),
  {reply, UpMerId, State};
handle_call({get_mer_id_default, PaymentType}, _From, #state{mer_router_map = MerRouteMap} = State)
  when is_atom(PaymentType) ->
  %% get the first mer in the list
  {_, [UpMerIdDefault | _]} = maps:get(PaymentType, MerRouteMap),
  {reply, UpMerIdDefault, State};
handle_call({get_mer_prop, MerId, Key}, _From, #state{mer_list_map = MerListMap} = State)
  when is_atom(Key) ->
  MerPropsMap = maps:get(MerId, MerListMap),
  Value = maps:get(Key, MerPropsMap, undefined),
  {reply, Value, State};
handle_call({get_config, Key}, _From, State) ->
  Return = do_get_config(Key, State),
  {reply, Return, State};
handle_call({check_payment_method, PaymentType, BankId, CardNo},
    _From,
    #state{mer_router_map = MerRouterMap, bank_id_dict = BankIdDict} = State) ->
  {PaymentMethod, _} = maps:get(PaymentType, MerRouterMap),
  Return = do_check_payment_method(PaymentMethod, {BankId, BankIdDict}, CardNo),

  {reply, Return, State};
handle_call({get_all_mer}, _From, #state{mer_list_map = MerListMap} = State) ->
  MerAtomList = maps:keys(MerListMap),
  MerList = lists:map(fun(MerIdAtom) -> atom_to_binary(MerIdAtom, utf8) end, MerAtomList),
  {reply, lists:delete(?MER_ID_TEST, MerList), State};
handle_call({get_mer_id_map}, _From, #state{mer_router_map = MerRouterMap} = State) ->
  F = fun
        (K, {_, [MerIdAtom]} = _V, Acc) ->
          MerBin = atom_to_binary(MerIdAtom, utf8),
          OldPaymentTypes = maps:get(MerBin, Acc, []),
          maps:put(MerBin, [K | OldPaymentTypes], Acc)
      end,

  MerIdMap = maps:fold(F, #{}, MerRouterMap),
  {reply, MerIdMap, State};

handle_call({get_public_key}, _From, #state{public_key = UpPublicKey} = State) ->
  {reply, UpPublicKey, State};

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
  {ok, UpMerList} = application:get_env(?APP, up_mer_list),
  maps:from_list(UpMerList).


get_mer_list() ->
  {ok, MerPropsMap} = application:get_env(?APP, up_mer_props),
  lager:debug("maps origal = ~p", [MerPropsMap]),
  F = fun
        (MerId, PropMap) when is_atom(MerId), is_map(PropMap) ->
          lager:debug("MerId = ~p,maps origal = ~p", [MerId, PropMap]),
          PrivateKey = load_private_key(MerId),
          PublicKey = load_public_key(MerId),
          MapsRet1 = maps:put(privateKey, PrivateKey, PropMap),
          MapsRet = maps:put(publicKey, PublicKey, MapsRet1),
          MapsRet
      end,
  MerPropsMapWithPK = maps:map(F, MerPropsMap),
  MerPropsMapWithPK.

get_keys_dir_config() ->
  {ok, UpKeysDirConfig} = application:get_env(?APP, up_keys_dir),
  UpKeysDirConfig.

get_up_public_key() ->
  PublicKeyFileName = xfutils:get_filename(get_keys_dir_config() ++ [up_public_key_file]),
  lager:debug("PublicKeyFileName = ~p", [PublicKeyFileName]),
  PublicKey = xfutils:load_public_key(PublicKeyFileName),
  PublicKey.

do_get_config(public_key, #state{public_key = Value} = State) when is_record(State, state) ->
  Value;
do_get_config(sens_public_key, #state{sens_public_key = Value} = State) when is_record(State, state) ->
  Value;
do_get_config(encrypt_cert_id, #state{encrypt_cert_id = Value} = State) when is_record(State, state) ->
  Value;
do_get_config(sign_version, #state{sign_version = Value} = State) when is_record(State, state) ->
  Value;
do_get_config(Key, _) when is_atom(Key) ->
  {ok, Value} = application:get_env(?APP, Key),
  Value.

%%--------------------------------------------------------------------
get_up_sens_public_key() ->
  SensCertFileName = xfutils:get_filename(?APP, get_keys_dir_config() ++ [up_senstive_info_key_file]),
  {ok, PemBin} = file:read_file(SensCertFileName),
  PemEntries = public_key:pem_decode(PemBin),
  {value, CertEntry} = lists:keysearch('Certificate', 1, PemEntries),
  {_, DerCert, _} = CertEntry,
  Decoded = public_key:pkix_decode_cert(DerCert, otp),
  PublicKey = Decoded#'OTPCertificate'.tbsCertificate#'OTPTBSCertificate'.subjectPublicKeyInfo#'OTPSubjectPublicKeyInfo'.subjectPublicKey,
  PublicKey.

get_up_sens_public_key_test_1() ->
  Exp = {'RSAPublicKey',
    26498026181949615086704577492793793837096612371985151086321328135922819361109340628655420170959038464619546023425402467728494575424306644941762193674523038253496443066500762638016208157503693033722201345670060030154552022430306033379156134032259876237571695399953075720784810879206128658921823604775469799580057927887903699759530869046017778223001727580248700229036435343237290475081383723336378046334266674778452008148955243850576767610192103008571774092703213471102192079603521811979763951695945896880111642868487339003629341023335340884665844452777420486215897391950795441041012780384297732635820627058446052697737,
    65537},
  ?assertEqual(Exp, get_up_sens_public_key()),
  ok.


%%--------------------------------------------------------------------
get_up_encrypt_cert_id() ->
  {ok, EncryptCertId} = application:get_env(?APP, encrypt_cert_id),
  EncryptCertId.
%%--------------------------------------------------------------------
get_up_sign_version() ->
  {ok, SignVersion} = application:get_env(?APP, sign_version),
  SignVersion.
%%--------------------------------------------------------------------
key_file_name(MerId, Type)
  when is_atom(MerId), is_atom(Type)
  , ((Type =:= private) or (Type =:= public)) ->
  MerIdBin = atom_to_binary(MerId, utf8),
  KeyPath = xfutils:get_path(get_keys_dir_config()),
  Ext = case Type of
          private ->
            ".key";
          public ->
            ".pub"
        end,
  KeyFileName = list_to_binary([KeyPath, MerIdBin, Ext]),
  KeyFileName.

%%--------------------------------------------------------------------
load_private_key(MerId) when is_atom(MerId) ->
  KeyFileName = key_file_name(MerId, private),
  lager:debug("private key file name = ~p", [KeyFileName]),
  {ok, Pwd} = application:get_env(?APP, private_key_default_pwd),
  PrivateKey = try
                 xfutils:load_private_key(KeyFileName, Pwd)
               catch
                 error:noent ->
                   lager:error("Could not file file ~p", [KeyFileName]),
                   <<>>;
                 _:_ ->
                   lager:error("load private key from ~p failed!set it to <<>>", [KeyFileName]),

                   <<>>
               end,
  PrivateKey.

%%--------------------------------------------------------------------
load_public_key(MerId) when is_atom(MerId) ->
  KeyFileName = key_file_name(MerId, public),
  lager:debug("public key file name = ~p", [KeyFileName]),
  PublicKey = try
                xfutils:load_public_key(KeyFileName, rsa)
              catch
                _:_ ->
                  lager:error("load public key from ~p failed!set it to <<>>", [KeyFileName]),
                  <<>>
              end,
  PublicKey.
%%--------------------------------------------------------------------
get_bank_dict() ->
  {ok, BankIdList} = application:get_env(?APP, netbank_only_list_all),
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

%%--------------------------------------------------------------------
do_check_payment_method(wap, _, _) ->
  ok;
do_check_payment_method(netbank, {Undefined, _}, _)
  when (Undefined =:= <<>>)
  or (Undefined =:= undefined) ->
  ok;
do_check_payment_method(netbank, {BankId, _}, _)
  when is_binary(BankId) ->
  error_bank_id_not_allowed;

do_check_payment_method(netbank_only, {BankId, BankIdDict}, CardNo)
  when is_binary(BankId) ->

  try

    BankIdAtom = binary_to_existing_atom(BankId, utf8),

    case dict:is_key(BankIdAtom, BankIdDict) of
      true ->
        case CardNo of
          <<>> ->
            ok;
          undefined ->
            ok;
          _ ->
            error_card_no_not_allowed
        end;
      false ->
        error_bank_id_error

    end

  catch
    _:_ ->
      error_bank_id_error

  end.


do_check_payment_method_test() ->
  BankIdList = [
    'ICBC'
    , 'ABC'
    , 'BOC'
    , 'BOCSH'
    , 'CCB'
%%          , 'CMB'
    , 'SPDB'
    , 'GDB'
    , 'BOCOM'
    , 'CNCB'
    , 'CMBC'
    , 'CIB'
    , 'CEB'
    , 'HXB'
    , 'BOS'
    , 'SPCB'
    , 'PSBC'
    , 'BCCB'
    , 'BRCB'
    , 'PAB'

%% debit only
    , 'ICBCD'
    , 'ABCD'
    , 'CCBD'
%%          , 'CMBD'
    , 'SPDBD'
    , 'GDBD'
    , 'CMBCD'
    , 'CEBD'
    , 'HXB'
    , 'PSBCD'
    , 'BOEAD'
  ],
  BankIdDict = new_bankid_dict(BankIdList),

  ?assertEqual(ok, do_check_payment_method(wap, {<<"ICBC">>, undefined}, <<"xxx">>)),

  ?assertEqual(ok, do_check_payment_method(netbank, {<<>>, undefined}, <<"xxx">>)),
  ?assertEqual(ok, do_check_payment_method(netbank, {undefined, undefined}, <<"xxx">>)),
  ?assertEqual(error_bank_id_not_allowed, do_check_payment_method(netbank, {<<"ICBC">>, undefined}, <<"xxx">>)),

  ?assertEqual(ok, do_check_payment_method(netbank_only, {<<"ICBC">>, BankIdDict}, undefined)),
  ?assertEqual(error_bank_id_error, do_check_payment_method(netbank_only, {<<"ICBD">>, BankIdDict}, <<"xxx">>)),
  ?assertEqual(ok, do_check_payment_method(netbank_only, {<<"ICBC">>, BankIdDict}, <<>>)),
  ?assertEqual(ok, do_check_payment_method(netbank_only, {<<"ICBC">>, BankIdDict}, undefined)),
  ?assertEqual(error_card_no_not_allowed, do_check_payment_method(netbank_only, {<<"ICBC">>, BankIdDict}, <<"xxx">>)),


  ok.
