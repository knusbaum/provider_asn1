-module(provider_asn1_clean).
-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-import(provider_asn1_util, 
        [verbose_out/3,
         move_files/4,
         move_file/4,
         delete_files/3,
         delete_file/3,
         resolve_args/2]).

-define(PROVIDER, 'clean').
-define(DEPS, [{default, app_discovery}]).
-define(DEFAULTS, [{verbose, false}]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},             % The 'user friendly' name of the task
            {module, ?MODULE},             % The module implementation of the task
            {namespace, asn},              % Compile resides in the asn namespace.
            {bare, true},                  % The task can be run by the user, always true
            {deps, ?DEPS},                 % The list of dependencies
            {example, "rebar3 asn clean"}, % How to use the plugin
            % list of options understood by the plugin
            {opts, [{verbose, $v, "verbose", boolean, "Be Verbose."}]},
            {short_desc, "Clean up ASN.1 generated files."},
            {desc, "Clean up ASN.1 generated files."}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(PreState) ->
    State = resolve_args(PreState, ?DEFAULTS),
    Apps = lists:map(fun (App) -> rebar_app_info:dir(App) end,
                     rebar_state:project_apps(State)),
    AllApps =
        case lists:member(rebar_state:dir(State), Apps) of
            true -> Apps;
            false -> [rebar_state:dir(State) | Apps]
        end,

    lists:foreach(fun (App) -> do_clean(State, App) end, AllApps),
    {ok, State}.


do_clean(State, AppPath) ->
    rebar_api:info("Cleaning ASN.1 generated files.", []),

    GenPath = filename:join(AppPath, "asngen"),
    IncludePath = filename:join(AppPath, "include"),
    SrcPath = filename:join(AppPath, "src"),

    ErlFiles = filelib:wildcard("*.erl", GenPath),
    verbose_out(State, "Erl files: ~p", [ErlFiles]),
    lists:foreach(fun(File) ->
                          delete_file(State, SrcPath, File)
                  end, ErlFiles),

    HrlFiles = filelib:wildcard("*.hrl", GenPath),
    verbose_out(State, "Hrl files: ~p", [HrlFiles]),
    lists:foreach(fun(File) ->
                          delete_file(State, IncludePath, File)
                  end, HrlFiles),

    DBFiles = filelib:wildcard("*.asn1db", GenPath),
    verbose_out(State, "DB files: ~p", [DBFiles]),
    lists:foreach(fun(File) ->
                          delete_file(State, SrcPath, File)
                  end, DBFiles),

    delete_files(State, GenPath, "*"),

    ok.

format_error(Reason) ->
    provider_asn1_util:format_error(Reason).
