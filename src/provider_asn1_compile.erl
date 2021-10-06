-module('provider_asn1_compile').
-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, 'compile').
-define(DEPS, [{default, app_discovery}]).
-define(DEFAULTS, [{verbose, false}, {encoding, ber}, {compile_opts, []}]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},               % The 'user friendly' name of the task
            {module, ?MODULE},               % The module implementation of the task
            {namespace, asn},                % Compile resides in the asn namespace.
            {bare, true},                    % The task can be run by the user, always true
            {deps, ?DEPS},                   % The list of dependencies
            {example, "rebar3 asn compile"}, % How to use the plugin
            % list of options understood by the plugin
            {opts, [{verbose, $v, "verbose", boolean, "Be Verbose."},
                    {encoding, $e, "encoding", atom, "The encoding to use (atom). ber by default."},
                    {compile_opts, $o, "compile_opts", binary, "A comma-separated list of options to send to erlang's asn.1 compiler."}]},
            {short_desc, "Compile ASN.1 with Rebar3"},
            {desc, "Compile ASN.1 with Rebar3"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

resolve_special_args(PreState) ->
    NewState = provider_asn1_util:resolve_args(PreState, ?DEFAULTS),
    CompileOpts = provider_asn1_util:get_arg(NewState, compile_opts),
    if
        is_binary(CompileOpts) ->
            NewCompileOpts = lists:map(fun(X) ->
                                               binary_to_atom(X, utf8) end,
                                       re:split(CompileOpts, ",")),
            provider_asn1_util:set_arg(NewState, compile_opts, NewCompileOpts);
        true -> NewState
    end.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(PreState) ->
    State = resolve_special_args(PreState),
    Apps = lists:map(fun rebar_app_info:dir/1,
                     rebar_state:project_apps(State)),
    AllApps =
        case lists:member(rebar_state:dir(State), Apps) of
            true -> Apps;
            false -> [rebar_state:dir(State) | Apps]
        end,

    lists:foreach(fun (App) -> process_app(State, App) end, AllApps),
    {ok, State}.

process_app(State, AppPath) ->
    ASNPath = filename:join(AppPath, "asn1"),
    GenPath = filename:join(AppPath, "asngen"),
    IncludePath = filename:join(AppPath, "include"),
    SrcPath = filename:join(AppPath, "src"),
    ensure_dir(State, SrcPath),

    case to_recompile(ASNPath, GenPath) of
        [] ->
            ok;
        Asns ->
            ensure_dir(State, GenPath),
            ensure_dir(State, IncludePath),
            lists:foreach(fun(AsnFile) -> generate_asn(State, GenPath, AsnFile) end, Asns),
            move_asns(State, GenPath, SrcPath, IncludePath, Asns)
    end.

move_asns(State, GenPath, SrcPath, IncludePath, Asns) ->
    lists:foreach(
      fun(AsnFile) ->
              Base = filename:basename(AsnFile, ".asn1"),
              provider_asn1_util:move_file(State, GenPath, Base ++ ".erl", SrcPath),
              provider_asn1_util:delete_file(State, GenPath, Base ++ ".asn1db"),
              provider_asn1_util:move_file(State, GenPath, Base ++ ".hrl", IncludePath)
      end, Asns),
    ok = file:del_dir(GenPath).

format_error(Reason) ->
    provider_asn1_util:format_error(Reason).

generate_asn(State, Path, AsnFile) ->
    rebar_api:info("~s", [AsnFile]),
    Args = provider_asn1_util:get_args(State),
    provider_asn1_util:verbose_out(State, "Args: ~p", [Args]),
    Encoding = proplists:get_value(encoding, Args),
    Verbose = proplists:get_value(verbose, Args),
    CompileArgs = [verbose || Verbose] ++ [noobj, Encoding, {outdir, Path}]
        ++ proplists:get_value(compile_opts, Args),
    provider_asn1_util:verbose_out(State, "Beginning compile with opts: ~p", [CompileArgs]),
    case asn1ct:compile(AsnFile, CompileArgs) of
        {error, E} ->
            provider_asn1_util:verbose_out(State, "Error ~p compiling ASN1 ~p~n", [E, AsnFile]);
        ok ->
            ok
    end.

ensure_dir(State, Path) ->
    case filelib:is_dir(Path) of
        true ->
            ok;
        false ->
            provider_asn1_util:verbose_out(State, "Making ~p ~p~n", [Path, file:make_dir(Path)])
    end.

to_recompile(ASNPath, GenPath) ->
    lists:filtermap(fun (File) ->
                            is_latest(File, ASNPath, GenPath)
                    end,
                    find_asn_files(ASNPath)).

find_asn_files(Path) ->
    [filename:join(Path, F) || F <- filelib:wildcard("**/*.asn1", Path)].

is_latest(ASNFileName, ASNPath, GenPath) ->
    Source = filename:join(ASNPath, ASNFileName),
    TargetFileName = filename:basename(ASNFileName, ".asn1") ++ ".erl",
    Target = filename:join(GenPath, TargetFileName),
    filelib:last_modified(Source) > filelib:last_modified(Target).
