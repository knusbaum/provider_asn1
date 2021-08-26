-module('provider_asn1_compile').
-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-import(provider_asn1_util,
        [verbose_out/3,
         move_files/4,
         move_file/4,
         delete_files/3,
         delete_file/3,
         resolve_args/2,
         get_args/1,
         get_arg/2,
         set_arg/3]).

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
    NewState = resolve_args(PreState, ?DEFAULTS),
    CompileOpts = get_arg(NewState, compile_opts),
    if
        is_binary(CompileOpts) ->
            NewCompileOpts = lists:map(fun(X) ->
                                               binary_to_atom(X, utf8) end,
                                       re:split(CompileOpts, ",")),
            set_arg(NewState, compile_opts, NewCompileOpts);
        true -> NewState
    end.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(PreState) ->
    State = resolve_special_args(PreState),

    Apps = lists:map(fun (App) -> rebar_app_info:dir(App) end,
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

    case to_recompile(ASNPath, GenPath) of
        [] ->
            ok;
        Asns ->
            verbose_out(State, "    Asns: ~p", [Asns]),
            verbose_out(State, "Making ~p ~p~n", [GenPath, file:make_dir(GenPath)]),
            lists:foreach(fun(AsnFile) -> generate_asn(State, GenPath, AsnFile) end, Asns),

            verbose_out(State, "ERL files: ~p", [filelib:wildcard("*.erl", GenPath)]),
            move_files(State, GenPath, SrcPath, "*.erl"),

            verbose_out(State, "DB files: ~p", [filelib:wildcard("*.asn1db", GenPath)]),
            move_files(State, GenPath, SrcPath, "*.asn1db"),

            verbose_out(State, "HEADER files: ~p", [filelib:wildcard("*.hrl", GenPath)]),
            move_files(State, GenPath, IncludePath, "*.hrl"),

            ok
    end.

format_error(Reason) ->
    provider_asn1_util:format_error(Reason).

find_asn_files(Path) ->
    [filename:join(Path, F) || F <- filelib:wildcard("**/*.asn1", Path)].

generate_asn(State, Path, AsnFile) ->
    rebar_api:info("Generating ASN.1 files.", []),
    Args = get_args(State),
    verbose_out(State, "Args: ~p", [Args]),
    Encoding = proplists:get_value(encoding, Args),
    CompileArgs =
        case proplists:get_value(verbose, Args) of
            true -> [Encoding, verbose, {outdir, Path}];
            _ -> [Encoding, {outdir, Path}]
        end ++ proplists:get_value(compile_opts, Args),
    verbose_out(State, "Beginning compile with opts: ~p", [CompileArgs]),
    asn1ct:compile(AsnFile, CompileArgs).

to_recompile(ASNPath, GenPath) ->
    case find_asn_files(ASNPath) of
        [] ->
            [];
        ASNFileNames ->
            case is_updated(ASNFileNames, ASNPath, GenPath) of
                true -> ASNFileNames;
                false -> []
            end
    end.

is_updated([], _, _) ->
    false;
is_updated([ASNFileName | RestFiles], ASNPath, GenPath) ->
    Source = filename:join(ASNPath, ASNFileName),
    TargetFileName = filename:basename(ASNFileName, ".asn1") ++ ".erl",
    Target = filename:join(GenPath, TargetFileName),
    case filelib:last_modified(Source) > filelib:last_modified(Target) of
        true ->
            true;
        false ->
            is_updated(RestFiles, ASNPath, GenPath)
    end.
