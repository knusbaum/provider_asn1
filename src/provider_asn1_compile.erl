-module('provider_asn1_compile').
-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, 'compile').
-define(DEPS, [{default, app_discovery}]).
-define(DEFAULTS, [{verbose, false}, {encoding, ber}, {compile_opts, []},
                   {compile_order, [{wildcard, "**/*.asn1"}]}]).

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
                    {compile_opts, $o, "compile_opts", binary,
                     "A comma-separated list of options to send to Erlang's ASN.1 compiler."},
                    {compile_order, $c, "compile_order", binary,
                     "An Erlang term consisting of a tuple-list of the specific order "
                     "to compile the ASN.1 files where the first tuple-element is "
                     "one of `wildcard' | `file' | `dir' and the second the filename "
                     "in string format. Defaults to `[{wildcard, \"**/*.asn1\"}]'."}
                   ]},
                    {overrides, $O, "overrides", binary,
                     "An Erlang term [{file | re, string}, [Opt]}] with specific options."}]},
            {short_desc, "Compile ASN.1 with Rebar3"},
            {desc, "Compile ASN.1 with Rebar3"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

resolve_special_args(PreState) ->
    DefaultState = provider_asn1_util:resolve_args(PreState, ?DEFAULTS),
    lists:foldl(fun (F, State) -> F(State) end,
                DefaultState,
                [fun resolve_compile_opts/1,
                 fun resolve_compile_order/1,
                 fun resolve_compile_overrides/1
                ]).

resolve_compile_opts(State) ->
    CompileOpts = provider_asn1_util:get_arg(State, compile_opts),
    if
        is_binary(CompileOpts) ->
            NewCompileOpts = lists:map(fun(X) ->
                                               binary_to_atom(X, utf8) end,
                                       re:split(CompileOpts, ",")),
            provider_asn1_util:set_arg(State, compile_opts, NewCompileOpts);
        true -> State
    end.
resolve_compile_order(State) ->
    CompileOrder = provider_asn1_util:get_arg(State, compile_order),
    if
        is_binary(CompileOrder) ->
            {ok, Toks, _} = erl_scan:string(binary_to_list(CompileOrder) ++ "."),
            {ok, NewCompileOrder} = erl_parse:parse_term(Toks),
            provider_asn1_util:set_arg(State, compile_order, NewCompileOrder);
        true -> State
    end.
resolve_compile_overrides(State) ->
    FileCompileOpts = provider_asn1_util:get_arg(State, overrides),
    if
        is_binary(FileCompileOpts) ->
            {ok, Toks, _} = erl_scan:string(binary_to_list(FileCompileOpts) ++ "."),
            {ok, NewFileCompileOpts} = erl_parse:parse_term(Toks),
            provider_asn1_util:set_arg(State, overrides, NewFileCompileOpts);
        true -> State
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

    case to_recompile(State, ASNPath, GenPath) of
        [] ->
            ok;
        Asns ->
            ensure_dir(State, GenPath),
            ensure_dir(State, IncludePath),
            lists:foreach(fun(AsnFile) -> generate_asn(State, GenPath, AsnFile, AppPath) end, Asns),
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

generate_asn(State, Path, AsnFile, AppPath) ->
    rebar_api:info("~s", [AsnFile]),
    Args = apply_file_overrides(AsnFile, provider_asn1_util:get_args(State)),
    provider_asn1_util:verbose_out(State, "Args: ~p", [Args]),
    Encoding = proplists:get_value(encoding, Args),
    Verbose = proplists:get_value(verbose, Args),
    CompileOpts = fix_paths(AppPath, proplists:get_value(compile_opts, Args)),
    CompileArgs = CompileOpts ++ [verbose || Verbose] ++ [noobj, Encoding, {outdir, Path}],
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

to_recompile(State, ASNPath, GenPath) ->
    lists:filtermap(fun (File) ->
                            is_latest(File, ASNPath, GenPath)
                    end,
                    find_asn_files(State, ASNPath)).

find_asn_files(State, BasePath) ->
    Order = provider_asn1_util:get_arg(State, compile_order),
    Fs = lists:flatmap(fun ({wildcard, Wildcard}) ->
                               [filename:join(BasePath, F) || F <- filelib:wildcard(Wildcard, BasePath)];
                           ({file, File}) ->
                               [filename:join(BasePath, File)];
                           ({dir, Dir}) ->
                               D = filename:join(BasePath, Dir),
                               [filename:join(D, F) || F <- filelib:wildcard("*.asn1", D)]
                       end, Order),
    uniq(Fs).

-ifdef(OTP_RELEASE).
  -if(?OTP_RELEASE >= 25).
uniq(Fs) ->
    lists:uniq(Fs).
  -elif(?OTP_RELEASE < 25).
uniq(Fs) ->
    Fs.
  -endif.
-else.
uniq(Fs) ->
    Fs.
-endif.

is_latest(ASNFileName, ASNPath, GenPath) ->
    Source = filename:join(ASNPath, ASNFileName),
    TargetFileName = filename:basename(ASNFileName, ".asn1") ++ ".erl",
    Target = filename:join(GenPath, TargetFileName),
    filelib:last_modified(Source) > filelib:last_modified(Target).

apply_file_overrides(File, Args) ->
    {[OverridesArgs], OtherArgs} = proplists:split(Args, [overrides]),
    case OverridesArgs of
        [{overrides, Overrides}] -> match_file_overrides(File, Overrides);
        []                            -> []
    end ++ OtherArgs.

match_file_overrides(_File, []) -> [];
match_file_overrides(File, [{{file, BName}, Args}|Rest]) ->
    case filename:basename(File) of
        BName -> Args;
        _     -> match_file_overrides(File, Rest)
    end;
match_file_overrides(File, [{{re, Pat}, Args}|Rest]) ->
    case re:run(File, Pat, [{capture, none}]) of
        nomatch -> match_file_overrides(File, Rest);
        match   -> Args
    end;
match_file_overrides(File, [ _ | Rest ]) ->
    match_file_overrides(File, Rest).

fix_paths(AppPath, Args) ->
    % there are issues when using relative paths in nested applications,
    % make them absolut be prepending the AppPath.
    lists:map(fun
        ({i, Path}) -> {i, filename:absname(Path, AppPath)};
        (Arg)       -> Arg
    end, Args).
