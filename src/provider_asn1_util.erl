-module(provider_asn1_util).

-export([verbose_out/3,
         format_error/1,
         move_file/4,
         delete_file/3,
         resolve_args/2,
         get_args/1,
         get_arg/2,
         set_arg/3]).

verbose_out(State, FormatString, Args)->
    CommArgs = get_args(State),
    case proplists:get_value(verbose, CommArgs) of
        true ->
            rebar_api:info(FormatString, Args);
        _ ->
            ok
    end.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

move_file(State, SrcPath, File, DestPath) ->
    F = filename:join(SrcPath, File),
    case filelib:is_file(F) of
        true ->
            Dest = filename:join(DestPath, File),
            verbose_out(State, "Moving: ~p", [F]),
            verbose_out(State, "~p", [ok = file:rename(F, Dest)]);
        false ->
            verbose_out(State, "Not a file: ~p", [F]),
            ok
    end.

delete_file(State, In, File) ->
    F = filename:join(In, File),
    delete_file(State, F).

delete_file(State, F) ->
    verbose_out(State, "Deleting: ~p", [F]),
    case file:delete(F) of
        ok ->
            verbose_out(State, "ok", []);
        {error, enoent} ->
            verbose_out(State, "ok - enoent", [])
    end.

resolve_args(State, Defaults) ->
    {PArgs, _} = rebar_state:command_parsed_args(State),
    Config = rebar_state:get(State, asn1_args, []),

    PArgsMap = maps:from_list(PArgs),
    ConfigMap = maps:from_list(Config),
    DefaultsMap = maps:from_list(Defaults),

    % Defaults overridden by Config overridden by PArgs (command-line)
    ResolvedMap = maps:merge(maps:merge(DefaultsMap, ConfigMap), PArgsMap),
    ResolvedArgs = maps:to_list(ResolvedMap),
    rebar_state:set(State, asn1_args, ResolvedArgs).

get_args(State) ->
    rebar_state:get(State, asn1_args, []).

get_arg(State, Key) ->
    Args = rebar_state:get(State, asn1_args, []),
    proplists:get_value(Key, Args).

set_arg(State, Key, Val) ->
    Args = rebar_state:get(State, asn1_args, []),
    ArgsMap = maps:from_list(Args),
    rebar_state:set(State, asn1_args, maps:to_list(maps:put(Key, Val, ArgsMap))).
