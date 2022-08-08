-module(rebar3_efmt_prv).

-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, efmt).
-define(DEPS, [app_discovery]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},            % The 'user friendly' name of the task
            {module, ?MODULE},            % The module implementation of the task
            {bare, true},                 % The task can be run by the user, always true
            {deps, ?DEPS},                % The list of dependencies
            {example, "rebar3 efmt --write"}, % How to use the plugin
            {opts, opts()},                   % list of options understood by the plugin
            {short_desc, "Erlang code formatter"},
            {desc, "Erlang code formatter"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    ok = ensure_efmt_installed(),
    case is_update_check_enabled(State) of
        true ->
            ok = update_check();
        false ->
            ok
    end,
    Args = rebar_state:command_args(State),
    ok = rebar3_efmt_command:execute(Args -- ["--disable-update-check"]),
    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% ===================================================================
%% Private API
%% ===================================================================
-spec opts() -> [{atom(), char(), atom(), string()}].
opts() ->
    [
     {help, $h, "help", undefined, "Prints help information"},
     {version, $V, "version", undefined, "Prints version information"},
     {check, $c, "check", undefined,
      "Checks if input is formatted correctly. "
      "If so, exits with 0. Otherwise, exits with 1 and shows a diff."},
     {write, $w, "write", undefined, "Overwrites input file with the formatted text"},
     {show_files, undefined, "show-files", undefined, "Shows the target input files"},
     {exclude_files, undefined, "exclude-file", string,
      "Excludes files that matches the specified regexs from the default target file list."},
     {verbose, undefined, "verbose", undefined, "Outputs debug log messages"},
     {parallel, undefined, "parallel", undefined, "Executes formatting in parallel"},
     {include_dirs, $I, "include-search-dir", string,
      "Where to search for include files to process Erlang `-include` directives. "
      "If omitted, '../', '../include/', '../src/' and '../test/' of the target file will be added as the include directories"},
     {disable_include, undefined, "disable-include", undefined,
      "Disables `-include` and `-include_lib` processing. This could improve formatting speed. "
      "All unknown macros will be replaced with `EFMT_DUMMY` atom"},
     {disable_include_cache, undefined, "disable-include-cache", undefined,
      "Disables include cache"},
     {include_cache_dirs, undefined, "include-cache-dir", string,
      "Where to save the caches for the macro definitions collected during processing "
      "`-include` or `-include_lib` directives [default: .efmt/cache]"},
     {default_off, undefined, "default-off", string,
      "Disables formatting by default. "
      "efmt behaves as if there is a \"% @efmt:off\" comment at the head of the each target file."},
     {files, undefined, undefined, string,
      "Format target files. "
      "If no files are specified and any of `-c`, `-w` or `--show-files` options is specified, "
      "All of the files named `**.{hrl,erl,app.src}` and `**/rebar.config` are used as the default "
      "(note that files spcified by `.gitignore` will be ignored)"},
     {disable_update_check, undefined, "disable-update-check", undefined,
      "Stops issuing an HTTP GET request each command execution to check if a newer version has been released"}
    ].

-spec ensure_efmt_installed() -> ok.
ensure_efmt_installed() ->
    {ok, AppVersion} = application:get_key(rebar3_efmt, vsn),
    case rebar3_efmt_command:version() of
        {error, not_found} ->
            rebar_api:info("No `efmt` binary found. Trying installing a pre-built binary.", []),
            case rebar3_efmt_command:install_prebuilt_binary(AppVersion) of
                ok ->
                    ensure_efmt_installed();
                {error, Reason} ->
                    rebar_api:abort("Failed to install a pre-built binary (~p). "
                                    "Please visit https://github.com/sile/efmt, "
                                    "and install it manually.", [Reason])
            end;
        {error, timeout} ->
            rebar_api:abort("Failed to execute `efmt` command due to timeout.", []);
        {ok, AppVersion} ->
            ok;
        {ok, Version} ->
            rebar_api:warn("Version mismatch between rebar3_efmt (~p) and emft (~p). "
                           "Trying installing a correct pre-built binary.",
                           [AppVersion, Version]),
            case rebar3_efmt_command:install_prebuilt_binary(AppVersion) of
                ok ->
                    ensure_efmt_installed();
                {error, Reason} ->
                    rebar_api:warn("Failed to install a pre-built binary (~p). "
                                   "Keeps using the current binary. "
                                   "If you'd like to update it, "
                                   "please visit https://github.com/sile/efmt, "
                                   "and install it manually.", [Reason]),
                    ok
            end
    end.

-spec term_to_arg_value(term()) -> string().
term_to_arg_value(X) ->
    lists:flatten(io_lib:format("~p", [X])).

-spec atom_to_arg_key(atom()) -> string().
atom_to_arg_key(X) ->
    case atom_to_list(X) of
        [C] ->
            [$-, C];
        S ->
            "--" ++ string:replace(S, "_", "-")
    end.

-spec update_check() -> ok.
update_check() ->
    Url = "https://github.com/sile/efmt/releases/latest",
    case httpc:request(get, {Url, []}, [{autoredirect, false}, {ssl, [{log_level, error}]}], []) of
        {error, Reason} ->
            rebar_api:warn("Failed to check update due to HTTP error: url=~p, reason=~p", [Url, Reason]),
            ok;
        {ok, {{_, 302, _}, Header, _Body}} ->
            case proplists:get_value("location", Header) of
                "https://github.com/sile/efmt/releases/tag/" ++ LatestVersion ->
                    {ok, CurrentVersion} = application:get_key(rebar3_efmt, vsn),
                    case CurrentVersion =:= LatestVersion of
                        true ->
                            rebar_api:debug("The efmt version is up-to-date", []);
                        false ->
                            rebar_api:warn("A new release of rebar3_efmt is available: ~p -> ~p\n"
                                           "To use the latest version, please execute the following command:\n"
                                           "$ rebar3 plugins upgrade rebar3_efmt",
                                           [CurrentVersion, LatestVersion])
                    end,
                    ok;
                Location ->
                    rebar_api:warn("Unexpected location URL format: ~p", [Location]),
                    ok
            end;
        {ok, {Status, _Header, _Body}} ->
            rebar_api:warn("Expected HTTP 302 response from ~p, but got ~p", [Url, Status]),
            ok
    end.

-spec is_update_check_enabled(rebar_state:t()) -> boolean().
is_update_check_enabled(State) ->
    case lists:member("--disable-update-check", rebar_state:command_args(State)) of
        true ->
            false;
        false ->
            case lists:member(disable_update_check, rebar_state:get(State, efmt, [])) of
                true ->
                    false;
                false ->
                    true
            end
    end.
