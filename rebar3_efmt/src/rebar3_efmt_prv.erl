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
    Args = rebar_state:command_args(State),
    ok = rebar3_efmt_command:execute(Args),
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
     {print_width, undefined, "print-width", integer,
      "Maximum line length. "
      "Note that this is a soft limit. "
      "Ths is, some lines could exceed the limit after formatting. "
      "Besides, this limit doesn't apply to comments. [default: 120]"},
     {verbose, undefined, "verbose", undefined, "Outputs debug log messages"},
     {parallel, undefined, "parallel", undefined, "Executes formatting in parallel"},
     {disable_include, undefined, "disable-include", undefined,
      "Disables `-include` and `-include_lib` processing. This could improve formatting speed. "
      "All unknown macros will be replaced with `EFMT_DUMMY` atom"},
     {disable_include_cache, undefined, "disable-include-cache", undefined,
      "Disables include cache"},
     {include_cache_dirs, undefined, "include-cache-dirs", string,
      "Where to save the caches for the macro definitions collected during processing "
      "`-include` or `-include_lib` directives [default: .efmt/cache]"},
     {files, undefined, undefined, string,
      "Format target files. "
      "If no files are specified and either `-c` or `-w` options is specified, "
      "`*.{hrl,erl,app.src}` and `rebar.config` files in your application are used as the default."
     }
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