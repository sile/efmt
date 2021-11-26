-module(efmt_prv).

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
    Args = rebar_state:command_parsed_args(State),
    Arch = rebar_api:get_arch(),
    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% ===================================================================
%% Private API
%% ===================================================================
-spec opts() -> [getopt:option_spec()].
opts() ->
    [
     {help, $h, "help", undefined, "Prints help information"},
     {version, $V, "version", undefined, "Prints version information"},
     {check, $c, "check", undefined,
      "Checks if input is formatted correctly. "
      "If so, exits with 0. Otherwise, exits with 1 and shows a diff."},
     {write, $w, "write", undefined, "Overwrites input file with the formatted text"},
     {include_search_dirs, $I, "include-search-dirs", string, % TODO: remove (read from rebar.config)
      "Where to search for include files to process Erlang `-include` directives"},
     {print_width, undefined, "print-width", integer,
      "Maximum line length. "
      "Note that this is a soft limit. "
      "Ths is, some lines could exceed the limit after formatting. "
      "Besides, this limit doesn't apply to comments. [default: 120]"},
     {verbose, undefined, "verbose", "Outputs debug log messages"},
     {parallel, undefined, "parallel", "Executes formatting in parallel"},
     {disable_include, undefined, "disable-include", undefined,
      "Disables `-include` and `-include_lib` processing. This could improve formatting speed. "
      "All unknown macros will be replaced with `EFMT_DUMMY` atom"},
     {disable_include_cache, undefined, "disable-include-cache", undefined,
      "Disables include cache"},
     {include_cache_dirs, undefined, "include-cache-dirs", string,
      "Where to save the caches for the macro definitions collected during processing "
      "`-include` or `-include_lib` directives [default: .efmt/cache]"},
     {files, undefined, undefined, string, % TODO: change the default
      "Format target files. "
      "`-` means the standard input. "
      "If no files are specified and either `-c` or `-w` options is specified, "
      "`{src,include,test}/*.{hrl,erl,app.src}` and `rebar.config` are used as the default."
     }
    ].
