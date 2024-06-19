-module(rebar3_efmt_command).

-include_lib("kernel/include/file.hrl").

-export([version/0,
         install_prebuilt_binary/1,
         execute/1]).


-spec version() -> {ok, string()} | {error, term()}.
version() ->
    case command_path() of
        error ->
            {error, not_found};
        {ok, Path} ->
            Port = erlang:open_port({spawn_executable, Path}, [{args, ["--version"]}, exit_status]),
            case collect_port_output(Port, []) of
                {error, Reason} ->
                    {error, Reason};
                {ok, "efmt" ++ Version} ->
                    {ok, string:trim(Version)}
            end
    end.


-spec collect_port_output(port(), iodata()) -> {ok, string()} | {error, term()}.
collect_port_output(Port, Acc) ->
    receive
        {Port, {data, Data}} ->
            collect_port_output(Port, [Acc | Data]);
        {Port, {exit_status, 0}} ->
            {ok, lists:flatten(Acc)};
        {Port, {exit_status, Status}} ->
            {error, {error_exit_status, Status}}
    after
        1000 ->
            {error, timeout}
    end.


-spec execute([string()]) -> ok.
execute(Args) ->
    {ok, Path} = command_path(),
    Port = erlang:open_port({spawn_executable, Path}, [{args, Args}, exit_status]),
    execute_output(Port).


-spec execute_output(port()) -> ok.
execute_output(Port) ->
    receive
        {Port, {data, Data}} ->
            io:format("~s", [Data]),
            execute_output(Port);
        {Port, {exit_status, 0}} ->
            ok;
        {Port, {exit_status, Status}} ->
            erlang:halt(Status);
        Other ->
            rebar_api:abort("Received an unexpected message: ~p", [Other])
    after
        60000 ->
            rebar_api:abort("Timeout", [])
    end.


-spec install_prebuilt_binary(string()) -> ok | {error, term()}.
install_prebuilt_binary(Version) ->
    case get_prebuilt_binary_arch() of
        error ->
            {error, {no_prebuilt_binary, rebar_api:get_arch()}};
        {ok, Arch} ->
            rebar_api:debug("Arch: ~p", [Arch]),
            Url = "https://github.com/sile/efmt/releases/download/" ++ Version ++
                "/efmt-" ++ Version ++ "." ++ Arch,
            rebar_api:info("Pre-built binary URL: ~p", [Url]),
            case httpc:request(get, {Url, []}, [{ssl, [{verify, verify_none}]}], []) of
                {error, Reason} ->
                    {error, {http_get_failed, Reason}};
                {ok, {{_, 200, _}, _Header, Body}} ->
                    Path = code:priv_dir(rebar3_efmt) ++ "/efmt",
                    case file:write_file(Path, Body, [binary]) of
                        {error, Reason} ->
                            {error, {write_file_failed, Reason}};
                        ok ->
                            case file:write_file_info(Path, #file_info{mode = 8#00774}) of
                                {error, Reason} ->
                                    {error, {write_file_info_failed, Reason}};
                                ok ->
                                    ok
                            end
                    end;
                {ok, {Status, _Header, Body}} ->
                    {error, {Status, Body}}
            end
    end.


-spec command_path() -> {ok, string()} | error.
command_path() ->
    case os:find_executable("efmt", code:priv_dir(rebar3_efmt)) of
        false ->
            case os:find_executable("efmt") of
                false ->
                    error;
                Path ->
                    rebar_api:debug("Found `efmt`: ~p", [Path]),
                    {ok, Path}
            end;
        Path ->
            rebar_api:debug("Found `efmt`: ~p", [Path]),
            {ok, Path}
    end.


-spec get_prebuilt_binary_arch() -> {ok, string()} | error.
get_prebuilt_binary_arch() ->
    Arch = rebar_api:get_arch(),
    Candidates = [{"x86_64-unknown-linux-musl", ".*x86_64.*linux.*"},
                  {"x86_64-apple-darwin", ".*x86_64.*apple-darwin.*"},
                  {"aarch64-apple-darwin", ".*aarch64.*apple-darwin.*"}],
    case [ K || {K, V} <- Candidates, re:run(Arch, V) =/= nomatch ] of
        [] ->
            error;
        [K] ->
            {ok, K}
    end.
