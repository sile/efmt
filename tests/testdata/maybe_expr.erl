%%---10--|----20---|----30---|----40---|----50---|
%%
%% The functions below is quoted from https://github.com/erlang/eep/blob/master/eeps/eep-0049.md
-module(maybe_expr).


commit_write(OpaqueData) ->
    maybe
        ok ?=
            disk_log:sync(OpaqueData#backup.file_desc),
        ok ?=
            disk_log:close(OpaqueData#backup.file_desc),
        ok ?=
            file:rename(OpaqueData#backup.tmp_file,
                        OpaqueData#backup.file),
        {ok, OpaqueData#backup.file}
    end.


commit_write(OpaqueData) ->
    maybe
        ok ?=
            disk_log:sync(OpaqueData#backup.file_desc),
        ok ?=
            disk_log:close(OpaqueData#backup.file_desc),
        ok ?=
            file:rename(OpaqueData#backup.tmp_file,
                        OpaqueData#backup.file),
        {ok, OpaqueData#backup.file}
    else
        {error, Reason} ->
            {error, Reason}
    end.


-spec fetch() -> {ok, iodata()} | {error, _}.
fetch() ->
    maybe
        {ok, B = <<_/binary>>} ?= f(),
        true ?= validate(B),
        {ok, sanitize(B)}
    else
        false ->
            {error, invalid_data};
        {error, R} ->
            {error, R}
    end.
