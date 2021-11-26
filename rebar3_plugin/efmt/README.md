efmt
=====

Erlang Code Formatter

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [
        {efmt, {git_subdir, "https://host/user/efmt.git", "master", "rebar3_plugin"}}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 efmt
    ===> Fetching efmt
    ===> Compiling efmt
    <Plugin Output>
