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
        {efmt, {git, "https://host/user/efmt.git", {tag, "0.1.0"}}}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 efmt
    ===> Fetching efmt
    ===> Compiling efmt
    <Plugin Output>
