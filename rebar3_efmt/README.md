rebar3_efmt
===========

[![hex.pm version](https://img.shields.io/hexpm/v/rebar3_efmt.svg)](https://hex.pm/packages/rebar3_efmt)
[![License: Apache-2.0](https://img.shields.io/badge/license-Apache2-blue.svg)](LICENSE)

Erlang Code Formatter

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [rebar3_efmt]}.

Then just call your plugin directly in an existing application:


    $ rebar3 efmt
    ===> Fetching efmt
    ===> Compiling efmt
    <Plugin Output>
