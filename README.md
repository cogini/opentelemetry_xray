![test workflow](https://github.com/reachfh/logger_formatter_json/actions/workflows/test.yml/badge.svg)
[![Contributor Covenant](https://img.shields.io/badge/Contributor%20Covenant-2.1-4baaaa.svg)](CODE_OF_CONDUCT.md)

# otel_propagator_xray

An implementation of {@link otel_propagator_text_map} that injects and
extracts trace context from AWS X-Ray.

https://docs.aws.amazon.com/xray/latest/devguide/xray-concepts.html#xray-concepts-tracingheader

Since `trace_context' and `baggage' are the two default propagators the
global TextMap Propagators must be configured.

## Configuration

```erlang
{text_map_propagators, [xray, baggage]},
```

## Contributing

Build:

```console
rebar3 compile
```

Test:

```console
rebar3 ct
```

Format code:

```console
rebar3 steamroll
```

Generate docs:

```console
rebar3 ex_docs
```

Publish:

```console
rebar3 hex user auth
rebar3 hex build
rebar3 hex publish
```

This project uses the Contributor Covenant version 2.1. Check [CODE_OF_CONDUCT.md](/CODE_OF_CONDUCT.md) for more information.