![test workflow](https://github.com/reachfh/logger_formatter_json/actions/workflows/test.yml/badge.svg)
[![Contributor Covenant](https://img.shields.io/badge/Contributor%20Covenant-2.1-4baaaa.svg)](CODE_OF_CONDUCT.md)

# opentelemetry_xray

[OpenTelemetry](https://opentelemetry.io/) [AWS X-Ray](https://aws.amazon.com/xray/) support for Erlang/Elixir.

This library includes two modules:

* An id generator that creates X-Ray-compatible `trace_id` and `span_id`.
  It implements the `otel_id_generator` protocol in the Erlang SDK.

* A propagator that reads and writes AWS X-Ray trace context headers.
  It implements the `otel_propagator_text_map` protocol in the Erlang SDK.

It assumes that you are using the
[AWS Distro for OpenTelemetry Collector](https://aws-otel.github.io/docs/getting-started/collector),
a version of the [OpenTelemetry Collector](https://opentelemetry.io/docs/collector/)
which has support for AWS services such as X-Ray. It accepts standard
OpenTelemetry traces, converts them to X-Ray format, and sends them to AWS.
You can run the collector as a sidecar container in an ECS task or as a daemon
on an EC2 instance.

In AWS X-Ray, the `trace_id` is a 128-bit value. The first 32 bits are a Unix
`time_t` and the rest are a 96-bit random number. If you use the default
`trace_id`, then X-Ray will reject your traces. This library generates ids that
are compatible with X-Ray.

If your app is running behind an AWS Application Load Balancer, then the ALB
will pass a trace in the `X-Amzn-Trace-Id` header. This library includes a
propagator which reads the trace id from this header and uses it within your app.
It can then pass the same trace id to downstream apps via the header.

Links:

* Propagator: https://opentelemetry.io/docs/specs/otel/context/api-propagators/
* https://docs.aws.amazon.com/xray/latest/devguide/xray-concepts.html#xray-concepts-tracingheader
* https://aws-otel.github.io/docs/getting-started/x-ray#configuring-the-aws-x-ray-exporter
* https://davelucia.com/blog/observing-elixir-with-lightstep
* https://opentelemetry.io/docs/instrumentation/erlang/getting-started/

## Installation

Erlang:

Add `opentelemetry_xray` to the list of dependencies in `rebar.config`:

```erlang
{deps, [opentelemetry_xray]}.
```

Elixir:

Add `logger_formatter_json` to the list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:opentelemetry_xray, "~> 0.7"},
  ]
end
```

## Configuration

In `config/config.exs` or `config/prod.exs`, configure `opentelemetry` to use this library:

```elixir
config :opentelemetry,
  id_generator: :opentelemetry_xray_id_generator,
  propagators: [:opentelemetry_xray_propagator, :baggage]
```

See [phoenix_container_example](https://github.com/cogini/phoenix_container_example)
for a complete Elixir Phoenix app that uses this library.
