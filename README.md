![test workflow](https://github.com/cogini/opentelemetry_xray/actions/workflows/test.yml/badge.svg)
[![Module Version](https://img.shields.io/hexpm/v/opentelemetry_xray.svg)](https://hex.pm/packages/opentelemetry_xray)
[![Hex Docs](https://img.shields.io/badge/hex-docs-lightgreen.svg)](https://hexdocs.pm/opentelemetry_xray)
[![Total Download](https://img.shields.io/hexpm/dt/opentelemetry_xray.svg)](https://hex.pm/packages/opentelemetry_xray)
[![License](https://img.shields.io/hexpm/l/opentelemetry_xray.svg)](https://github.com/cogini/opentelemetry_xray/blob/master/LICENSE.md)
[![Last Updated](https://img.shields.io/github/last-commit/cogini/opentelemetry_xray/main)](https://github.com/cogini/opentelemetry_xray/commits/main)
[![Contributor Covenant](https://img.shields.io/badge/Contributor%20Covenant-2.1-4baaaa.svg)](CODE_OF_CONDUCT.md)

# opentelemetry_xray

[OpenTelemetry](https://opentelemetry.io/) [AWS X-Ray](https://aws.amazon.com/xray/) support for Erlang/Elixir.

It works with the [AWS Distro for OpenTelemetry Collector](https://aws-otel.github.io/docs/getting-started/collector),
a version of the [OpenTelemetry Collector](https://opentelemetry.io/docs/collector/)
which has support for AWS services such as X-Ray. This collector accepts standard
OpenTelemetry traces, converts them to X-Ray format, and sends them to AWS. You
can run the collector as a sidecar container in an ECS task or as a daemon
on an EC2 instance.

In AWS X-Ray, the `trace_id` is a 128-bit value. The first 32 bits are a Unix
`time_t` and the rest are a 96-bit random number. If you use the default
`trace_id`, then X-Ray will reject your traces. This library generates ids that
are compatible with X-Ray.

When your app app is downstream from another app or the AWS load balancer, the
upstream app creates the trace for the current request and sends it in the
`X-Amzn-Trace-Id` HTTP header. The header includes the trace id and optional
information about the parent span and sampling.

This library includes a propagator which reads the trace id from this header
and uses it within your app. It can then pass the same trace id to downstream
apps via the header.

NOTE: By default Amazon samples relatively few traces. If you want to ensure
that your traces are sampled, make sure that you turn on sampling in your app.
A common approach is to turn on sampling for all traces that have errors
and for some percentage of normal traces.

This library includes the following modules:

* An id generator that creates X-Ray-compatible `trace_id` and `span_id`.
  It implements the `otel_id_generator` protocol in the Erlang SDK.

* A propagator that reads and writes AWS X-Ray trace context headers.
  It implements the `otel_propagator_text_map` protocol in the Erlang SDK.

Links:

* Propagators in general: https://opentelemetry.io/docs/specs/otel/context/api-propagators/
* Erlang SDK propagation: https://opentelemetry.io/docs/instrumentation/erlang/propagation/
* Erlang SDK id generator: https://github.com/open-telemetry/opentelemetry-erlang/blob/main/apps/opentelemetry/src/otel_id_generator.erl
* X-Ray tracing header: https://docs.aws.amazon.com/xray/latest/devguide/xray-concepts.html#xray-concepts-tracingheader
* X-Ray sampling: https://docs.aws.amazon.com/xray/latest/devguide/xray-concepts.html#xray-concepts-sampling
* X-Ray configuration: https://aws-otel.github.io/docs/getting-started/x-ray#configuring-the-aws-x-ray-exporter
* AWS CloudWatch Log Group resource: https://github.com/aws/aws-xray-sdk-python/issues/188
* OpenTelemetry resources: https://opentelemetry.io/docs/instrumentation/erlang/resources/
* OpenTelemetry getting started: https://opentelemetry.io/docs/instrumentation/erlang/getting-started/
* OpenTelemetry intro: https://davelucia.com/blog/observing-elixir-with-lightstep
* Addding Erlang logger filters to new Elixir logger: https://write.as/yuriploc/elixir-logger-and-erlang-filters

## Installation

Erlang:

Add `opentelemetry_xray` to the list of dependencies in `rebar.config`:

```erlang
{deps, [opentelemetry_xray]}.
```

Elixir:

Add `opentelemetry_xray` to the list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:opentelemetry_xray, "~> 0.7"},
  ]
end
```

## Configuration

Erlang:

In `sys.config`:

```erlang
{opentelemetry, {
    id_generator, [opentelemetry_xray_id_generator],
    propagators: [opentelemetry_xray_propagator, baggage]
}}
```

Elixir:

In `config/prod.exs`, configure `opentelemetry` to use this library:

```elixir
config :opentelemetry,
  id_generator: :opentelemetry_xray_id_generator,
  propagators: [:opentelemetry_xray_propagator, :baggage]
```

Add resource attributes to the span to connect it to log messages:

```shell
export OTEL_RESOURCE_ATTRIBUTES="aws.log.group.names=$AWS_LOG_GROUP"
```

See [phoenix_container_example](https://github.com/cogini/phoenix_container_example)
for a complete Elixir Phoenix app that uses this library.
