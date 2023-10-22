![test workflow](https://github.com/reachfh/logger_formatter_json/actions/workflows/test.yml/badge.svg)
[![Contributor Covenant](https://img.shields.io/badge/Contributor%20Covenant-2.1-4baaaa.svg)](CODE_OF_CONDUCT.md)

# opentelemetry_propagator_xray

An OpenTelemetry propagator that reads and writes AWS X-Ray trace context headers.

It implements the `otel_propagator_text_map` protocol in the Erlang SDK.

Links:

* Propagator: https://opentelemetry.io/docs/specs/otel/context/api-propagators/
* https://docs.aws.amazon.com/xray/latest/devguide/xray-concepts.html#xray-concepts-tracingheader
* https://aws-otel.github.io/docs/getting-started/x-ray#configuring-the-aws-x-ray-exporter

Since `trace_context` and `baggage` are the two default propagators the
global TextMap Propagators must be configured.

## Configuration

Since `trace_context` and `baggage` are the two default propagators the global
TextMap Propagators must be configured if B3 is to be used for propagation:

```erlang
{text_map_propagators, [xray, baggage]},
```

```erlang
CompositePropagator = otel_propagator_text_map_composite:create([xray, baggage]),
opentelemetry:set_text_map_propagator(CompositePropagator).
```

It is also possible to set a separate list of injectors or extractors. For
example, if the service should extract X-Ray encoded context but you only want
to inject context encoded with the W3C TraceContext format (maybe you have some
services only supporting B3 that are making requests to your server but you
have no reason to continue propagating in both formats when communicating to
other services further down the stack). In that case you would instead set
configuration like:

```erlang
{text_map_extractors, [xray, trace_context, baggage]},
{text_map_injectors, [trace_context, baggage]},
```

Or using calls to `opentelemetry` at runtime:

```erlang
XrayCompositePropagator = otel_propagator_text_map_composite:create([xray, trace_context, baggage]),
CompositePropagator = otel_propagator_text_map_composite:create([trace_context, baggage]),
opentelemetry:set_text_map_extractor(XrayCompositePropagator),
opentelemetry:set_text_map_injector(CompositePropagator).
```
