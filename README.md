![test workflow](https://github.com/reachfh/logger_formatter_json/actions/workflows/test.yml/badge.svg)
[![Contributor Covenant](https://img.shields.io/badge/Contributor%20Covenant-2.1-4baaaa.svg)](CODE_OF_CONDUCT.md)

# opentelemetry_propagator_xray

An OpenTelemetry propagator that reads and writes AWS X-Ray trace context headers.

It implements the `otel_propagator_text_map` protocol in the Erlang SDK.

Links:

* Propagator: https://opentelemetry.io/docs/specs/otel/context/api-propagators/
* https://docs.aws.amazon.com/xray/latest/devguide/xray-concepts.html#xray-concepts-tracingheader

Since `trace_context` and `baggage` are the two default propagators the
global TextMap Propagators must be configured.

## Configuration

```erlang
{text_map_propagators, [xray, baggage]},
```
