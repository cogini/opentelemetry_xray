-module(otel_propagator_xray_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("opentelemetry_api/include/opentelemetry.hrl").

% all() -> [decode, encode, fields].
all() -> [fields, parse, decode, encode].

parse() -> [{docs, "low level parsing"}].

parse(_) ->
  ?assertEqual(
    otel_propagator_xray:string_to_integer(<<"5759e988bd862e3fe1be46a994272793">>, 16),
    otel_propagator_xray:parse_trace_id(<<"5759e988">>, <<"bd862e3fe1be46a994272793">>)
  ),
  ok.


decode() -> [{docs, "decode header"}].

decode(_) ->
  SpanCtx = otel_tracer:from_remote_span(0, 0, 0),
  TraceId = otel_propagator_xray:string_to_integer(<<"5759e988bd862e3fe1be46a994272793">>, 16),
  SpanId = otel_propagator_xray:string_to_integer(<<"53995c3f42cd8ad8">>, 16),
  ?assertEqual(
    SpanCtx#span_ctx{trace_id = TraceId},
    otel_propagator_xray:decode(<<"Root=1-5759e988-bd862e3fe1be46a994272793">>, SpanCtx)
  ),
  ?assertEqual(
    SpanCtx#span_ctx{span_id = SpanId},
    otel_propagator_xray:decode(<<"Parent=53995c3f42cd8ad8">>, SpanCtx)
  ),
  ?assertEqual(
    SpanCtx#span_ctx{trace_flags = 0},
    otel_propagator_xray:decode(<<"Sampled=0">>, SpanCtx)
  ),
  ?assertEqual(
    SpanCtx#span_ctx{trace_flags = 1},
    otel_propagator_xray:decode(<<"Sampled=1">>, SpanCtx)
  ),
  ?assertEqual(
    SpanCtx,
    otel_propagator_xray:decode(<<"Lineage=a87bd80c:1|68fd508a:5|c512fbe3:2">>, SpanCtx)
  ),
  ?assertEqual(
    SpanCtx#span_ctx{trace_id = TraceId, span_id = SpanId},
    otel_propagator_xray:decode(
      <<"Root=1-5759e988-bd862e3fe1be46a994272793;Parent=53995c3f42cd8ad8;Sampled=0">>
    )
  ),
  ?assertEqual(
    SpanCtx#span_ctx{trace_id = TraceId, trace_flags = 1},
    otel_propagator_xray:decode(
      <<
        "Root=1-5759e988-bd862e3fe1be46a994272793;Sampled=1;Lineage=a87bd80c:1|68fd508a:5|c512fbe3:2"
      >>
    )
  ),
  ok.


encode() -> [{docs, "encode header"}].

encode(_) ->
  TraceId = otel_propagator_xray:string_to_integer(<<"5759e988bd862e3fe1be46a994272793">>, 16),
  SpanId = otel_propagator_xray:string_to_integer(<<"53995c3f42cd8ad8">>, 16),
  SpanCtx = otel_tracer:from_remote_span(0, 0, 0),
  ?assertEqual(
    <<"Root=1-5759e988-bd862e3fe1be46a994272793;Sampled=1">>,
    otel_propagator_xray:encode(SpanCtx#span_ctx{trace_id = TraceId, trace_flags = 1})
  ),
  ?assertEqual(
    <<"Root=1-5759e988-bd862e3fe1be46a994272793">>,
    otel_propagator_xray:encode(SpanCtx#span_ctx{trace_id = TraceId, trace_flags = 0})
  ),
  ok.


fields(_) ->
  ?assertEqual([<<"X-Amzn-Trace-Id">>], otel_propagator_xray:fields(foo)),
  ok.
