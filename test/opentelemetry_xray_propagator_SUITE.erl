-module(opentelemetry_xray_propagator_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("opentelemetry_api/include/opentelemetry.hrl").

% all() -> [decode, encode, fields].
all() -> [fields, parse, decode, decode_invalid, encode, encode_id].

parse() -> [{docs, "low level parsing"}].

parse(_) ->
  ?assertEqual(
    binary_to_integer(<<"5759e988bd862e3fe1be46a994272793">>, 16),
    opentelemetry_xray_propagator:parse_trace_id(<<"5759e988">>, <<"bd862e3fe1be46a994272793">>)
  ),
  ok.


decode() -> [{docs, "decode header"}].

decode(_) ->
  SpanCtx = otel_tracer:from_remote_span(0, 0, 0),
  TraceId = binary_to_integer(<<"5759e988bd862e3fe1be46a994272793">>, 16),
  SpanId = binary_to_integer(<<"53995c3f42cd8ad8">>, 16),
  ?assertEqual(
    SpanCtx#span_ctx{trace_id = TraceId},
    opentelemetry_xray_propagator:decode(<<"Root=1-5759e988-bd862e3fe1be46a994272793">>, SpanCtx)
  ),
  ?assertEqual(
    SpanCtx#span_ctx{span_id = SpanId},
    opentelemetry_xray_propagator:decode(<<"Parent=53995c3f42cd8ad8">>, SpanCtx)
  ),
  ?assertEqual(
    SpanCtx#span_ctx{trace_flags = 0},
    opentelemetry_xray_propagator:decode(<<"Sampled=0">>, SpanCtx)
  ),
  ?assertEqual(
    SpanCtx#span_ctx{trace_flags = 1},
    opentelemetry_xray_propagator:decode(<<"Sampled=1">>, SpanCtx)
  ),
  ?assertEqual(
    SpanCtx,
    opentelemetry_xray_propagator:decode(<<"Lineage=a87bd80c:1|68fd508a:5|c512fbe3:2">>, SpanCtx)
  ),
  ?assertEqual(
    SpanCtx#span_ctx{trace_id = TraceId, span_id = SpanId},
    opentelemetry_xray_propagator:decode(
      <<"Root=1-5759e988-bd862e3fe1be46a994272793;Parent=53995c3f42cd8ad8;Sampled=0">>
    )
  ),
  ?assertEqual(
    SpanCtx#span_ctx{trace_id = TraceId, trace_flags = 1},
    opentelemetry_xray_propagator:decode(
      <<
        "Root=1-5759e988-bd862e3fe1be46a994272793;Sampled=1;Lineage=a87bd80c:1|68fd508a:5|c512fbe3:2"
      >>
    )
  ),
  ok.


decode_invalid() -> [{docs, "decode invalid"}].

decode_invalid(_) ->
  % invalid trace id
  ?assertError(
    badarg,
    opentelemetry_xray_propagator:decode(
      <<"Root=1-12345678-abcdefghijklmnopqrstuvwx;Parent=53995c3f42cd8ad8;Sampled=0">>
    )
  ),
  % invalid size trace id
  ?assertThrow(
    invalid,
    opentelemetry_xray_propagator:decode(
      <<"Root=1-8a3c60f7-d188f8fa79d48a391a778fa600;Parent=53995c3f42cd8ad8;Sampled=0">>
    )
  ),
  % invalid span id
  ?assertError(
    badarg,
    opentelemetry_xray_propagator:decode(
      <<"Root=1-8a3c60f7-d188f8fa79d48a391a778fa6;Parent=abcdefghijklmnop;Sampled=0">>
    )
  ),
  % invalid size span id
  ?assertThrow(
    invalid,
    opentelemetry_xray_propagator:decode(
      <<"Root=1-8a3c60f7-d188f8fa79d48a391a778fa6;Parent=53995c3f42cd8ad800;Sampled=0">>
    )
  ),
  % no sampled flag
  ?assertThrow(
    invalid,
    opentelemetry_xray_propagator:decode(
      <<"Root=1-8a3c60f7-d188f8fa79d48a391a778fa6;Parent=53995c3f42cd8ad8;Sampled=">>
    )
  ),
  % invalid size sampled
  ?assertThrow(
    invalid,
    opentelemetry_xray_propagator:decode(
      <<"Root=1-8a3c60f7-d188f8fa79d48a391a778fa6;Parent=53995c3f42cd8ad8;Sampled=011">>
    )
  ),
  % non numeric sampled flag
  ?assertThrow(
    invalid,
    opentelemetry_xray_propagator:decode(
      <<"Root=1-8a3c60f7-d188f8fa79d48a391a778fa6;Parent=53995c3f42cd8ad8;Sampled=a">>
    )
  ),
  ok.


encode() -> [{docs, "encode header"}].

encode(_) ->
  TraceId = binary_to_integer(<<"5759e988bd862e3fe1be46a994272793">>, 16),
  SpanCtx = otel_tracer:from_remote_span(0, 0, 0),
  ?assertEqual(
    <<"Root=1-5759e988-bd862e3fe1be46a994272793;Sampled=1">>,
    opentelemetry_xray_propagator:encode(SpanCtx#span_ctx{trace_id = TraceId, trace_flags = 1})
  ),
  ?assertEqual(
    <<"Root=1-5759e988-bd862e3fe1be46a994272793">>,
    opentelemetry_xray_propagator:encode(SpanCtx#span_ctx{trace_id = TraceId, trace_flags = 0})
  ),
  ok.


encode_id(_) ->
  TraceId = binary_to_integer(<<"5759e988bd862e3fe1be46a994272793">>, 16),
  SpanId = binary_to_integer(<<"53995c3f42cd8ad8">>, 16),
  ?assertEqual(
    [<<"5759e988">>, "-", <<"bd862e3fe1be46a994272793">>],
    opentelemetry_xray_propagator:encode_trace_id(TraceId)
  ),
  ?assertEqual("53995c3f42cd8ad8", opentelemetry_xray_propagator:encode_span_id(SpanId)),
  ok.


fields(_) ->
  ?assertEqual([<<"X-Amzn-Trace-Id">>], opentelemetry_xray_propagator:fields(foo)),
  ok.
