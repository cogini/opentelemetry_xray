-module(otel_propagator_xray_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("opentelemetry_api/include/opentelemetry.hrl").

% all() -> [decode, encode, fields].
all() -> [fields, decode].

decode() -> [{docs, "decode header"}].

decode(_) ->
    SpanCtx = otel_tracer:from_remote_span(0, 0, 0),
    ?assertEqual(
       58654881323332183544033650579,
       otel_propagator_xray:parse_trace_id(<<"bd862e3fe1be46a994272793">>)
      ),
    ?assertEqual(
       SpanCtx#span_ctx{trace_id=58654881323332183544033650579,
                        tracestate=[{<<"xray">>,<<"1-5759e988-bd862e3fe1be46a994272793">>}]},
       otel_propagator_xray:decode(<<"Root=1-5759e988-bd862e3fe1be46a994272793">>, SpanCtx)
      ),
    ?assertEqual(
       SpanCtx#span_ctx{span_id=6023947403358210776},
       otel_propagator_xray:decode(<<"Parent=53995c3f42cd8ad8">>, SpanCtx)
      ),
    ?assertEqual(
       SpanCtx#span_ctx{trace_flags=0},
       otel_propagator_xray:decode(<<"Sampled=0">>, SpanCtx)
      ),
    ?assertEqual(
       SpanCtx#span_ctx{trace_flags=1},
       otel_propagator_xray:decode(<<"Sampled=1">>, SpanCtx)
      ),
    ?assertEqual(
       SpanCtx,
       otel_propagator_xray:decode(<<"Lineage=a87bd80c:1|68fd508a:5|c512fbe3:2">>, SpanCtx)
      ),
    ok.

fields(_) ->
    ?assertEqual([<<"X-Amzn-Trace-Id">>], otel_propagator_xray:fields(foo)),
    ok.
