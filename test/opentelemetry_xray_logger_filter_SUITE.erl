-module(opentelemetry_xray_logger_filter_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/logger.hrl").
-include_lib("stdlib/include/assert.hrl").

all() -> [encode].

encode() -> [{docs, "encode"}].

encode(_) ->
  TraceId = binary_to_integer(<<"5759e988bd862e3fe1be46a994272793">>, 16),
  SpanId = binary_to_integer(<<"53995c3f42cd8ad8">>, 16),
  ?assertMatch(
    #{meta := #{trace_id := <<"1-5759e988-bd862e3fe1be46a994272793">>}},
    opentelemetry_xray_logger_filter:trace_id(#{meta => #{trace_id => TraceId}}, undefined)
  ),
  ?assertMatch(
    #{meta := #{trace_id := <<"1-5759e988-bd862e3fe1be46a994272793@53995c3f42cd8ad8">>}},
    opentelemetry_xray_logger_filter:trace_id(
      #{meta => #{trace_id => TraceId, span_id => SpanId}},
      undefined
    )
  ),
  ?assertMatch(#{meta := #{}}, opentelemetry_xray_logger_filter:trace_id(#{meta => #{}}, undefined)),
  ok.
