%% @doc
%%
%% Erlang logger filter to format trace id in X-Ray format.
%%
%% This module implements a logger filter which formats the trace_id in X-Ray
%% format so log messages can be correlated with traces.
%%
%% See:
%% * https://www.erlang.org/doc/man/logger#type-filter
%% * https://github.com/aws/aws-xray-sdk-python/issues/188
%% @end

-module(opentelemetry_xray_logger_filter).

-include_lib("kernel/include/logger.hrl").

-export([trace_id/2]).

-spec trace_id(LogEvent, Extra) ->
  logger:filter_return() when LogEvent :: logger:log_event(), Extra :: term().
trace_id(#{meta := #{otel_trace_id := TraceId, otel_span_id := SpanId} = Meta} = LogEvent, _Extra)
when is_integer(TraceId), is_integer(SpanId) ->
  EncodedTraceId = opentelemetry_xray_propagator:encode_trace_id(TraceId),
  EncodedSpanId = opentelemetry_xray_propagator:encode_span_id(SpanId),
  NewId = otel_utils:assert_to_binary(["1-", EncodedTraceId, "@", EncodedSpanId]),
  NewMeta = maps:put(xray_trace_id, NewId, Meta),
  maps:update(meta, NewMeta, LogEvent);

trace_id(#{meta := #{otel_trace_id := TraceId} = Meta} = LogEvent, _Extra) when is_integer(TraceId) ->
  EncodedTraceId = opentelemetry_xray_propagator:encode_trace_id(TraceId),
  NewId = otel_utils:assert_to_binary(["1-", EncodedTraceId]),
  NewMeta = maps:put(xray_trace_id, NewId, Meta),
  maps:update(meta, NewMeta, LogEvent);

trace_id(
  #{
    meta
    :=
    #{otel_trace_id := <<Time:8/binary, TraceId/binary>> = HexTraceId, otel_span_id := HexSpanId} =
      Meta
  } = LogEvent,
  _Extra
)
when is_binary(HexTraceId), is_binary(HexSpanId) ->
  NewId = otel_utils:assert_to_binary(["1-", Time, "-", TraceId, "@", HexSpanId]),
  NewMeta = maps:put(xray_trace_id, NewId, Meta),
  maps:update(meta, NewMeta, LogEvent);

trace_id(
  #{meta := #{otel_trace_id := <<Time:8/binary, TraceId/binary>> = HexTraceId} = Meta} = LogEvent,
  _Extra
)
when is_binary(HexTraceId) ->
  NewId = otel_utils:assert_to_binary(["1-", Time, "-", TraceId]),
  NewMeta = maps:put(xray_trace_id, NewId, Meta),
  maps:update(meta, NewMeta, LogEvent);

trace_id(LogEvent, _Extra) -> LogEvent.
