%% @doc
%%
%% Generate trace_id and span_id compatible with AWS X-Ray.
%%
%% The X-Ray trace_id is a 64-bit identifier which is unique within a single
%% trace. The first 32 bits is the timestamp, the remaining 96 bits are random.
%% OpenTelemetry trace_ids are integers, so we combine the time and the unique
%% id into a single 128-bit value.
%%
%% It is represented in text as three values separated by hyphens.
%% For example, the trace_id {1-5759e988-bd862e3fe1be46a994272793} includes three
%% values: version number 1, the time of the original request in Unix epoch
%% time in hex, and finally a 96-bit identifier for the trace which is globally unique.
%%
%% https://docs.aws.amazon.com/xray/latest/devguide/xray-api-sendingdata.html#xray-api-traceids
%% https://docs.aws.amazon.com/xray/latest/devguide/xray-concepts.html
%% https://aws-otel.github.io/docs/getting-started/x-ray#configuring-the-aws-x-ray-exporter
%% @end

-module(opentelemetry_xray_id_generator).

% Don't specify behavior to avoid dependency on opentelemetry
% -behaviour(otel_id_generator).
-export([generate_trace_id/0, generate_span_id/0]).

-ifdef(TEST).

-export([merge_trace_id/2]).

-endif.

-include_lib("opentelemetry_api/include/gradualizer.hrl").

% @doc Generate 128-bit random integer to use as trace id.
-spec generate_trace_id() -> opentelemetry:trace_id().
generate_trace_id() ->
  Timestamp = opentelemetry:convert_timestamp(opentelemetry:timestamp(), second),
  % 2 shifted left by 95 == 2 ^ 96
  UniqueId = rand:uniform(?assert_type(2 bsl 95 - 1, pos_integer())),
  merge_trace_id(Timestamp, UniqueId).


% @doc Generate 64-bit random integer to use as a span id.
-spec generate_span_id() -> opentelemetry:span_id().
generate_span_id() ->
  % 2 shifted left by 63 == 2 ^ 64
  rand:uniform(?assert_type(2 bsl 63 - 1, pos_integer())).


-spec merge_trace_id(non_neg_integer(), non_neg_integer()) -> opentelemetry:trace_id().
merge_trace_id(Timestamp, UniqueId) -> (Timestamp bsl 96) bor UniqueId.
