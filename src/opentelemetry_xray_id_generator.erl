%% @doc
%%
%% Generate trace_id and span_id compatible with AWS X-Ray.
%%
%% An X-Ray trace_id consists of three values separated by hyphens.
%% For example, the trace_id {1-5759e988-bd862e3fe1be46a994272793} includes three
%% values: the version number 1, the time of the original request in Unix epoch
%% time, and finally a 96-bit identifier for the trace which is globally unique.
%% OpenTelemetry trace_ids are integers, so we combine the time and the unique
%% id into a single 128-bit value.
%%
%% X-Ray (sub)segment ID is a 64-bit identifier which is unique within a single
%% trace, and consists of 16 hexadecimal digits.
%%
%% https://docs.aws.amazon.com/xray/latest/devguide/xray-concepts.html
%% https://aws-otel.github.io/docs/getting-started/x-ray#configuring-the-aws-x-ray-exporter
%% @end

-module(opentelemetry_xray_id_generator).

% Don't implement behavior to avoid dependency on opentelemetry
% -behaviour(otel_id_generator).
-export([generate_trace_id/0, generate_span_id/0]).

-ifdef(TEST).

-export([merge_trace_id/2]).

-endif.

-include_lib("opentelemetry_api/include/gradualizer.hrl").

% @doc Generates a 128 bit random integer to use as a trace id.
-spec generate_trace_id() -> opentelemetry:trace_id().
generate_trace_id() ->
  Timestamp = opentelemetry:convert_timestamp(opentelemetry:timestamp(), second),
  % 2 shifted left by 95 == 2 ^ 96
  UniqueId = rand:uniform(?assert_type(2 bsl 95 - 1, pos_integer())),
  merge_trace_id(Timestamp, UniqueId).


% @doc Generates a 64 bit random integer to use as a span id.
-spec generate_span_id() -> opentelemetry:span_id().
generate_span_id() ->
  % 2 shifted left by 63 == 2 ^ 64
  rand:uniform(?assert_type(2 bsl 63 - 1, pos_integer())).


-spec merge_trace_id(non_neg_integer(), non_neg_integer()) -> opentelemetry:trace_id().
merge_trace_id(Timestamp, UniqueId) -> (Timestamp bsl 96) bor UniqueId.
