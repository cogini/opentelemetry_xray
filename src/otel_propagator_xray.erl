%% @doc
%%
%% Propagator that injects and extracts context from the AWS X-Ray tracing header.
%%
%% When the current app is downstream from another app or the AWS load balancer,
%% the upstream app creates the trace for the current request and sends it in
%% the "X-Amzn-Trace-Id" HTTP header. The header includes the trace id
%% and optional information about the parent span and sampling.
%%
%% Similarly, when the current app makes calls to downstream services, it sets
%% the header to pass the current trace context.
%%
%% NOTE: Amazon assumes that spans are not sampled by default.
%% If you want your traces to be sampled, make sure that you turn it on.
%%
%% https://docs.aws.amazon.com/xray/latest/devguide/xray-concepts.html#xray-concepts-tracingheader
%% @end

-module(otel_propagator_xray).

-behaviour(otel_propagator_text_map).

-export([fields/1, inject/4, extract/5]).

-ifdef(TEST).

-export([decode/2, parse_trace_id/1]).

-endif.

-include_lib("kernel/include/logger.hrl").
-include_lib("opentelemetry_api/include/opentelemetry.hrl").

-define(XRAY_CONTEXT_KEY, <<"X-Amzn-Trace-Id">>).
-define(XRAY_TRACESTATE_KEY, <<"xray">>).
-define(STATE_HEADER_KEY, <<"tracestate">>).

% @doc Return list of the keys the propagator sets with `inject'.
fields(_) -> [?XRAY_CONTEXT_KEY].

% @doc Inject context into carrier.
-spec inject(Context, Carrier, CarrierSetFun, Options) ->
  Carrier
  when Context :: otel_ctx:t(),
       Carrier :: otel_propagator:carrier(),
       CarrierSetFun :: otel_propagator_text_map:carrier_set(),
       Options :: otel_propagator_text_map:propagator_options().
inject(Ctx, Carrier, CarrierSet, _Options) ->
  case otel_tracer:current_span_ctx(Ctx) of
    #span_ctx{trace_id = TraceId, span_id = SpanId} = SpanCtx when TraceId =/= 0, SpanId =/= 0 ->
      CarrierSet(?XRAY_CONTEXT_KEY, encode(SpanCtx), Carrier);

    _ -> Carrier
  end.

% @doc Extract context from carrier.
-spec extract(Context, Carrier, CarrierKeysFun, CarrierGetFun, Options) ->
  Context
  when Context :: otel_ctx:t(),
       Carrier :: otel_propagator:carrier(),
       CarrierKeysFun :: otel_propagator_text_map:carrier_keys(),
       CarrierGetFun :: otel_propagator_text_map:carrier_get(),
       Options :: otel_propagator_text_map:propagator_options().
extract(Ctx, Carrier, _CarrierKeysFun, CarrierGet, _Options) ->
  try
    SpanCtx = parse_xray_context(Carrier, CarrierGet),
    otel_tracer:set_current_span(Ctx, SpanCtx)
  catch
    throw : invalid -> undefined;
    %% thrown if _to_integer fails or an invalid string encoding is sent
    error : badarg -> undefined
  end.


-spec parse_xray_context(Carrier, CarrierGet) ->
  opentelemetry:span_ctx()
  when Carrier :: otel_propagator:carrier(), CarrierGet :: otel_propagator_text_map:carrier_get().
parse_xray_context(Carrier, CarrierGet) ->
  case CarrierGet(?XRAY_CONTEXT_KEY, Carrier) of
    Context when is_binary(Context) ->
      SpanCtx = otel_tracer:from_remote_span(0, 0, 0),
      lists:foldl(fun decode/2, SpanCtx, string:split(Context, ";", all));

    _ -> throw(invalid)
  end.

% @doc Decode context into span_ctx.
% Examples:
% * X-Amzn-Trace-Id: Root=1-5759e988-bd862e3fe1be46a994272793;Parent=53995c3f42cd8ad8;Sampled=0
% * X-Amzn-Trace-Id: Root=1-5759e988-bd862e3fe1be46a994272793;Sampled=1;Lineage=a87bd80c:1|68fd508a:5|c512fbe3:2
% @end

-spec decode(binary(), opentelemetry:span_ctx()) -> opentelemetry:span_ctx().
decode(<<"Root=1-", Time:8/binary, "-", Id:24/binary>>, SpanCtx0) ->
  % Save original trace ID in tracestate
  SpanCtx = set_tracestate(SpanCtx0, <<"1-", Time/binary, "-", Id/binary>>),
  % Parse Id and use it as trace_id
  SpanCtx#span_ctx{trace_id = parse_trace_id(Id)};

decode(<<"Parent=", ParentId:16/binary>>, SpanCtx) ->
  SpanCtx#span_ctx{span_id = parse_span_id(ParentId)};

decode(<<"Sampled=0">>, SpanCtx) -> SpanCtx#span_ctx{trace_flags = 0};
decode(<<"Sampled=1">>, SpanCtx) -> SpanCtx#span_ctx{trace_flags = 1};
decode(<<>>, SpanCtx) -> SpanCtx;

decode(Value, SpanCtx) ->
  ?LOG_DEBUG("Ignoring value ~w", [Value]),
  SpanCtx.


-spec set_tracestate(opentelemetry:span_ctx(), binary()) -> opentelemetry:span_ctx().
set_tracestate(#span_ctx{tracestate = []} = SpanCtx, Value) ->
  SpanCtx#span_ctx{tracestate = [{<<"xray">>, Value}]};

set_tracestate(#span_ctx{tracestate = Tracestate} = SpanCtx, Value) ->
  % Add new trace id to front of tracestate, removing any existing value
  SpanCtx#span_ctx{tracestate = [{<<"xray">>, Value} | lists:keydelete(<<"xray">>, 1, Tracestate)]}.

%% Trace ID is a 24-byte hex binary

parse_trace_id(TraceId) when is_binary(TraceId) ->
  case string:length(TraceId) =:= 24 of
    true -> string_to_integer(TraceId, 16);
    _ -> throw(invalid)
  end.


% Span ID is a 16-byte hex binary
parse_span_id(SpanId) when is_binary(SpanId) ->
  case string:length(SpanId) =:= 16 of
    true -> string_to_integer(SpanId, 16);
    _ -> throw(invalid)
  end.

%% @doc Encode span context to value of X-Amzn-Trace-Id HTTP header.
%% X-Amzn-Trace-Id: Root=1-5759e988-bd862e3fe1be46a994272793;Parent=53995c3f42cd8ad8;Sampled=1

-spec encode(opentelemetry:span_ctx()) -> unicode:unicode_binary().
encode(SpanCtx) ->
  TraceId = encode_trace_id(SpanCtx),
  Parent = encode_parent(SpanCtx),
  Sampled = encode_sampled(SpanCtx),
  otel_utils:assert_to_binary(["Root=1-", TraceId, Parent, Sampled]).


-spec encode_trace_id(opentelemetry:span_ctx()) -> unicode:latin1_chardata().
encode_trace_id(#span_ctx{trace_id = TraceId, tracestate = []}) -> generate_trace_id(TraceId);

encode_trace_id(#span_ctx{trace_id = TraceId, tracestate = Tracestate}) ->
  case lists:keyfind(<<"xray">>, 1, Tracestate) of
    false -> generate_trace_id(TraceId);
    {_, Value} -> Value
  end.


-spec encode_parent(opentelemetry:span_ctx()) -> unicode:latin1_chardata().
encode_parent(#span_ctx{span_id = 0}) -> "";
encode_parent(#span_ctx{span_id = SpanId}) -> io_lib:format(";Parent=~16.16.0b", [SpanId]).

-spec encode_sampled(opentelemetry:span_ctx()) -> unicode:latin1_chardata().
encode_sampled(#span_ctx{trace_flags = TraceFlags}) ->
  % Sampling is the default
  case TraceFlags band 1 of
    0 -> <<";Sampled=0">>;
    _ -> <<>>
  end.


-spec generate_trace_id(opentelemetry:trace_id()) -> unicode:latin1_chardata().
generate_trace_id(TraceId) ->
  Timestamp = opentelemetry:convert_timestamp(opentelemetry:timestamp(), second),
  ["1-", io_lib:format("~8.16.0b", [Timestamp]), io_lib:format("~24.16.0b", [TraceId])].


string_to_integer(S, Base) when is_binary(S) -> binary_to_integer(S, Base).

%% @doc Encode span context tracestate.
%% https://www.w3.org/TR/trace-context/#tracestate-header

% -spec encode_tracestate(opentelemetry:span_ctx()) -> [{unicode:latin1_binary(), unicode:latin1_binary()}].
% encode_tracestate(#span_ctx{tracestate=undefined}) ->
%     [];
% encode_tracestate(#span_ctx{tracestate=Entries}) ->
%     HeaderValue = lists:join($,, [[Key, $=, Value] || {Key, Value} <- Entries]),
%     [{<<"tracestate">>, otel_utils:assert_to_binary(HeaderValue)}].
