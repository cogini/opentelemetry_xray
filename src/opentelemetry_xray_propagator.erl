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

-module(opentelemetry_xray_propagator).

-behaviour(otel_propagator_text_map).

-export([fields/1, inject/4, extract/5]).
% Used by opentelemetry_xray_logger_filter.erl
-export([encode_span_id/1, encode_trace_id/1]).

-ifdef(TEST).

-export([decode/1, decode/2, parse_trace_id/2, encode/1]).

-endif.

-include_lib("kernel/include/logger.hrl").
-include_lib("opentelemetry_api/include/opentelemetry.hrl").

-define(XRAY_CONTEXT_KEY, <<"X-Amzn-Trace-Id">>).

% @doc Return list of the keys the propagator sets with `inject'.
fields(_) ->
    [?XRAY_CONTEXT_KEY].

% @doc Inject context into carrier.
-spec inject(Context, Carrier, CarrierSetFun, Options) -> Carrier
    when Context :: otel_ctx:t(),
         Carrier :: otel_propagator:carrier(),
         CarrierSetFun :: otel_propagator_text_map:carrier_set(),
         Options :: otel_propagator_text_map:propagator_options().
inject(Ctx, Carrier, CarrierSet, _Options) ->
    case otel_tracer:current_span_ctx(Ctx) of
        #span_ctx{trace_id = TraceId, span_id = SpanId} = SpanCtx
            when TraceId =/= 0, SpanId =/= 0 ->
            CarrierSet(?XRAY_CONTEXT_KEY, encode(SpanCtx), Carrier);
        _ ->
            Carrier
    end.

% @doc Extract context from carrier.
-spec extract(Context, Carrier, CarrierKeysFun, CarrierGetFun, Options) -> Context
    when Context :: otel_ctx:t(),
         Carrier :: otel_propagator:carrier(),
         CarrierKeysFun :: otel_propagator_text_map:carrier_keys(),
         CarrierGetFun :: otel_propagator_text_map:carrier_get(),
         Options :: otel_propagator_text_map:propagator_options().
extract(Ctx, Carrier, _CarrierKeysFun, CarrierGet, _Options) ->
    try
        SpanCtx = parse_context(Carrier, CarrierGet),
        otel_tracer:set_current_span(Ctx, SpanCtx)
    catch
        invalid ->
            undefined;
        %% thrown if _to_integer fails or an invalid string encoding is sent
        error:badarg ->
            undefined
    end.

-spec parse_context(Carrier, CarrierGet) -> opentelemetry:span_ctx()
    when Carrier :: otel_propagator:carrier(),
         CarrierGet :: otel_propagator_text_map:carrier_get().
parse_context(Carrier, CarrierGet) ->
    case CarrierGet(?XRAY_CONTEXT_KEY, Carrier) of
        Context when is_binary(Context) ->
            decode(Context);
        _ ->
            throw(invalid)
    end.

% @doc Decode context into span_ctx.
% Examples:
% * X-Amzn-Trace-Id: Root=1-5759e988-bd862e3fe1be46a994272793;Parent=53995c3f42cd8ad8;Sampled=0
% * X-Amzn-Trace-Id: Root=1-5759e988-bd862e3fe1be46a994272793;Sampled=1;Lineage=a87bd80c:1|68fd508a:5|c512fbe3:2
% @end
-spec decode(binary()) -> opentelemetry:span_ctx().
decode(Context) ->
    % Initialize span context, setting tracing off by default
    SpanCtx = otel_tracer:from_remote_span(0, 0, 0),
    lists:foldl(fun decode/2, SpanCtx, string:split(Context, ";", all)).

-spec decode(binary(), opentelemetry:span_ctx()) -> opentelemetry:span_ctx().
decode(<<"Root=1-", Time:8/binary, "-", UniqueId:24/binary>>, SpanCtx) ->
    SpanCtx#span_ctx{trace_id = parse_trace_id(Time, UniqueId)};
decode(<<"Root=", _rest/binary>>, _SpanCtx) ->
    throw(invalid);
decode(<<"Parent=", ParentId:16/binary>>, SpanCtx) ->
    SpanCtx#span_ctx{span_id = parse_span_id(ParentId)};
decode(<<"Parent=", _rest/binary>>, _SpanCtx) ->
    throw(invalid);
decode(<<"Sampled=0">>, SpanCtx) ->
    SpanCtx#span_ctx{trace_flags = 0};
decode(<<"Sampled=1">>, SpanCtx) ->
    SpanCtx#span_ctx{trace_flags = 1};
decode(<<"Sampled=", _rest/binary>>, _SpanCtx) ->
    throw(invalid);
decode(_Value, SpanCtx) ->
    % Ignore things that we don't understand, e.g. Lineage
    SpanCtx.

% @doc Parse trace_id to integer.
% Combines time and unique id into a single big integer, following other AWS SDKs:
% * https://github.com/open-telemetry/opentelemetry-python-contrib/blob/main/propagator/opentelemetry-propagator-aws-xray/src/opentelemetry/propagators/aws/aws_xray_propagator.py
% * https://github.com/open-telemetry/opentelemetry-go-contrib/blob/main/propagators/aws/xray/propagator.go
-spec parse_trace_id(Time, UniqueId) -> non_neg_integer()
    when Time :: binary(),
         UniqueId :: binary().
parse_trace_id(Time, UniqueId) when is_binary(UniqueId) ->
    case string:length(Time) =:= 8 andalso string:length(UniqueId) =:= 24 of
        true ->
            TraceId = iolist_to_binary([Time, UniqueId]),
            binary_to_integer(TraceId, 16);
        _ ->
            throw(invalid)
    end.

% @doc Parse span id to integer.
parse_span_id(SpanId) when is_binary(SpanId) ->
    case string:length(SpanId) =:= 16 of
        true ->
            binary_to_integer(SpanId, 16);
        _ ->
            throw(invalid)
    end.

% @doc Encode span context.
-spec encode(opentelemetry:span_ctx()) -> unicode:unicode_binary().
encode(SpanCtx) ->
    TraceId = encode_trace_id(SpanCtx),
    Parent = encode_parent(SpanCtx),
    Sampled = encode_sampled(SpanCtx),
    otel_utils:assert_to_binary(["Root=1-", TraceId, Parent, Sampled]).

% @doc Encode trace_id to string.
-spec encode_trace_id(opentelemetry:span_ctx() | non_neg_integer()) ->
                         unicode:latin1_chardata().
encode_trace_id(#span_ctx{trace_id = TraceId}) ->
    TraceIdHex =
        unicode:characters_to_binary(
            io_lib:format("~32.16.0b", [TraceId])),
    <<Time:8/binary, UniqueId:24/binary>> = TraceIdHex,
    [Time, "-", UniqueId];
encode_trace_id(TraceId) when is_integer(TraceId) ->
    TraceIdHex =
        unicode:characters_to_binary(
            io_lib:format("~32.16.0b", [TraceId])),
    <<Time:8/binary, UniqueId:24/binary>> = TraceIdHex,
    [Time, "-", UniqueId].

% @doc Encode span_id to string.
-spec encode_span_id(non_neg_integer()) -> unicode:latin1_chardata().
encode_span_id(0) ->
    "";
encode_span_id(SpanId) ->
    io_lib:format("~16.16.0b", [SpanId]).

-spec encode_parent(opentelemetry:span_ctx()) -> unicode:latin1_chardata().
encode_parent(#span_ctx{span_id = 0}) ->
    "";
encode_parent(#span_ctx{span_id = SpanId}) ->
    io_lib:format(";Parent=~16.16.0b", [SpanId]).

-spec encode_sampled(opentelemetry:span_ctx()) -> unicode:latin1_chardata().
encode_sampled(#span_ctx{trace_flags = TraceFlags}) ->
    % No sampling is the default
    case TraceFlags band 1 of
        1 ->
            <<";Sampled=1">>;
        _ ->
            <<>>
    end.
