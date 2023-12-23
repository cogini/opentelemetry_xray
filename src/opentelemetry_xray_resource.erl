%% @doc
%%
%% OpenTelemetry resource for AWS.
%%
%% This module implements the otel_resource_detector behaviour. It reads the
%% environment variable `AWS_LOG_GROUP` and creates a resource in the span
%% which correlates traces and log messages.
%%
%% See:
%% * https://github.com/aws/aws-xray-sdk-python/issues/188
%% * https://opentelemetry.io/docs/instrumentation/erlang/resources/
%% @end

-module(opentelemetry_xray_resource).

% Don't set behavior to avoid dependency on opentelemetry
% -behaviour(otel_resource_detector).
-export([get_resource/1, parse/1]).

% From opentelemetry/src/otel_resource.erl
-type key() :: unicode:latin1_binary() | atom().
-type value() :: unicode:latin1_binary() | integer() | float() | boolean().

get_resource(_Config) -> otel_resource:create(parse(os:getenv("AWS_LOG_GROUP"))).

-spec parse(false | string()) -> list({key(), value()}).
parse(false) -> [];
parse(LogGroup) -> [{<<"aws.log.group.names">>, list_to_binary(LogGroup)}].
