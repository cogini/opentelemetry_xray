-module(opentelemetry_xray_resource_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("opentelemetry_api/include/opentelemetry.hrl").

all() -> [parse].

parse() -> [{docs, "parse env value"}].

parse(_) ->
  ?assertEqual([], opentelemetry_xray_resource:parse(false)),
  ?assertEqual([{<<"aws.log.group.names">>, <<"foo">>}], opentelemetry_xray_resource:parse("foo")),
  ok.
