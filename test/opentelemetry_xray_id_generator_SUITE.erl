-module(opentelemetry_xray_id_generator_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("opentelemetry_api/include/opentelemetry.hrl").

all() -> [merge].

merge() -> [{docs, "merge integers"}].

% io:format("~128.02b~n", [(1698024497 bsl 96) bor 16]).
merge(_) ->
  Timestamp = 1698024497,
  UniqueId = 51635583967283991042469687690,
  ?assertEqual(
    ((1698024497 bsl 96) bor 51635583967283991042469687690),
    opentelemetry_xray_id_generator:merge_trace_id(Timestamp, UniqueId)
  ),
  ok.
