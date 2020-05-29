Records of the function\_results migration to function\_results\_v2

Plan:
https://trello.com/c/nl1szxK8/444-migrate-storedfunctionresults-table

SQL for copying the data

```
prodclone=# insert into function_results_v2 (canvas_id, tlid, fnname, id, hash, timestamp, value, trace_id) select canvas_id, tlid, fnname, id, hash, timestamp, value, trace_id from function_results where trace_id is not null and fnname in ('DB::insert', 'DB::delete', 'DB::deleteAll', 'DB::update', 'DB::fetchBy', 'DB::fetchOneBy', 'DB::fetchByMany', 'DB::fetchOneByMany', 'DB::fetchAll', 'DB::keys', 'DB::schema', 'DB::set_v1', 'DB::get_v1', 'DB::getMany_v1', 'DB::delete_v1', 'DB::deleteAll_v1', 'DB::query_v1', 'DB::query_v2', 'DB::queryWithKey_v1', 'DB::queryOne_v1', 'DB::queryOneWithKey_v1', 'DB::getAll_v1', 'DB::schemaFields_v1', 'DB::schema_v1', 'emit', 'HttpClient::post', 'HttpClient::put', 'HttpClient::get', 'HttpClient::delete', 'HttpClient::options', 'HttpClient::head', 'HttpClient::patch', 'HttpClient::basicAuth', 'Int::random', 'String::random', 'String::htmlEscape', 'Date::now', 'Uuid::generate', 'Twilio::sendText');
INSERT 0 35580
Time: 50957.414 ms
```
