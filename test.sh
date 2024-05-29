#!/bin/bash

# test cases for bashunit

STACK_RUN="stack run --"
function run() {
    $STACK_RUN -q "$1" test.toml
}

function set_up_before_script() {
    # Compile ans install bin (if not already)
    $STACK_RUN -h > /dev/null 2>&1
}

function test_table() {
    OUT=$(run "owner.name")
    assert_successful_code
    assert_equals "$OUT" "Tom Preston-Werner"
}

function test_date() {
    OUT=$(run "owner.dob")
    assert_successful_code
    assert_equals "$OUT" "1979-05-27 07:32:00 -0800"
}

function test_float() {
    OUT=$(run "database.temp_targets.cpu")
    assert_successful_code
    assert_equals "$OUT" 79.5
}

function test_array() {
    OUT=$(run "database.ports.[1]")
    assert_successful_code
    assert_equals "$OUT" "8001"
}

function test_array_of_arrays() {
    OUT=$(run "database.ports.[1]")
    assert_successful_code
    assert_equals "$OUT" "8001"
}

function test_error_file_not_found() {
    OUT=$($STACK_RUN -q "x" xx.toml 2>&1)
    assert_general_error
    assert_contains "does not exist" "$OUT"
}

function test_error_invalid_query() {
    OUT=$(run "[0].owner" 2>&1)
    assert_general_error
    assert_equals "$OUT" "Invalid query"
}

function test_error_past_leaf_node() {
    OUT=$(run "owner.dob.xyz" 2>&1)
    assert_general_error
    assert_equals "$OUT" "Trying to query past leaf node"
}

function test_error_non_leaf_node() {
    OUT=$(run "owner" 2>&1)
    assert_general_error
    assert_equals "$OUT" "Querying a non-leaf node"
}

function test_error_index_on_table() {
    OUT=$(run "servers.alpha.[0]" 2>&1)
    assert_general_error
    assert_equals "$OUT" "Trying to index a table"
}

function test_error_key_on_table() {
    OUT=$(run "database.ports.0" 2>&1)
    assert_general_error
    assert_equals "$OUT" "Trying to key into an array"
}

function test_error_key_error() {
    OUT=$(run "xxx" 2>&1)
    assert_general_error
    assert_equals "$OUT" "Key does not exist: 'xxx'"
}

function test_error_index_out_of_range() {
    OUT=$(run "database.ports.[9999]" 2>&1)
    assert_general_error
    assert_equals "$OUT" "Index out of range: 9999"
}
