#!/bin/bash

GREEN='\033[0;32m'
RED='\033[0;31m'
NC='\033[0m' # No Color

TEST_DIR="$(dirname "$0")"

for goku_file in "$TEST_DIR"/*.goku; do
    echo "Running test: $(basename "$goku_file")"
    output=$(stack run goku-exe "$goku_file" 2>&1)
    exit_code=$?

    if [[ "$(basename "$goku_file")" == "assert_false.goku" ]]; then
        if [[ $exit_code -ne 0 && "$output" == *"Assertion failed!"* ]]; then
            echo -e "  ${GREEN}PASS${NC}: Expected failure for $(basename "$goku_file")"
        else
            echo -e "  ${RED}FAIL${NC}: Expected failure for $(basename "$goku_file"), but it passed or failed unexpectedly."
            echo "Output: $output"
        fi
    else
        if [[ $exit_code -eq 0 && "$output" == *"Program executed successfully."* ]]; then
            echo -e "  ${GREEN}PASS${NC}: Expected success for $(basename "$goku_file")"
        else
            echo -e "  ${RED}FAIL${NC}: Expected success for $(basename "$goku_file"), but it failed or passed unexpectedly."
            echo "Output: $output"
        fi
    fi
done
