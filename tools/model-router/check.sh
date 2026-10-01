#!/usr/bin/env bash
set -euo pipefail
candidate_dir=$(cd -- "$(dirname -- "$0")" && pwd)
# Caller example:
# ./check.sh /path/to/project/stack.yaml
# Alternatively set GRACE_NATIVE_STACK_YAML to that explicit configuration.
stack_yaml=${1:-${GRACE_NATIVE_STACK_YAML:-}}
if [[ $# -gt 1 || -z "$stack_yaml" || ! -f "$stack_yaml" ]]; then
  printf 'Usage: %s STACK.yaml (or set GRACE_NATIVE_STACK_YAML)\n' "$0" >&2
  exit 2
fi
mkdir -p "$candidate_dir/build/protocol" "$candidate_dir/build/runner" "$candidate_dir/build/mcp" "$candidate_dir/build/mcp-tests" "$candidate_dir/build/router" "$candidate_dir/build/stdio-smoke" "$candidate_dir/build/benchmark-objects"
stack --stack-yaml "$stack_yaml" exec -- ghc -O0 -Wall \
  -i"$candidate_dir" -outputdir "$candidate_dir/build/protocol" \
  "$candidate_dir/Tests.hs" -o "$candidate_dir/build/protocol-tests" \
  -package grace -package aeson -package tasty -package tasty-hunit -package process
"$candidate_dir/build/protocol-tests"
stack --stack-yaml "$stack_yaml" exec -- ghc -O0 -Wall \
  -i"$candidate_dir" -outputdir "$candidate_dir/build/runner" \
  "$candidate_dir/Main.hs" -o "$candidate_dir/build/grace-native" \
  -package grace -package aeson -package process
stack --stack-yaml "$stack_yaml" exec -- ghc -O0 -Wall \
  -i"$candidate_dir" -outputdir "$candidate_dir/build/mcp-tests" \
  "$candidate_dir/MCPTests.hs" -o "$candidate_dir/build/mcp-tests-runner" \
  -package grace -package hs-mcp -package aeson -package tasty -package tasty-hunit -package process
"$candidate_dir/build/mcp-tests-runner" "$candidate_dir"
stack --stack-yaml "$stack_yaml" exec -- ghc -O0 -Wall \
  -i"$candidate_dir" -outputdir "$candidate_dir/build/mcp" \
  "$candidate_dir/MCPMain.hs" -o "$candidate_dir/build/grace-router-mcp" \
  -package grace -package hs-mcp -package aeson -package process
stack --stack-yaml "$stack_yaml" exec -- ghc -O0 -Wall \
  -i"$candidate_dir" -outputdir "$candidate_dir/build/router" \
  "$candidate_dir/RouterTests.hs" -o "$candidate_dir/build/router-tests" \
  -package grace -package aeson
"$candidate_dir/build/router-tests" "$candidate_dir"
stack --stack-yaml "$stack_yaml" exec -- ghc -O0 -Wall -Werror \
  -outputdir "$candidate_dir/build/stdio-smoke" \
  "$candidate_dir/MCPStdioSmoke.hs" -o "$candidate_dir/build/mcp-stdio-smoke" \
  -package aeson -package process -package unix
stack --stack-yaml "$stack_yaml" exec -- ghc -O0 -Wall \
  -i"$candidate_dir" -outputdir "$candidate_dir/build/benchmark-objects" \
  "$candidate_dir/experiments/Benchmark.hs" -o "$candidate_dir/build/context-benchmark" \
  -package grace -package aeson -package tasty -package tasty-hunit -package process
"$candidate_dir/build/context-benchmark" --test "$candidate_dir"
# Paid benchmark and native stdio smoke require explicit invocation/provider.
# This check runs only synthetic prompt handlers and never provider inference.
