#!/usr/bin/env bash

STACK_FLAGS="
  --flag bits-extra:bmi2
  --flag hw-rankselect-base:bmi2
  --flag hw-rankselect:bmi2
  --flag hw-simd:bmi2
  --flag hw-simd:avx2
  --flag hw-simd-cli:bmi2
  --flag hw-simd-cli:avx2
"

case $1 in
  build)
    stack build $STACK_FLAGS \
      --test --no-run-tests --bench --no-run-benchmarks \
    ;;

  test)
    stack test $STACK_FLAGS
    ;;

  bench)
    stack bench $STACK_FLAGS
    ;;

  repl)
    stack repl $STACK_FLAGS
    ;;

  install)
    stack install $STACK_FLAGS
    ;;
esac
