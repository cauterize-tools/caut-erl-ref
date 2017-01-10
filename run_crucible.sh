#!/bin/sh

rm -rf crucible-*

stack exec crucible -- tester                                                                \
  --build-cmd="stack exec caut-erl-ref -- --spec=%s --output=src"                            \
  --build-cmd="erlc src/*.erl"                                                               \
  --run-cmd="tee data.bin | ../../crucible/crucible.erl"                                     \
  --schema-count=10                                                                          \
  --instance-count=100                                                                       \
  --type-count=10                                                                            \
  --enc-size=1048576                                                                         \
