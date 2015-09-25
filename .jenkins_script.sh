#!/bin/bash

# NOTE: uses env vars:
#
#  * PROF -- control profiling.
#

set -e
set -x

STACK=stack

CFG=""
if [ "$PROF" == "" ] || [ "$PROF" == "0" ]; then
  CFG="$CFG --no-library-profiling "
# --disable-executable-profiling
else
  CFG="$CFG --library-profiling --executable-profiling"
fi

$STACK setup
$STACK test $CFG
