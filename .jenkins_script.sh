#!/bin/bash

# ========================================
# Generic GHC package testing setup:
# ========================================

# NOTE: uses env vars JENKINS_GHC and CABAL_FLAGS, if available.
#       Also passes through extra args to the major cabal install command.

set -e
set -x

# Temporarily staying off of 1.20 due to cabal issue #1811:
# CABAL=cabal-1.18.0
# Fixed now [2015.05.03]:
CABAL=cabal-1.22
SHOWDETAILS=always
# SHOWDETAILS=streaming

if [ "$JENKINS_GHC" == "" ]; then 
  GHC=ghc
else
  ENVSCRIPT=$HOME/rn_jenkins_scripts/acquire_ghc.sh
  # This is specific to our testing setup at IU:
  if [ -f "$ENVSCRIPT" ]; then 
    source "$ENVSCRIPT"
  fi
  GHC=ghc-$JENKINS_GHC
fi

TOP=`pwd`
$CABAL sandbox init
$CABAL sandbox hc-pkg list

# # No packages deeper in the repo curretnly:
# for path in $PKGS; do 
#   cd $TOP/$path
#   $CABAL sandbox init --sandbox=$TOP/.cabal-sandbox
# done
# cd $TOP

CFG=" --force-reinstalls "

if [ "$PROF" == "" ] || [ "$PROF" == "0" ]; then 
  CFG="$CFG --disable-library-profiling "
# --disable-executable-profiling
else
  CFG="$CFG --enable-library-profiling --enable-executable-profiling"
fi  

# Install dependencies in parallel, including those needed for testing:
$CABAL install $CFG $CABAL_FLAGS --with-ghc=$GHC --enable-tests --only-dep $*
# Install the packages WITHOUT testing:
$CABAL install $CFG $CABAL_FLAGS --with-ghc=$GHC $*


GHC_VER=`$GHC --version | egrep -o '[0123456789]+\.[0123456789]+\.[0123456789]+'`
MAJOR=`echo $GHC_VER | sed 's/\.[[:digit:]]*$//'`
OLDVER=`echo "$MAJOR < 7.8" | bc`

# Avoiding the atomic-primops related bug on linux / GHC 7.6:
# Temporarily just DONT test under linux, fixme!!
if [ `uname` == "Linux" ] && [ "$OLDVER" == 1 ] ; 
then  
  echo "Skipping tests!"
else
  echo "Testing packages."
  cd $TOP/$path
  # Assume cabal 1.20+:
  $CABAL test --show-details=$SHOWDETAILS
fi
