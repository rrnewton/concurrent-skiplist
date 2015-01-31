#!/bin/bash

echo "Begin running jenkins benchmark script for concurrent skiplist.  First use regression script to build packages:"
set -x

# Step 0: Configuration options:
# ================================================================================

TABLENAME=AdaptivelyScalable

# All regressions to be performed by Criterion:
REGRESSES="--regress=allocated:iters --regress=bytesCopied:iters --regress=cycles:iters \
   --regress=numGcs:iters --regress=mutatorWallSeconds:iters --regress=gcWallSeconds:iters \
   --regress=cpuTime:iters "

# Parfunc account LVish uploader project:
# CID=820162629229-kp29aklebt6ucos5a71u8tu3hu8unres.apps.googleusercontent.com
# SEC=pSsMxVAJCFKyWsazuxZVRZwX
# Over limit on [2014.11.12]

# if [[ "$HOSTNAME" =~ cutter ]]; then    
#    # Google API Project specific to CompactNF
#    CID=155144430612-v8nb20thmtg3eflt5bkkl40dssk6glnr.apps.googleusercontent.com    
#    SEC=9CEIm4AbJt5aSbw_sYpi85gi
#    export MACHINECLASS=cutter
# elif [ "$MACHINECLASS" == "swarm" ]; then
#    # Using generic uploader because we're over limit:
#    # Generic 2:
#    CID=546809307027-8tm2lp5gtqg5o3pn3s016gd6467cf7j3.apps.googleusercontent.com
#    SEC=148aQ08EPpgkb0DiYVLoT9X2
# else
    # Generic 3:
   CID=759282369766-ijonhc4662ot2qos4lgud0e0sltjshlj.apps.googleusercontent.com
   SEC=yI8GfZXsHPrW44udqklCHeDH    
# fi

# Step 1: Examine environment
# ================================================================================

if [ "$MACHINECLASS" == "" ]; then
    export MACHINECLASS=`hostname -s`
fi

echo "On linux platforms, check CPU affinity:"
taskset -pc $$ || echo ok

echo "Also check load:"
sar 2 2 || echo ok

echo "And who"
who -a || echo ok

# Switch to the top of the repo:
cd `dirname $0`

# Install dependencies and build the code:
# NOTEST=1 ./.jenkins_script.sh -j

echo "\nReturned to benchmarking script."

# CONVENTION: The working directory is passed as the first argument.
CHECKOUT=$1
shift || echo ok

if [ "$CHECKOUT" == "" ]; then
  CHECKOUT=`pwd`
fi
if [ "$JENKINS_GHC" == "" ]; then
   echo "JENKINS_GHC unset"
   export JENKINS_GHC=7.8.3
fi

echo "Running benchmarks remotely on server `hostname`"
if [ -f "$HOME/continuous_testing_setup/rn_jenkins_scripts/acquire_ghc.sh" ]; then
  source $HOME/continuous_testing_setup/rn_jenkins_scripts/acquire_ghc.sh
fi

which cabal
cabal --version
which ghc
ghc --version


# BKUP=$HOME/criterion_reports/
BKUP=$HOME/results_backup_adaptively_scalable/
mkdir -p $BKUP
mkdir -p ./old_reports/
mv report_* ./old_reports/

gitdepth=`git log --pretty=oneline | wc -l`
set -e


# Step 1: build or acquire HSBencher stuff
# ================================================================================

# For now we just preinstall these hsbencher executables:
CSVUPLOAD=hsbencher-fusion-upload-csv-0.3.7
CRITUPLOAD=hsbencher-fusion-upload-criterion-0.3.7

# Step 2: Run benchmarks
# ================================================================================

executable=bench-concurrent-skiplist
    
cabal sandbox init

TAG=`date +'%s'`
    
echo "Installing benchmark program."
which -a ghc-$JENKINS_GHC
cabal install -w ghc-$JENKINS_GHC --with-ghc-pkg=ghc-pkg-$JENKINS_GHC --enable-benchmarks
cabal configure --enable-benchmarks
cabal build ${executable}

# Vary parameter?  
#  for ((depth=1; depth<=$MAXTREEHEIGHT; depth++)); do
REPORT=report_${executable}_${TAG}_${depth}
CRITREPORT=$REPORT.crit
CSVREPORT=$REPORT.csv

time ./dist/build/$executable/$executable --raw $CRITREPORT $REGRESSES +RTS -T -s

# Convert to raw .csv file for backup and bulk upload:
$CRITUPLOAD --noupload --csv=$CSVREPORT --variant=$VARIANT --args=$depth $CRITREPORT

cp $CSVREPORT ${BKUP}/${gitdepth}_${CSVREPORT} || echo "Hmm, why did that copy fail?"

$CSVUPLOAD $CSVREPORT --fusion-upload --name=$TABLENAME --clientid=$CID --clientsecret=$SEC
