#!/bin/bash

# This script is supposed to be run inside a planex-buildenv container, for a
# specific xenserver branch, in order to generate a whitelist of packages
# needed to build all the components in which we have Travis CI enabled on
# GitHub (for that branch). The whitelist for that branch can then be merged
# into the existing whitelist in update_xs_yum.ml

set -e

# update this list to support all the packages for which we will use Travis CI
SOURCES="xapi.src xenopsd.src xcp-networkd.src forkexecd.src xcp-rrdd.src squeezed.src vhd-tool.src xenops-cli.src gpumon.src rrdd-plugins.src"
PACKAGES=`echo $SOURCES | sed 's/\.src//g'`

# get the dependencies, excluding upstream ones, as those can be obtained directly and shouldn't need uploading
BUILD_DEPS=`repoquery --disablerepo=base --disablerepo=epel --disablerepo=extras --disablerepo=updates --requires --recursive --resolve $SOURCES | sed 's/^\(.*\)\-[^\-]*\-[^\-]*$/\1/'`

echo $PACKAGES $BUILD_DEPS | tr " " "\n" | sort | uniq

