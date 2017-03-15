#!/bin/bash

# This script is supposed to be run inside a planex-buildenv container
# in order to generate a whitelist of packages needed to build all the
# components in which we have Travis CI enabled on GitHub.

set -e

# update this list to support all the packages which we will use Travis CI
PACKAGES="xapi.src xenopsd.src xcp-networkd.src forkexecd.src xcp-rrdd.src squeezed.src vhd-tool.src xenops-cli.src gpumon.src"

# print out the dependencies, excluding ones from CentOS, as those can be obtained directly from them and shouldn't need uploading
repoquery --disablerepo=base --disablerepo=epel --disablerepo=extras --disablerepo=updates --requires --recursive --resolve $PACKAGES | sort | sed 's/^\(.*\)\-[^\-]*\-[^\-]*$/\1/' | uniq
