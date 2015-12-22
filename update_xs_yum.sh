#!/bin/bash

set -e
set -x

today=`date +"%Y-%m-%d"`
today=2015-12-18
wget http://downloadns.citrix.com.edgesuite.net/8170/${today}/binpkg.iso
for i in `iso-info -f binpkg.iso | grep domain0/RPMS | grep rpm | awk '{print $2}'`; do mkdir -p .`dirname $i`; iso-read -i binpkg.iso -e $i -o ./$i; done
rm binpkg.iso
wget http://downloadns.citrix.com.edgesuite.net/8170/${today}/source.iso
mkdir -p domain0/SRPMS
for i in `iso-info -f source.iso | grep src.rpm | grep -v guest | awk '{print $2}'`; do mkdir -p .`dirname $i`; iso-read -i source.iso -e $i -o domain0/SRPMS/$i; done
rm source.iso
cd domain0
createrepo .
cd ..
rsync -avz domain0 jon@bark.recoil.org:/data/www/www.recoil.org/~jon/xs-dundee



