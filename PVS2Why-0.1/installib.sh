#!/bin/bash
if [ -h ../$1 ]; then
  echo "*** Warning: It seems that there was a previous installation of $1."
  rm -f ../$1;
fi 
dir=`pwd`
ln -s $dir/lib ../$1 
echo "$1 has been installed. Try: \$make test PVS_DIR=<pvs-dir>"
