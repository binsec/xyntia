#!/bin/bash
##########################################################################
#  This file is part of BINSEC.                                          #
#                                                                        #
#  Copyright (C) 2019-2022                                               #
#    CEA (Commissariat à l'énergie atomique et aux énergies              #
#         alternatives)                                                  #
#                                                                        #
#  you can redistribute it and/or modify it under the terms of the GNU   #
#  Lesser General Public License as published by the Free Software       #
#  Foundation, version 2.1.                                              #
#                                                                        #
#  It is distributed in the hope that it will be useful,                 #
#  but WITHOUT ANY WARRANTY; without even the implied warranty of        #
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         #
#  GNU Lesser General Public License for more details.                   #
#                                                                        #
#  See the GNU Lesser General Public License version 2.1                 #
#  for more details (enclosed in the file licenses/LGPLv2.1).            #
#                                                                        #
##########################################################################

set -e # force the script to exit at the first error

bin=$1
timeout=$2
out=$3

resdir=$PWD/$out/$(basename $bin)/synthesized
cfgdir=$PWD/$out/$(basename $bin)/samples

if [ -d $resdir ];then
    echo "[ARG ERROR] '$resdir' already exists" 
    exit 1
else
    mkdir $resdir
fi

i=0
for blockdir in $(ls -d $cfgdir/*/);do
    echo $i
    python3 ./scripts/utils/synthesize_block.py --dir $blockdir --timeout $timeout --out $resdir/$(basename $blockdir)
    i=$((i+1))
done | tqdm --total $(ls -d $cfgdir/*/ |wc -l) >> /dev/null
