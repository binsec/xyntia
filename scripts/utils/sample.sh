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

bin=$1
out=$2

if [[ "$bin" = "" ]];then
    echo "[ARG ERROR] $(basename $0) takes as argument the directory of the binary"
    exit 1
fi

if [[ "$out" = "" ]];then
    echo "[ARG ERROR] $(basename $0) takes as argument the output directory"
    exit 1
fi

binname=$(basename $bin)
cfgdir="$PWD/$out/$binname/cfgs"
samplesdir="$PWD/$out/$binname/samples"

mkdir $samplesdir

i=0
for trace in $(ls $cfgdir);do
    echo $i
    #echo [INFO] Sample $cfgdir/$trace
    outdir="$samplesdir/${trace%.*}"

    python3 ./scripts/utils/binsec/sample.py --bin $cfgdir/$trace --out $outdir
    i=$((i+1))
done | tqdm --total $(ls $cfgdir |wc -l) >> /dev/null

