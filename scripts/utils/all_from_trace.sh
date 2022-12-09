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

set -e # force the script to exit if any command failed


GREEN='\033[1;32m'
RED='\033[0;31m'
NC='\033[0m'

PYTHON=python3

bin=""
args=""
timeout=60

all=false
sample=false
synthesis=false
check=false
gdb=false
outdir=""

manual="""\
usage: $0 --outdir <path> [--all] [--gdb] [--sample] [--learn] [--check] [--timeout] [--extractor] -- binary arg1 arg2 ...

arguments:
    -o / --outdir <path>                : path to store results
    -a / --all                          : extract traces/cfg, sample and synthesize (equivalent to -g -s -l -c) 
    -g / --gdb                          : extract traces using GDB
    -s / --sample                       : sample blocks of code
    -l / --learn                        : synthesize blocks of code
    -c / --check                        : check equivalence between sythesized and extracted formula
    -t / --timeout                      : synthesis timeout
"""

POSITIONAL=()
while [[ $# -gt 0 ]];do
    key="$1"
    
    case $key in
        -a|--all)
        all=true
        shift # past argument
        ;;
        -g|--gdb) # Exrtact blocks using GDB (Default)
        gdb=true
        shift # past argument
        ;;
        -s|--sample)
        sample=true
        shift # past argument
        ;;
        -l|--learn)
        synthesis=true
        shift # past argument
        ;;
        -c|--check)
        check=true
        shift # past argument
        ;;
        -t|--timeout)
        timeout="$2"
        shift # past argument
        shift # past value
        ;;
        -o|--outdir)
        outdir="$2"
        shift # past argument
        shift # past value
        ;;
        -h|--help)
        echo -e "$manual"
        exit 0
        ;;
        --)
        bin="$2"
        shift # past argument
        shift # past value
        args=$@
        break
        ;;
        *)    # unknown option
        POSITIONAL+=("$1") # save it in an array for later
        shift # past argument
        ;;
    esac
done
set -- "${POSITIONAL[@]}" # restore positional parameters

if [[ "$outdir" = "" ]];then
    echo -e "${red}[arg error] option -o/--outdir must be set${nc}"
    echo -e "$manual"
    exit 1
fi

if [[ "$bin" = "" ]];then
    echo -e "${red}[arg error] no path to binary given${nc}"
    echo -e "$manual"
    exit 1
fi

if $all;then
    # If $all set to true, then set all other options to true
    gdb=true
    sample=true
    synthesis=true
    check=true
fi

cfgdir="$outdir/$(basename $bin)/cfgs"
samplesdir="$outdir/$(basename $bin)/samples"

if [ ! -d "./$outdir/$(basename $bin)" ];then
    mkdir -p "./$outdir/$(basename $bin)"
fi

if $gdb;then
    echo -e "$GREEN[CONFIG] Save CFGs to \"$cfgdir\"$NC"
fi
if $sample;then
    echo -e "$GREEN[CONFIG] Save samples to \"$samplesdir\"$NC"
fi
if $synthesis;then
    echo -e "$GREEN[CONFIG] Save synthesis results to \"$outdir/$(basename $bin)/synthesized\"$NC"
fi

if $gdb;then
    # Extract blocks of code from execution trace
    echo -e "$GREEN[INFO] Extrat CFG graph ($bin $args) to ${cfgdir}${NC}"

    mkdir $cfgdir
    echo -e "$GREEN[INFO] Run GDB to extract CFG${NC}"
    gdb -batch-silent -ex "py args = \"$args\"; outdir = \"$cfgdir\"" -x scripts/utils/gdbscript.txt $bin
fi

if $sample;then
    # Sample each block of code
    echo -e "$GREEN\n[INFO] Sample each block$NC"
    ./scripts/utils/sample.sh $bin $outdir
fi

if $synthesis;then
    # Synthesize each block of code
    echo -e "$GREEN\n[INFO] Synthesize each block$NC"
    ./scripts/utils/synthesize_cfg.sh "$bin" "$timeout" "$outdir"
fi

if $check;then
    echo -e "$GREEN\n[INFO] Check equivalence$NC"
    $PYTHON ./scripts/utils/binsec/check_equiv.py --dir "$outdir/$(basename $bin)"
fi
