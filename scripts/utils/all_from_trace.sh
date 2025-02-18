#!/bin/bash
##########################################################################
#  This file is part of BINSEC.                                          #
#                                                                        #
#  Copyright (C) 2019-2025                                               #
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

all=false
sample=false
synthesis=false
gdb=false
ghidra=false
outdir=""
ARCH="x86"

manual="""\
usage: $0 --outdir <path> [--all] [--gdb] [--sample] [--learn] -- binary arg1 arg2 ...

ENVIRONEMENT VARIABLES:
    XYNTIA                              : the xyntia command to run
    GHIDRA                              : ghidra directory path

arguments:
    -o / --outdir <path>                : path to store results
    -a / --all                          : extract traces/cfg, sample and synthesize (equivalent to -gh -s -l -c) 
    -g / --gdb                          : extract traces using GDB
    -gh / --ghidra                      : extract blocks using ghidra (no need to give binary argument)
    -s / --sample                       : sample blocks of code
    -l / --learn                        : synthesize blocks of code
    -ar / --arch                        : the architecture to use among x86, amd64 (default x86)
"""

POSITIONAL=()
while [[ $# -gt 0 ]];do
    key="$1"
    
    case $key in
        -a|--all)
        all=true
        shift # past argument
        ;;
        -g|--gdb) # Exrtact blocks using GDB
        gdb=true
        shift # past argument
        ;;
        -gh|--ghidra) # Extract blocks using Ghidra (default)
        ghidra=true
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
        -o|--outdir)
        outdir="$2"
        shift # past argument
        shift # past value
        ;;
        -ar|--arch)
        ARCH="$2"
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

if [[ "$XYNTIA" = "" ]];then
    echo -e "${RED}[arg error] the XYNTIA variable must be set${NC}"
    exit 1
fi

if [[ "$outdir" = "" ]];then
    echo -e "${RED}[arg error] option -o/--outdir must be set${NC}"
    echo -e "$manual"
    exit 1
fi

if [[ "$bin" = "" ]];then
    echo -e "${RED}[arg error] no path to binary given${NC}"
    echo -e "$manual"
    exit 1
fi

if $all;then
    # If $all set to true, then set all other options to true
    ghidra=true
    sample=true
    synthesis=true
fi

cfgdir="$outdir/$(basename $bin)/cfgs"
samplesdir="$outdir/$(basename $bin)/samples"
resdir="$outdir/$(basename $bin)/synthesized"


echo -e "$GREEN[CONFIG] Xyntia command: $XYNTIA$NC"
echo -e "$GREEN[CONFIG] Binary to analyze: $bin $args$NC"

if [ ! -d "$outdir/$(basename $bin)" ];then
    mkdir -p "$outdir/$(basename $bin)"
fi

if $gdb || $ghidra;then
    echo -e "$GREEN[CONFIG] Save CFGs to \"$cfgdir\"$NC"
fi
if $sample;then
    echo -e "$GREEN[CONFIG] Save samples to \"$samplesdir\"$NC"
fi
if $synthesis;then
    echo -e "$GREEN[CONFIG] Save synthesis results to \"$outdir/$(basename $bin)/synthesized\"$NC"
fi

if $gdb || $ghidra;then
    # Extract blocks of code from execution trace
    echo -e "$GREEN[INFO] Extrat CFG graph ($bin $args) to ${cfgdir}${NC}"
    mkdir $cfgdir
    if $gdb; then
        echo -e "$GREEN[INFO] Run GDB to extract CFG${NC}"
        gdb -batch-silent -ex "py args = \"$args\"; outdir = \"$cfgdir\"" -x scripts/utils/gdbscript.txt $bin
    else
        echo -e "$GREEN[INFO] Run Ghidra to extract CFG${NC}"
        tmpfile=/tmp/$(basename $bin)_$(cat /dev/urandom | tr -dc '[:alpha:]' | fold -w ${1:-20} | head -n 1).$(date +"%H-%M-%S-%N")
        mkdir $tmpfile
        ${GHIDRA}/support/analyzeHeadless $tmpfile empty -import $bin -postscript scripts/utils/ghidra/DumpBlocks.py $cfgdir 1> /dev/null
        rm -rf $tmpfile
    fi

fi

if $sample;then
    # Sample each block of code
    echo -e "$GREEN\n[INFO] Sample each block$NC"
    mkdir $samplesdir
    for block in $(ls $cfgdir/*.bin);do
        echo $block
        blockfile=$(basename $block)
        addr=${blockfile%.*}
        $PYTHON ./scripts/utils/sample.py --bin $block --arch $ARCH --out $samplesdir/$addr
    done
fi

if $synthesis;then
    # Synthesize each block of code
    echo -e "$GREEN\n[INFO] Synthesize each block$NC"
    mkdir $resdir
    for samples in $(ls $samplesdir);do
        echo $samplesdir/$samples
        mkdir $resdir/$samples
        for sample in $(ls $samplesdir/$samples/*.json);do
            bname=$(basename $sample)
	    dirname=$(dirname $sample)
            $XYNTIA -json $sample -formula $dirname/formula > $resdir/$samples/$bname
        done
    done
fi

echo -e "$GREEN\n[INFO] Compute statistics$NC"
$PYTHON ./scripts/utils/get_stats.py --resdir $resdir --sampdir $samplesdir
