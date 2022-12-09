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

from pathlib import Path
from plumbum import local
import argparse
import json
import os
import sys

#xyntia = local["/home/greg/Documents/CEA/CAUIM/cauim-greybox-program-synthesis/_build/default/bin/main.exe"]
xyntia = local["xyntia"]

def main(sampledir, timeout, outdir):
    for samplefile in Path(sampledir).glob("*.json"):
        try:
            res = xyntia["-heur", "ils", "-time", timeout, "-dist", "arith", "-strat", "full", samplefile]()
            with open(outdir + "/" + samplefile.stem + ".txt", "w") as f:
                f.write(res)
        except Exception as e:
            print(e)

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('--dir', required=True, type=str, help="directory containing samples (with 1 output)")
    parser.add_argument('--timeout', required=True, type=int, help="synthesis timeout")
    parser.add_argument('--out', required=True, type=str, help="output directory")
    
    args = parser.parse_args()
    
    if os.path.exists(args.out):
        print("[ARG ERROR] --out should not exists", file=sys.stderr)
        sys.exit(1)

    os.mkdir(args.out)

    main(args.dir, args.timeout, args.out)
