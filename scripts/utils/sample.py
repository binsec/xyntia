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

from plumbum import local
import argparse
import tempfile


xyntia = local["xyntia"]

config = """\
starting from 0x0

set optimal sampling

prune constant outputs
explore all


hook  <.raw:size> with 
    sample 100
    halt
end
"""

def main(outdir, binary, arch):
    with tempfile.NamedTemporaryFile(mode="w") as tmp:
        tmp.write(config)
        tmp.flush()
        xyntia["-isa", arch,
               "-sample-only",
               "-bin", binary, "-config", tmp.name,
               "-sampleout", outdir
        ]()
    return 

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('--bin', required=True, type=str, help="binary to load")
    parser.add_argument('--arch', required=True, type=str, help="architecture to give to binsec")
    parser.add_argument('--out', required=True, type=str, help="output directory")
    args = parser.parse_args()

    main(args.out, args.bin, args.arch)
