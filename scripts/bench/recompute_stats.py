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

#!/usr/bin/env python3

from pathlib import Path
import argparse
import json

def mean(l):
    if len(l) == 0:
        return None
    return round(sum(l) / len(l), 4)

def display_stats(stats):
    print("Mean success rate: {}%".format(100*mean(stats["successes"])))
    print("Mean equiv. rate: {} - {}%".format(100*mean(stats["equiv_lower"]), 100*mean(stats["equiv_upper"])))
    print("Mean quality: {}".format(mean(stats["qualities"])))

def main(directory):
    stats = {
        "successes": [],
        "equiv_lower": [],
        "equiv_upper": [],
        "qualities": []
    }

    for resfile in (directory / "synthesized").glob("*/*.json"):
        with open(resfile, "r") as f:
            res = json.load(f)
        
        stats["successes"].append(res["success"] == "yes")
        if res["success"] == "yes":
            stats["equiv_lower"].append(res["equiv"] == "yes")
            stats["equiv_upper"].append(res["equiv"] != "no")
            if res["equiv"] != "no" and res.get("quality"):
                stats["qualities"].append(res["quality"])
        else:
            stats["equiv_lower"].append(False)
            stats["equiv_upper"].append(False)




    display_stats(stats)



if __name__  == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('--dir', required=True, type=str, help="directory where results are stored")
    args = parser.parse_args()
    main(Path(args.dir))
