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

from pathlib import Path
import argparse
import json

def mean(l):
    if len(l) > 0:
        return sum(l) / len(l)
    else:
        return None

def display(res):
    print("Success Rate: {}%".format(res["nsucc"] * 100 / res["total"]))
    print("Equiv Rate: {} - {}%".format(
            res["nequiv"] * 100 / res["total"],
            (res["nequiv"] + res["nukn"]) * 100 / res["total"]
    ))
    print("Mean quality: {}".format(mean(res["quals"])))

def size(expr):
    """
    Size in term of number of operators, variables and constant values
    """
    l = [ x for x in expr.replace("(", "").replace(")", "").split() if x.strip() != "" ]
    return len(l)

def main(resdir, samplesdir):
    
    res = {
        "nsucc": 0,
        "nequiv": 0,
        "nukn": 0,
        "total": 0,
        "quals" : [],
    }

    for filepath in resdir.glob("*/*.json"):
        with open(filepath, "r") as f:
            resdata = json.load(f)

        with open(samplesdir / filepath.parent.name / filepath.name, "r") as f:
            sampdata = json.load(f)

        if resdata["success"] == "yes":
            res["nsucc"] += 1
            res["quals"].append(size(resdata["simplified"]) / sampdata["info"]["exprsize"])

        if resdata["equiv"] == "yes":
            res["nequiv"] += 1
        elif resdata["equiv"] == "ukn":
            res["nukn"] += 1

        res["total"] += 1

    display(res)

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('--resdir', required=True, type=str, help="results directory")
    parser.add_argument('--sampdir', required=True, type=str, help="samples directory")
    args = parser.parse_args()

    main(Path(args.resdir), Path(args.sampdir))
