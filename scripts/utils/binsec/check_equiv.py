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
from tqdm import tqdm
from z3 import Solver, sat, unsat, unknown
import argparse
import json

def resformat(res):
    res = res.replace("<32>", "")
    res = res.replace("<16>", "")
    res = res.replace("<8>", "")
    return res

def res_smtlib(resfile):
    with open(resfile, "r") as f:
        res = [ line.split(":")[1] for line in f.readlines() if line.startswith("smtlib") ][0].strip()
    return resformat(res)

def ctx_smtlib(data):
    return data["symbolic"]["ctx"]


def orig_smtlib(data):
    return data["symbolic"]["smtlib"]

def success(resfile):
    with open(resfile, "r") as f:
        res = [ line.split(":")[1] for line in f.readlines() if line.startswith("success") ][0].strip()
    return res == "yes"

def check_result(resfile, samplefile):
    with open(samplefile, "r") as f:
        data = json.load(f)
   
    xyntia_res = res_smtlib(resfile) 
    ctx = ctx_smtlib(data)
    orig = orig_smtlib(data)
    output = data["initial"]["outputs"]["0"]["location"]
    
    meminputs = ""
    for inp in data["initial"]["inputs"].values():
        if "mem_" in inp["location"]:
            meminputs += "(define-fun {name} () (_ BitVec {size}) {formula})\n".format(name=inp["location"], size=8*int(inp["size"], 16), formula=inp["name"])

    assertion = "(assert (distinct {} {}))".format(output, xyntia_res)

    full_formula = ctx + meminputs + assertion + "\n(check-sat)\n(exit)"

    solv = Solver()
    solv.set("timeout", 10 * 1000)
    solv.from_string(full_formula)
    res = solv.check()
    if res == sat:
        return "no"
    elif res == unsat:
        return "yes"
    else:
        return "ukn"


def already_checked(resfile):
    with open(resfile, "r") as f:
        data = f.read()
    return "equiv:" in data

def append_equiv(filename, equiv):
    with open(filename, "a") as f:
        f.write("\nequiv: {}\n".format(equiv))

def main(directory, name):
    sampledir = Path("{}/{}/samples".format(directory, name))
    resdir = Path("{}/{}/synthesized".format(directory, name))
    length = sum(1 for _ in Path(sampledir).glob("*/*.json"))
    for samplefile in tqdm(Path(sampledir).glob("*/*.json"), total=length):
        resfile = resdir / samplefile.parent.name / (samplefile.stem + ".txt")
        if (not resfile.exists()) or already_checked(resfile):
            continue
        if success(resfile):
            equiv = check_result(resfile, samplefile)
        else:
            equiv = "no"
        append_equiv(resfile, equiv)
        

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('--dir', required=True, type=str, help="directory where are stored results")
    args = parser.parse_args()
    
    directory = Path(args.dir).parent.stem
    name = Path(args.dir).stem

    main(directory, name)


