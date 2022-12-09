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

import argparse
import random
import json
import sys
import os
from pathlib import Path

args = ["v0", "v1", "v2", "v3", "v4", "v5"]

ITE = False

def getArgs(exprstr):
    maxiarg = max([ int(arg[1:]) for arg in args if arg in exprstr ])
    return args[:maxiarg+1]

def gettype(exprstr):
    arith = [ "+", "-", "*" ]
    boolean = [ "&", "|", "^", "~" ] 

    res = 0
    for op in arith:
        if op in exprstr:
            res += 1
            break
    for op in boolean:
        if op in exprstr:
            res += 2
            break

    if res == 1:
        return "arith"
    elif res == 2:
        return "boolean"
    elif res == 3:
        return "mba"
    else:
        raise Exception("bug in gettype()")

def getnbbranches(exprstr):
    n = exprstr.count("If") + 1
    assert n > 1, "no if-then-else"
    return n
    
def If(cond, e1, e2):
    return e1 if cond else e2

def evalExpr(exprstr, args, values, bits=32):
    toeval = exprstr
    for i, arg in enumerate(args):
        #toeval = toeval.replace(arg, str(signed(values[i], bits)))
        toeval = toeval.replace(arg, str(values[i]))
    return eval(toeval) % 2**bits

def sample(exprstr, args, nsamples):
    samples = {
        "initial" : {
            "inputs": { str(i): {
                "location": arg,
                "size": "0x4",
                "value": "0x0"
            } for i, arg in enumerate(args) },
            "outputs": { 
                "0" : {
                    "location": "EAX",
                    "size": "0x4",
                    "value": hex(evalExpr(exprstr, args, [0 for _ in args]))
                } 
            }
        },
        "sampling": {
            str(sample) : {
                "inputs": { str(i): {} for i in range(len(args)) },
                "outputs": { "0" : {} }
            } for sample in range(nsamples)
        }
    }

    constvec = [0, 1, 0xffffffff, 0x7fffffff, 0x80000000 ]
    for sample in range(nsamples):
        if sample < 5:
            values = [ constvec[sample] for _ in args ]
        else:
            values = [ random.randint(-50, 50) % 2**32 for _ in args ]
        for i, arg in enumerate(args):
            samples["sampling"][str(sample)]["inputs"][str(i)]["location"] = arg
            samples["sampling"][str(sample)]["inputs"][str(i)]["size"] = "0x4"
            samples["sampling"][str(sample)]["inputs"][str(i)]["value"] = hex(values[i])

        samples["sampling"][str(sample)]["outputs"]["0"]["location"] = "EAX"
        samples["sampling"][str(sample)]["outputs"]["0"]["size"] = "0x4"
        samples["sampling"][str(sample)]["outputs"]["0"]["value"] = hex(evalExpr(exprstr, args, values))

    return samples

def sampleite(exprstr, args, nsamples):
    samples = {
        "initial" : {
            "inputs": { str(i): {
                "location": arg,
                "size": "0x4",
                "value": "0x0"
            } for i, arg in enumerate(args) },
            "outputs": { 
                "0" : {
                    "location": "EAX",
                    "size": "0x4",
                    "value": hex(evalExpr(exprstr, args, [0 for _ in args]))
                } 
            }
        },
        "sampling": {
            str(sample) : {
                "inputs": { str(i): {} for i in range(len(args)) },
                "outputs": { "0" : {} }
            } for sample in range(nsamples)
        }
    }
    
    # How many branches
    nbranches = getnbbranches(exprstr)

    for sample in range(nsamples):
        branchindex = int(sample / int(nsamples / nbranches))

        values = [ random.randint(-50, 50) % 2**32 if arg != "v2" or branchindex + 1 == nbranches else branchindex for arg in args ]
        for i, arg in enumerate(args):
            samples["sampling"][str(sample)]["inputs"][str(i)]["location"] = arg
            samples["sampling"][str(sample)]["inputs"][str(i)]["size"] = "0x4"
            samples["sampling"][str(sample)]["inputs"][str(i)]["value"] = hex(values[i])

        samples["sampling"][str(sample)]["outputs"]["0"]["location"] = "EAX"
        samples["sampling"][str(sample)]["outputs"]["0"]["size"] = "0x4"
        samples["sampling"][str(sample)]["outputs"]["0"]["value"] = hex(evalExpr(exprstr, args, values))

    return samples
    
if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('--bench', required=False, type=str, help="benchmark to sample")
    parser.add_argument('--expr', required=False, type=str, help="expression to sample")
    parser.add_argument('--n', required=False, type=int, help="number of samples per expression (default: 100)")
    parser.add_argument('--nargs', required=False, type=int, help="select expression with specific number of arguments")
    parser.add_argument('--type', required=False, type=str, help="select expressions' type : arith, boolean, mba")
    parser.add_argument('--ite', action="store_true", help="sample if-the-else branches uniformly")
    parser.add_argument('--out', required=False, type=str, help="output directory")
    arguments = parser.parse_args()

    assert arguments.bench != None or arguments.expr != None, "One of the option --expr and --bench must be chosen"
    assert arguments.bench == None or arguments.expr == None, "--expr and --bench cannot be set together"
    assert arguments.bench == None or arguments.out != None, "--bench and --out should both be set"

    if arguments.out != None:
        resdir = Path(arguments.out)
        if not resdir.exists():
            os.mkdir(resdir)
        elif not resdir.is_dir():
            print("[ERROR] {} must be a directory".format(arguments.out))
            sys.exit(1)


    if arguments.bench:
        samples_dir = Path("{}/samples/".format(arguments.out))
        if not samples_dir.exists():
            os.mkdir(samples_dir)
        if not samples_dir.is_dir():
            print("[ERROR] {}/samples/ must be a directory".format(arguments.out))
            sys.exit(1)
        if next(samples_dir.iterdir(), None) != None:
            print("[ERROR] {}/samples/ directory must be empty".format(arguments.out))
            sys.exit(1)

        assert arguments.type in [ None, "arith", "boolean", "mba" ], "unknown type '{}'".format(arguments.type)

        with open(arguments.bench, "r") as f:
            exprs = [ expr for expr in f.readlines() if expr.strip() != "" and "(*" not in expr ]

        for index, expr in enumerate(exprs):
            exprargs = getArgs(expr) 
            exprtype = gettype(expr)

            if arguments.nargs != None and arguments.nargs != len(exprargs):
                continue

            if arguments.type != None and arguments.type != exprtype:
                continue

            if arguments.ite:
                samples = sampleite(expr, exprargs, arguments.n if arguments.n != None else 100)
            else:
                samples = sample(expr, exprargs, arguments.n if arguments.n != None else 100)

            json.dump(samples, open("{}/samples/{}.json".format(arguments.out, index), "w"), indent=4, sort_keys=True)
    elif arguments.expr != None:
        exprargs = getArgs(arguments.expr) 
        samples = sample(arguments.expr, exprargs, arguments.n if arguments.n != None else 100)
        print(json.dumps(samples, indent=4, sort_keys=True))
