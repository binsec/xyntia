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
import json
import os
import sys
from joblib import Parallel, delayed
from pathlib import Path
from plumbum import local
import tempfile
from tqdm import tqdm

UTOPCONST = None

xyntia = None
timeout = local["timeout"]
cegis_to = None

def exec_xyntia(inifile, cmdargs, sampledir):
    with tempfile.NamedTemporaryFile(mode="w") as tmp:
        tmp.flush()
        if cegis_to != None:
            output = timeout[
                "-s", "ABRT", to,
                xyntia, 
                cmdargs,
                "-json",
                "-sampleout", sampledir,
                "-bin", tmp.name,
                "-config", inifile.name,
            ](retcode=[0, 124])

        else:
            retcode, output, stderr = xyntia[cmdargs][
                "-json",
                "-sampleout", sampledir,
                "-bin", tmp.name,
                "-config", inifile.name,
            ].run()
        
        if not (sampledir / "res.json").exists():
            # the output was a constant value
            os.remove(sampledir / "formula")
            sampledir.rmdir()
            return None

        errormsg = "Not enough time to start synthesis"
        if errormsg in output:
            return {
                "success": "no",
                "equiv": "no",
                "errormsg": errormsg,
            }
        else:
            return json.loads(output)

def synthesize(index, expr, nsamples, cmdargs, outdir):
    ini = """\
starting from 0x0

prune constant outputs

set optimal sampling 

explore all

v0<32> := nondet
v1<32> := nondet
v2<32> := nondet
v3<32> := nondet
v4<32> := nondet
v5<32> := nondet
v6<32> := nondet
set domain v0 [-50, 50]
set domain v1 [-50, 50]
set domain v2 [-50, 50]
set domain v3 [-50, 50]
set domain v4 [-50, 50]
set domain v5 [-50, 50]
set domain v6 [-50, 50]

hook 0x0 with 
    res<32> := {}
    sample {} res
    halt
end
""".format(expr, nsamples)

    sampledir = outdir / "samples" / f"{index}"
    resfile = outdir / "synthesized" / f"{index}.json"

    with tempfile.NamedTemporaryFile(mode="w") as tmp:
        tmp.write(ini)
        tmp.flush()
        res = exec_xyntia(tmp, cmdargs, sampledir)

    if res == None: 
        # the expression to sample was a constant value
        return None

    res_stats = {
        "success": res["success"],
        "quality": None,
        "equiv": res["equiv"],
    }

    if res["equiv"] != "no" and res["orig_size"] > 0:
        # if we check equivalence, we compute the quality of expression that are equivalent
        # or at least not proven not equivalent
        res["quality"] = res["synth_size"] / res["orig_size"]
        res_stats["quality"] = res["quality"]

    with open(resfile, "w") as f:
        json.dump(res, f, indent=4, sort_keys=True)

    return res_stats


def synthesize_parallel(njobs, cmdargs, exprs, nsamples, outdir):
    all_res_stats = Parallel(n_jobs=njobs)(delayed(synthesize)(index, expr, nsamples, cmdargs, outdir) 
            for index, expr in enumerate(tqdm(exprs, leave=False)))

    all_res_stats = [ v for v in all_res_stats if v != None ] # Remove constant expressions from stats
    successes = [ 1 if stat["success"] == "yes" else 0 for stat in all_res_stats ]
    qualities = [ stat["quality"] for stat in all_res_stats if stat["quality"] != None ]
    equiv_proven = [ 1 if stat["equiv"] == "yes" else 0 for stat in all_res_stats ]
    equiv_optim = [ 1 if stat["equiv"] != "no" else 0 for stat in all_res_stats ]

    assert len(successes) != 0 and (len(equiv_proven) != 0 and len(equiv_optim) != 0), "No expression to synthesize"
        
    successrate = round(100*sum(successes) / len(successes), 1) if len(successes) != 0 else 0
    print("Success rate : {}%".format(successrate))

    equivmin = round(100*sum(equiv_proven) / len(equiv_proven), 1) if len(equiv_proven) != 0 else 0
    equivmax = round(100*sum(equiv_optim) / len(equiv_optim), 1) if len(equiv_optim) != 0 else 0
    print("Equiv range : {} - {}%".format(equivmin, equivmax))

    mqual = round(sum(qualities) / len(qualities), 2) if len(qualities) != 0 else None
    print("Mean Quality : {}".format(mqual))


def check_dir(directory):
    if not directory.exists():
        print("[ERROR] {} must be a directory".format(directory))
        sys.exit(1)
    elif not directory.is_dir():
        print("[ERROR] {} must be a directory".format(directory))
        sys.exit(1)

def check_create(directory):
    if not directory.exists():
        os.mkdir(directory)
    else:
        print("[ERROR] {} already exists".format(directory))
        sys.exit(1)

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('--parallel', action="store_true", help="Run synthesis in parallel")
    parser.add_argument('--dataset', required=True, type=str, help="benchmark file")
    parser.add_argument('--cegis-to', required=False, type=int, help="timeout of the cegis process")
    parser.add_argument('--nsamples', required=False, type=int, help="number of samples (default: 100)")
    parser.add_argument('--out', required=True, type=str, help="output directory")
    parser.add_argument('cmd', nargs="+", type=str, help="command to bench")
    arguments = parser.parse_args()

    resdir = Path(arguments.out)
    check_dir(resdir)

    sampledir = resdir / "samples"
    check_create(sampledir)

    synthesized_dir = resdir / "synthesized"  
    check_create(synthesized_dir)

    njobs = -1 if arguments.parallel else 1
    xyntia = local[arguments.cmd[0]]
    cmdargs = arguments.cmd[1:]
    cegis_to = arguments.cegis_to
    nsamples = arguments.nsamples if arguments.nsamples != None else 100

    with open(arguments.dataset, "r") as f:
        exprs = [ e for e in f.readlines() if e.strip() != "" and not e.strip().startswith("#") ]

    synthesize_parallel(njobs, cmdargs, exprs, nsamples, Path(arguments.out))
