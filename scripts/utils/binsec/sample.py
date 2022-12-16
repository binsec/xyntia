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

from multiprocessing import Pool
from pysmt.smtlib.parser import SmtLibParser
from plumbum import local
from random import randint
from six.moves import cStringIO
import argparse
import json
import os
import sys
import z3

RED = "\33[0;31m"
GREEN = "\33[1;32m"
NC = "\33[0m"

ARCH = None

binsec = local["binsec"]

regs_32 = [ "eax", "ebx", "ecx", "edx", "ebp", "esp", "esi", "edi" ]
regs_64 = [ "rax", "rbx", "rcx", "rdx", "rbp", "rsp", "rsi", "rdi", "r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15" ]


class MemoryLocation:
    
    clindex = 0

    def __init__(self, mem, size):
        self.mem = mem
        self.index = MemoryLocation.clindex
        MemoryLocation.clindex += 1
        self.size = size

    def getLoc(self):
        return "mem_{}".format(self.index)

    def getName(self):
        return self.mem.to_smtlib()

    def getSize(self):
        return self.size

class RegisterLocation:
    
    clindex = 0

    def __init__(self, reg, size):
        self.reg = reg
        self.index = RegisterLocation.clindex
        RegisterLocation.clindex += 1
        self.size = size

    def getLoc(self):
        return self.getName()

    def getName(self):
        return self.reg.to_smtlib()

    def getSize(self):
        return self.size

class RegisterWrite:
    def __init__(self, name, size):
        self.name = name
        self.size = size

    def getBaseName(self):
        return ''.join(filter(str.isalpha, self.getLoc()))

    def getName(self):
        return self.getLoc()

    def getLoc(self):
        return self.name

    def getSize(self):
        return self.size

class MemoryWrite:

    clindex = 0

    def __init__(self, mem, stores, basesize):
        self.mem = mem
        self.stores = stores
        self.basesize = basesize
        self.index = MemoryWrite.clindex
        MemoryWrite.clindex += 1

    def remove(self, memorywrite):
        for st in memorywrite.stores:
            if st in self.stores:
                self.stores.remove(st)

    def getBaseName(self):
        return "memwrite_{}".format(self.index)

    def getName(self):
        return "memwrite_{}".format(self.index)

    def getLoc(self):
        assert(len(self.stores) > 0)
        res = "(concat"
        for store in self.stores:
            res += " (select {} {})".format(self.mem, store.to_smtlib())

        return res + ")" 

    def getSize(self):
        return self.basesize * len(self.stores)

class Solver():
    def __init__(self, formula, bits):
        self.regs = regs_32 if bits == 32 else regs_64 
        
        self.smtstring = """
        (set-logic QF_ABV) 
        (set-option :produce-models true)
        {} 
        """.format(formula)

        parser = SmtLibParser()
        self.smt = parser.get_script(cStringIO(formula))

        self.memory = [ s for s in self.smt.get_declared_symbols() if s.symbol_name() == "__memory_0" ][0]


    def _is_concat_then_select(self, cmd):
        if not cmd.is_bv_concat(): return False

        def rec(cmd, rest):
            if cmd.is_select() and self.memory == cmd.args()[0]:
                return True
            elif cmd.is_bv_concat():
                assert(len(cmd.args()) == 2)
                c = cmd.args()[0]
                r = cmd.args()[1]
                return rec(c, r)
            else:
                return False

        assert(len(cmd.args()) == 2)
        c = cmd.args()[0]
        rest = cmd.args()[1]
        return rec(c, rest)

    def _find_mem_read(self, cmd):
        if cmd.is_symbol() or cmd.is_constant():
            return set()

        elif self.memory in cmd.get_free_variables():
            if cmd.is_select() and self.memory == cmd.args()[0]:
                return {cmd,}
            elif self._is_concat_then_select(cmd):
                return {cmd,}
            else:
                res = set()
                for c in cmd.args():
                    toadd = self._find_mem_read(c)
                    res.update(toadd)
                return res
        else:
            return set()

        assert(False)

    def get_memory_reads(self):
        """
        Return memory inputs
        """
        res = set()
        for defined in self.smt.filter_by_command_name("define-fun"):
            cmd = defined.args[-1]
            if self.memory in cmd.get_free_variables():
                res.update(self._find_mem_read(cmd))
        return [ MemoryLocation(c, c.bv_width()) for c in res ]

    def get_register_reads(self):
        """
        Return register inputs
        """
        res = set()
        for declared in self.smt.get_declared_symbols():
            basic_name = ''.join(filter(str.isalpha, declared.symbol_name()))

            if basic_name in self.regs:
                res.add(RegisterLocation(declared, declared.bv_width()))

        return list(res)

    def get_register_writes(self):
        res = set()
        already = set()
        for defined in list(self.smt.filter_by_command_name("define-fun"))[::-1]: # reverse order to find the last modification of the register
            name = defined.args[0]
            basic_name = ''.join(filter(str.isalpha, name))
            if basic_name in self.regs and basic_name not in already:
                size = defined.args[2].width
                res.add(RegisterWrite(name, size))
                already.add(basic_name)
            
        return list(res)

    def _get_stores(self, memory):
        store_indexes = []
        mem = memory.args[-1]
        while mem.is_store():
            store_indexes.append(mem.args()[1])
            mem = mem.args()[0]
        
        return store_indexes

    def get_memory_writes(self):
        res = []
        storesize = self.memory.get_type().args[1].width
        memory = None
        for defined in list(self.smt.filter_by_command_name("define-fun"))[::-1]:
            name = defined.args[0]
            defined_type = defined.args[2]
            if "memory" in name and defined_type.is_array_type():
                if memory == None:
                    memory = name # this is the last version of memory

                stores = MemoryWrite(memory, self._get_stores(defined), storesize)
                for i in range(len(res)):
                    res[i].remove(stores) 

                res.append(stores)
        return res

    def getInputsRandom(self, index, inputs):
        insample = {}
        for i, inp in enumerate(inputs):
            size = inp.getSize()

            if index == 0:
                val = 0
            elif index == 1:
                val = 1
            elif index == 2:
                val = (2**size)-1
            elif index == 3:
                val = (2**(size-1))-1
            elif index == 4:
                val = 2**(size-1)
            else:
                val = randint(0, (2**size)-1)

            insample[str(i)] = {
                "location": inp.getLoc(),
                "name": inp.getName(),
                "size": hex(size//8),
                "value": hex(val)
            }
            
        return insample
    

def format_sample(inputs_sample):
    res = ""
    for inp in inputs_sample.values():
        res += "(assert (= {} (_ bv{} {})))\n".format(inp["name"], int(inp["value"], 16), int(inp["size"], 16)*8)

    return res
        
def format_outputs(outputs):
    res = ""
    for output in outputs:
        if isinstance(output, MemoryWrite):
            res += """\
(declare-fun {name} () (_ BitVec {size}))
(assert (= {name} {formula}))
""".    format(name=output.getName(), size=output.getSize(), formula=output.getLoc())

        elif isinstance(output, RegisterWrite):
            res += """\
(declare-fun {name} () (_ BitVec {size}))
(assert (= {name} {formula}))
""".format(name=output.getBaseName(), size=output.getSize(), formula=output.getName())
        else:
            assert(False)

    return res

def extract_smt_formula(target):
    global ARCH

    res = ""
    r = binsec["-isa", ARCH, "-sse", "-sse-script", "./scripts/utils/binsec/extract.ini", target]()

    save = False
    for line in r.split("\n"):
        if "[sse:result] Formula" in line:
            save = True
        elif save == True:
            if "[sse" not in line:
                res += line + "\n"
            else:
                save = False

    return res


def filter_inputs(solv, output, inputs_sample):
    cmds = solv.smt.filter_by_command_name("define-fun")

    outputname = output.getName() if isinstance(output, RegisterWrite) else output.mem 

    variables = [ s for s in cmds if outputname == s.args[0] ][0].args[-1].get_free_variables()
    
    res = {}
    i = 0
    for sample in inputs_sample.values():
        for var in variables:
            if ("mem_" in sample["location"] and var.symbol_name() == "__memory_0") or (sample["location"] == var.symbol_name()):
                res[str(i)] = sample
                i += 1
                break

    if res == {}:
        res["0"] = inputs_sample["0"] 

    return res


def random_sample(solv, index_sample, inputs, outputs):
    inputs_sample = solv.getInputsRandom(index_sample, inputs)

    full_formula = solv.smtstring + "\n" + format_sample(inputs_sample) + format_outputs(outputs) + "\n(check-sat)\n(get-value ({}))\n(exit)".format(" ".join([ o.getName() for o in outputs]))

    z3solv = z3.Solver()
    z3solv.from_string(full_formula)
    assert(z3solv.check() == z3.sat)
    model = z3solv.model()
    
    res = { output: None for output in outputs }
    for output in outputs:
        z3var = [ v for v in model if v.name() == output.getBaseName() ][0]
        outval = hex(model[z3var].as_long())

        filtered_inputs_sample = filter_inputs(solv, output, inputs_sample)

        res[output] = {
            "inputs": filtered_inputs_sample,
            "outputs": {
                "0": {
                    "location": output.getLoc(),
                    "name": output.getBaseName(),
                    "size": hex(output.getSize() // 8),
                    "value": outval
                }
            },
            "symbolic": {
                "ctx": solv.smtstring
            }
        }

    return res

        
def main(binary, nsamples, reg_out, outdir, bits=32):
    formula = extract_smt_formula(binary)
    solv = Solver(formula, bits)
    
    inputs = solv.get_register_reads() + solv.get_memory_reads()
    outputs = solv.get_register_writes() + solv.get_memory_writes()
    if reg_out != None:
        outputs = [ o for o in outputs if reg_out in o.getName() ]

    res = { 
        output: {
        "initial": {},
        "sampling": {},
        "symbolic": {}
        }
        for output in outputs
    }

    for i in range(nsamples):
        # Sample all outputs in one call to solver
        samples = random_sample(solv, i, inputs, outputs)

        for output, sample in samples.items():
            if i == 0: res[output]["initial"] = { "inputs": sample["inputs"], "outputs": sample["outputs"] }
            res[output]["sampling"][str(i)] = { "inputs": sample["inputs"], "outputs": sample["outputs"] }
            res[output]["symbolic"]["smtlib"] = output.getLoc()
            res[output]["symbolic"]["ctx"] = sample["symbolic"]["ctx"]

    for out_index, output in enumerate(outputs):
        with open("{}/out_{}.json".format(outdir, out_index), "w") as f:
            json.dump(res[output], f, indent=4, sort_keys=True)


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('--bin', required=True, type=str, help="binary to load")
    parser.add_argument('--arch', required=False, type=str, help="architecture: x86, x64 (default: x86)")
    parser.add_argument('--nsamples', required=False, type=int, help="number of samples (default = 100)")
    parser.add_argument('--reg_out', required=False, type=str, help="output register to sample (if not set, samples all outputs)")
    parser.add_argument('--out', required=True, type=str, help="output directory")
    args = parser.parse_args()

    try:
        os.mkdir("{}".format(args.out))
    except:
        print("{}[ERROR] '{}' directory already exists{}".format(RED, args.out, NC))
        sys.exit(1)

    ARCH = args.arch if args.arch != None else "x86"
    nsamples = args.nsamples if args.nsamples != None else 100

    main(args.bin, nsamples, args.reg_out, args.out)
