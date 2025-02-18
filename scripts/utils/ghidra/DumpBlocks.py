# -*- coding: utf-8 -*-
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
from ghidra.program.model.block import BasicBlockModel
from ghidra.util.task import TaskMonitor
import binascii

def save(filename, opcodes):
    if len(opcodes) > 1:
        with open(filename, "wb") as f:
            for opcode in opcodes:
                f.write(opcode)


def isControlFlow(ins):
    flowtype = ins.getFlowType()
    if flowtype.isCall():
        return True
    elif flowtype.isJump():
        return True
    elif flowtype.isTerminal():
        return True
    elif "REP" in ins.getMnemonicString():
        return True
    else:
        return False


def dump(block, outdir):
    # Warning a block may contains calls
    filename = "0x{}.bin".format(block.getFirstStartAddress())

    listing = currentProgram.getListing()
    ins_iter = listing.getInstructions(block, True)

    opcodes = []

    while ins_iter.hasNext():
        ins = ins_iter.next()
        filename = filename if filename != None else "0x{}.bin".format(ins.getAddress())
        if isControlFlow(ins):
            save("{}/{}".format(outdir, filename), opcodes)
            filename = None
            opcodes = []

        else:
            opcodes.append(ins.getBytes())

    save("{}/{}".format(outdir, filename), opcodes)


def main(outdir):
    section_text = getMemoryBlock('.text')
    bbm = BasicBlockModel(currentProgram)
    blocks = bbm.getCodeBlocks(TaskMonitor.DUMMY)
    block = blocks.next()

    while block:
        if section_text.contains(block.getFirstStartAddress()): 
            dump(block, outdir)
        block = blocks.next()



if __name__ == "__main__":
    args = getScriptArgs()
    if len(args) != 1:
        print("[*] Parameters: <output file>")
        exit(-1)
    main(args[0])
