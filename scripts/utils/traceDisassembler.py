#!/usr/bin/env python3
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

import argparse
import gdb
import re
import os
import sys

if (sys.version_info > (3, 0)):
    from pathlib import Path

def toint(string):
    if (sys.version_info > (3, 0)):
        return int(string)
    else:
        return long(string)

def tobytes(integer):
    assert integer < 256 and integer >= 0, "Not a byte: {}".format(integer)
    if (sys.version_info > (3, 0)):
        hexstring = "%02x"%(integer)
        return bytes.fromhex(hexstring)
    else:
        assert False, "TODO"


def instn_length(addr_expr):
    t = gdb.execute('x/2i ' + addr_expr, to_string=True)
    return toint(gdb.parse_and_eval('$_')) - toint(gdb.parse_and_eval(addr_expr))

def isJumpInstr(addr_expr):
    t = gdb.execute('x/i ' + addr_expr, to_string=True)
    t = t.split(":")[1].strip()
    if "j" in t:
        return True
    else:
        return False
    

def parse_opcodes(string):
    """
    Parse bytes values. Format:
    addr1: byte1 byte2 ...
    addr2: byteX ...
    """

    hexvalues = [] 
    lines = [ line.strip() for line in string.split("\n") if line.strip() != "" ]

    for line in lines:
        for val in line.split(":")[1].strip().split():
            assert val.startswith("0x"), "WTF {}".format(val)
            hexvalues.append(int(val, 16))
            assert hexvalues[-1] < 256 and hexvalues[-1] >= 0, "not a byte in disassembly"
    return hexvalues



class TraceDisassembler(gdb.Command):
    def __init__(self, saveDir, args):
        super(TraceDisassembler, self).__init__("trace_disassembler", gdb.COMMAND_USER)
        self.dir = saveDir

        # Check if the main symbol exists
        info = gdb.execute("p main", True, True)
        assert "0x" in info, "No symbol for the main function"

        info = gdb.execute("b *main", True, True) # break at the real start of main (!= b main) 
        baddr = [ int(i, 16) for i in re.findall(r"0x[a-f0-9]+", info) ]
        assert len(baddr) == 1
        baddr = baddr[0]

        gdb.execute("r {}".format(args), True, True) # run with arguments
        info = gdb.execute("p $pc", True, True) # run with arguments
        gdb_baddr = [ int(i, 16) for i in re.findall(r"0x[a-f0-9]+", info) ]
        assert len(gdb_baddr) == 1
        gdb_baddr = gdb_baddr[0]

        self.base_addr = gdb_baddr - baddr

        self.binname = self._getbinname()
        self.end = self._get_end()
        self.lower, self.upper = self._getbinarymapping()

    def _getbinname(self):
        exe = gdb.execute("info proc exe", True, True).split("\n")[1].split("=")[1].strip()
        exe = exe[1:-1] # 'lala' -> lala
        return exe

    def _getbinarymapping(self):
        """
        Returns address intervals where is loaded the binary (other addresses are thus libraries that we don't monitor
        """
        lowers, uppers = [], []
        mappings = gdb.execute("info proc mappings", True, True)
        lines = mappings.split("\n")
        for line in lines:
            if self.binname in line:
                fields = [ field.strip() for field in line.split() if field.strip() != "" ]
                lowers.append(int(fields[0], 16))
                uppers.append(int(fields[1], 16))
        interval = (min(lowers), max(uppers))
        return interval

    def _get_end(self):
        """
        Returns first and last address of main function
        """
        
        # Check if main function symbol
        info = gdb.execute("p main", True, True)
        assert "0x" in info, "No symbol for the main function"

        rets = [ line.strip() for line in gdb.execute("disas main", True, True).split("\n") if "0x" in line and "ret" in line ]
        assert len(rets) == 1, "Multiple returns ?!"
        end = int(rets[0].split()[0], 16)
        print("end = ", end)
        return end

    def invoke(self, args, from_tty):
        blockopcodes = []
        nextrip = None
        blockaddr = None
        addr = None
        old_addr = None
        instrsize = None
        ninstr = 0

        while addr == None or addr != self.end:
            old_addr = addr
            addr = toint(gdb.selected_frame().read_register('pc'))

            if blockaddr == None:
                blockaddr = addr

            if (nextrip != None and nextrip != addr) or (isJumpInstr(str(old_addr)) if old_addr != None else False):
                assert(instrsize != None)
                # Save block
                if not self.filter_blocks(blockaddr, blockopcodes[:-instrsize], ninstr):
                    self._saveOpcodes(blockaddr, blockopcodes[:-instrsize])

                blockaddr = addr
                blockopcodes = []
                ninstr = 0

            instrsize = instn_length(str(addr))
            nextrip = addr + instrsize

            opcodes = parse_opcodes(gdb.execute("x/{}b {}".format(instrsize, addr), True, True))

            blockopcodes += opcodes
            ninstr += 1

            _ = gdb.execute("stepi", True, True)

        assert(instrsize != None)
        self._saveOpcodes(blockaddr, blockopcodes[:-instrsize]) # save last opcodes 

    def filter_blocks(self, blockaddr, opcodes, ninstr):
        """
        Filter blocks we don't want ot synthesize (e.g. because there are to little)"
        """
        if ninstr <= 2: # 2 because the last one is removed by default
            return True

        elif len(opcodes) <= 1:
            return True

        elif blockaddr < self.lower or blockaddr > self.upper:
            return True

        else:
            return False

    def _saveOpcodes(self, blockaddr, opcodes):
        blockfile = "{}/{}.bin".format(self.dir, hex(blockaddr - self.base_addr))
        rawops = b"".join([ tobytes(op) for op in opcodes ])

        with open(blockfile, "wb") as f:
            f.write(rawops)

TraceDisassembler(outdir, args) # outdir, start and end are defined in ./all_from_trace.sh when calling gdb
