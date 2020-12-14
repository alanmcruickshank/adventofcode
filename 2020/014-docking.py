"""Advent of Code Day 14

https://adventofcode.com/2020/day/14

BINARY
"""

import re

class Ferry:
    addr_re = re.compile(r'(?<=mem\[)([0-9]*)(?=\])')
    val_re = re.compile(r'(?<= \= )[0-9]*')

    def __init__(self, mask=None):
        # Don't initialise the *whole* memory space, just
        # save the vales we need.
        self.memory = {}
        # set a base mask if not provided
        self.set_mask(mask or 'X' * 32)

    def set_mask(self, mask):
        self._and_mask = int(mask.replace('X', '1'), 2)
        self._or_mask = int(mask.replace('X', '0'), 2)
        return self._and_mask, self._or_mask

    def set_mem(self, addr, val):
        # Make integer
        val = int(val)
        # Mask
        val = (val & self._and_mask) | self._or_mask
        self.memory[addr] = val
        return val

    def set_regex(self, cmd):
        # Extract values
        addr = int(self.addr_re.search(cmd).group())
        val = int(self.val_re.search(cmd).group())
        return addr, val, self.set_mem(addr, val)
    
    def mem_sum(self):
        return sum(self.memory.values())

    def cmd(self, cmd):
        if cmd[:3] == 'mem':
            return self.set_regex(cmd)
        elif cmd[:4] == 'mask':
            return self.set_mask(cmd[7:].strip())
        else:
            raise ValueError("Unexpected Command: {}".format(cmd))


for fname in ['014-p1.txt', '014-p2.txt']:
    print("##########", fname)
    with open(fname) as f:
        commands = f.readlines()

    ferry = Ferry()
    for cmd in commands:
        print(ferry.cmd(cmd))
    print("Memory Sum:", ferry.mem_sum())
    # Part 1: 13476250121721


