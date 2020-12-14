"""Advent of Code Day 14

https://adventofcode.com/2020/day/14

BINARY
"""

import re

class Ferry:
    addr_re = re.compile(r'(?<=mem\[)([0-9]*)(?=\])')
    val_re = re.compile(r'(?<= \= )[0-9]*')

    def __init__(self, mask=None, protocol=1):
        # Don't initialise the *whole* memory space, just
        # save the vales we need.
        self.memory = {}
        # set a base mask if not provided
        self.set_mask(mask or 'X' * 36)
        # Set protocol
        self.protocol = protocol

    def set_mask(self, mask):
        self._and_mask = int(mask.replace('X', '1'), 2)
        self._or_mask = int(mask.replace('X', '0'), 2)
        self._float_bits = [idx for idx, val in enumerate(reversed(mask)) if val == 'X']
        self._float_mask = int(mask.replace('1', '0').replace('X', '1'), 2)
        return self._and_mask, self._or_mask

    @staticmethod
    def and_or_mask(val, or_mask, and_mask):
        return (val & and_mask) | or_mask
    
    def iter_masked_addresses(self, addr):
        if self._float_bits:
            for val in range(2 ** (len(self._float_bits))):
                bit_idx = 0
                transformed_val = 0
                val_buff = val
                while (val_buff >> bit_idx):
                    lsb = (val_buff >> bit_idx) & 1
                    transformed_val += lsb * (2 ** self._float_bits[bit_idx])
                    bit_idx += 1
                yield self.and_or_mask(addr | self._and_mask, transformed_val, transformed_val | (~self._float_mask))
        else:
            yield addr | self._and_mask

    def set_mem(self, addr, val):
        # Make integer
        val = int(val)
        # Mask
        if protocol == 1:
            val = self.and_or_mask(val, self._or_mask, self._and_mask)
            self.memory[addr] = val
        elif protocol == 2:
            for masked_addr in self.iter_masked_addresses(addr):
                self.memory[masked_addr] = val
        else:
            raise ValueError("Unexpected Protocol {}".format(protocol))
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


 # NB: the original example (p1) doesn't complete in a sensible time
for fname in ['014-p3.txt', '014-p2.txt']:
    for protocol in [1, 2]:
        print("##########", fname, "Protocol", protocol)
        with open(fname) as f:
            commands = f.readlines()

        ferry = Ferry(protocol=protocol)
        for cmd in commands:
            ferry.cmd(cmd)
        print("Memory Sum:", ferry.mem_sum())
        # Part 1: 13476250121721
        # Part 2: 4463708436768
