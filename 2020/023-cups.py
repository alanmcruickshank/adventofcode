"""Advent of Code Day 23

https://adventofcode.com/2020/day/23

Cups
"""

import cProfile


class RingList:
    def __init__(self, iterable):
        self._vals = list(iterable)
    
    def __len__(self):
        return len(self._vals)
    
    def __getitem__(self, key):
        ring_len = len(self)
        if isinstance(key, slice):
            idx = key.start or 0
            vals = []
            while idx < key.stop:
                vals.append(self[idx % ring_len])
                idx += key.step or 1
            return vals
        else:
            v = self._vals[key % ring_len]
            return v
    
    def __repr__(self):
        return "<RingList {0!r}>".format(self._vals)
    
    def __contains__(self, val):
        return val in self._vals
    
    def __iter__(self):
        return iter(self._vals)
    
    def index(self, val):
        return self._vals.index(val)
    
    def pop(self, idx):
        ring_len = len(self)
        return self._vals.pop(idx % ring_len)
    
    def pop_many(self, idx, n=1):
        vals = []
        ring_len = len(self)
        # Shortcut, in some circumstances.
        if idx + n < ring_len:
            vals = self._vals[idx: idx + n]
            self._vals = self._vals[:idx] + self._vals[idx + n:]
            return vals

        ring_len = len(self)
        pop_point = idx % ring_len
        for _ in range(n):
            vals.append(self.pop(pop_point))
            if pop_point >= len(self):
                pop_point = 0
        return vals
    
    def insert(self, idx, val):
        ring_len = len(self)
        self._vals.insert(idx % ring_len, val)
    
    def insert_many(self, idx, vals):
        ring_len = len(self)
        idx %= ring_len
        self._vals = self._vals[:idx] + list(vals) + self._vals[idx:]
    
    def to_list(self, idx):
        ring_len = len(self)
        return [self[i] for i in range(idx, idx + ring_len)]


class CrabRing(RingList):
    def __init__(self, iterable):
        super().__init__(iterable)
        self._len = len(self._vals)
        self._min = min(self._vals)
        self._max = max(self._vals)
    
    def __len__(self):
        return self._len
    
    def crab_move(self, current_cup):
        # print("Pre Move:", self._vals, current_cup)
        cup_idx = self._vals.index(current_cup)
        # print(cup_idx)
        picked_end_idx = cup_idx + 1
        picked_start_idx = max(cup_idx + 4 - self._len, 0)
        picked_end_slice = slice(picked_end_idx, min(picked_end_idx + 3, self._len))
        picked_start_slice = slice(0, picked_start_idx)

        picked_cups = self._vals[picked_end_slice] + self._vals[picked_start_slice]
        # print(self._vals[picked_end_slice], self._vals[picked_start_slice])

        dest_cup = current_cup - 1
        if dest_cup in picked_cups:
            dest_cup = min(picked_cups) - 1
        if dest_cup < self._min:
            dest_cup = self._max
        while dest_cup in picked_cups:
            dest_cup -= 1
        # print(current_cup, dest_cup)

        dest_cup_idx = self._vals.index(dest_cup)
        # print(dest_cup_idx, cup_idx)

        if dest_cup_idx > cup_idx:
            # print("A", self._vals[picked_start_idx:cup_idx + 1], self._vals[cup_idx + 4: dest_cup_idx + 1], picked_cups, self._vals[dest_cup_idx + 1:])
            self._vals = self._vals[picked_start_idx:cup_idx + 1] + self._vals[cup_idx + 4: dest_cup_idx + 1] + picked_cups + self._vals[dest_cup_idx + 1:]
        else:
            # print("B", self._vals[picked_start_idx:dest_cup_idx + 1], picked_cups, self._vals[dest_cup_idx + 1: cup_idx + 1], self._vals[cup_idx + 4:])
            self._vals = self._vals[picked_start_idx:dest_cup_idx + 1] + picked_cups + self._vals[dest_cup_idx + 1: cup_idx + 1] + self._vals[cup_idx + 4:]
        # print(self._vals, dest_cup_idx)
        return self[self.index(current_cup) + 1]

    
def crab_move(ring, current_cup):
    # print("Pre Move:", ring[:10], current_cup)
    cup_idx = ring.index(current_cup)
    min_cup = min(cups)
    max_cup = max(cups)
    picked_cups = ring.pop_many(cup_idx + 1, n=3)
    dest_cup = current_cup - 1
    while dest_cup not in ring:
        dest_cup -= 1
        if dest_cup < min_cup:
            dest_cup = max_cup
    # print(picked_cups, dest_cup)
    dest_cup_idx = ring.index(dest_cup)
    ring.insert_many(dest_cup_idx + 1, picked_cups)
    return ring, ring[ring.index(current_cup) + 1]


inputs = [
    '389125467',
    '562893147'
]

for puzz in inputs:
    print("Starting:", puzz)
    cups = RingList(int(elem) for elem in puzz)  # CrabList
    current_cup = cups[0]
    for i in range(100):
        # print("Move", i + 1, cups._vals)
        # current_cup = cups.crab_move(current_cup)
        cups, current_cup = crab_move(cups, current_cup)
        
    print(''.join(str(elem) for elem in cups.to_list(cups.index(1) + 1)))
    # Part 1 answer: 38925764
    
    # Part 2. Try just iterating.
    #pre_puzz = [int(elem) for elem in puzz]
    #cups = RingList(pre_puzz + list(range(max(pre_puzz) + 1, 1000000 + 1)))
    #current_cup = cups[0]
    #with cProfile.Profile() as pr:
    #    for i in range(100):
    #        cups, current_cup = crab_move(cups, current_cup)
    #pr.print_stats()
    #cup1_idx = cups.index(1)
    #v1, v2 = cups[cup1_idx + 1], cups[cup1_idx + 2]
    #print(v1, v1, v1 * v2)
