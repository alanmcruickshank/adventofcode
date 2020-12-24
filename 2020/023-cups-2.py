"""Advent of Code Day 23

https://adventofcode.com/2020/day/23

Cups. Circular Linked Lists.
"""


class DoubleLinkedElement:
    __slots__ = ["val", "nxt", "prv"]
    
    def __init__(self, val, nxt=None, prv=None):
        self.val = val
        self.nxt = nxt
        self.prv = prv
    
    def __repr__(self):
        return "<DLE: [{1}<-] {0} [->{2}]>".format(
            self.val,
            self.prv.val if self.prv else "-",
            self.nxt.val if self.nxt else "-",
        )


class CircularIndexedLinkedList:
    def __init__(self, iterable):
        self._vals = {}
        self.head = None
        prv = None
        # Set up elements
        for val in iterable:
            elem = DoubleLinkedElement(val, prv=prv)
            self._vals[val] = elem
            if not self.head:
                self.head = elem
            prv = elem
        # When we get to the end, link forward to the head
        # and then backward to the start.
        elem.nxt = self.head
        self.head.prv = elem
        while elem is not self.head:
            elem.prv.nxt = elem
            elem = elem.prv
        # Store the max value for later
        self._max = max(self._vals.keys())
    
    def readout(self, head=None):
        if head is None:
            head = self.head
        else:
            head = self._vals[head]
        yield head.val
        pos = head.nxt
        while pos != head:
            yield pos.val
            pos = pos.nxt
    
    def str_readout(self, head=None):
        return "".join(str(elem) for elem in self.readout(head=head))
    
    def crab_move(self, current_cup):
        # Current Cup
        c_cup = self._vals[current_cup]
        # Picked Cups (we only need references to the first and last)
        first_picked = c_cup.nxt
        last_picked = first_picked.nxt.nxt
        post_picked = last_picked.nxt
        picked_vals = set([first_picked.val, first_picked.nxt.val, last_picked.val])
        # Work out the destination cup (and it's follower)
        dest_cup = current_cup - 1
        while dest_cup not in self._vals or dest_cup in picked_vals:
            dest_cup -= 1
            if dest_cup < 1:
                dest_cup = self._max
        d_cup = self._vals[dest_cup]
        post_d_cup = d_cup.nxt
        # SPLICE!
        ###print(c_cup, first_picked, last_picked, post_picked, d_cup, post_d_cup, picked_vals)
        # 1. join up the gap we made
        c_cup.nxt = post_picked
        post_picked.prv = c_cup
        # 2. Insert the picked cups after the destination cup.
        d_cup.nxt = first_picked
        first_picked.prv = d_cup
        last_picked.nxt = post_d_cup
        post_d_cup.prv = last_picked
        # Return the new current cup.
        return post_picked.val


inputs = [
    '389125467',
    '562893147'
]

for puzz in inputs:
    print("Starting:", puzz)
    cups = CircularIndexedLinkedList(int(elem) for elem in puzz)  # CrabList
    current_cup = cups.head.val

    print(". Starting", cups.str_readout(head=1), current_cup)
    for i in range(100):
        current_cup = cups.crab_move(current_cup)
    print(". Final", cups.str_readout(head=1)[1:])
    # Part 1 answer: 38925764

    #    
    #print(''.join(str(elem) for elem in cups.to_list(cups.index(1) + 1)))
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
