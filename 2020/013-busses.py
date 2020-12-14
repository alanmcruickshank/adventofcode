"""Advent of Code Day 13

https://adventofcode.com/2020/day/13

Bus times.
Appears easy?
"""

puzzles = [
    {
        "ts": 939,
        "busses": "7,13,x,x,59,x,31,19"
    },
    {
        "ts": 1002461,
        "busses": "29,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,41,x,x,x,x,x,x,x,x,x,521,x,x,x,x,x,x,x,23,x,x,x,x,13,x,x,x,17,x,x,x,x,x,x,x,x,x,x,x,x,x,601,x,x,x,x,x,37,x,x,x,x,x,x,x,x,x,x,x,x,19"
    }
]


for puzzle in puzzles:
    ts = puzzle["ts"]
    print("Puzzle:", ts)
    busses = [int(n) for n in puzzle["busses"].split(',') if n != 'x']
    print(busses)
    first_ts = []
    for bus in busses:
        # Get the timestamp, module bus number.
        # This is the time since the last bus.
        offset = ts % bus
        # The next bus will be the bus number after that.
        next_bus = (ts - offset) + bus
        first_ts.append(next_bus)
    
    print(first_ts)
    # Get the first bus
    first_bus_ts = min(first_ts)
    first_bus_no = busses[first_ts.index(first_bus_ts)]
    first_bus_wait = first_bus_ts - ts
    print(first_bus_no, first_bus_wait, first_bus_no * first_bus_wait)
    # Part 1 answer = 4207

print("# ########### PART 2")

bus_lists = [
    "17,x,13,19",
    "67,7,59,61",
    "67,x,7,59,61",
    "67,7,x,59,61",
    "1789,37,47,1889",
    "7,13,x,x,59,x,31,19",
    "29,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,41,x,x,x,x,x,x,x,x,x,521,x,x,x,x,x,x,x,23,x,x,x,x,13,x,x,x,17,x,x,x,x,x,x,x,x,x,x,x,x,x,601,x,x,x,x,x,37,x,x,x,x,x,x,x,x,x,x,x,x,19",
]

# Source: https://www.programiz.com/python-programming/examples/lcm
# This function computes GCD 
def compute_gcd(x, y):

   while(y):
       x, y = y, x % y
   return x

# This function computes LCM
def compute_lcm(x, y):
   lcm = (x*y)//compute_gcd(x,y)
   return lcm


for blist in bus_lists:
    print("Puzzle:", blist)
    busses = [int(n) for n in blist.split(',') if n != 'x']
    # Part 2, we need the indexes.
    original_list = blist.split(',')
    bus_indexes = [original_list.index(str(b)) for b in busses]
    print(bus_indexes)
    zipped = list(zip(busses, bus_indexes))
    print(zipped)
    base = busses[0]
    
    step = base

    # This one was hard. The importand idea here is to imagine
    # the buses as being in phase with eachother. Once we've
    # locked a point when two buses are in phase, we can predict
    # the point when they will next be in phase. This extends
    # to multiple busses by useing the LCM of their wavelengths.

    # Step by step get each bus line into phase.
    # First step by the base, then by the lowest common multiple
    # of the busses which are already in phase.
    val = 0
    for bus, offset in zipped:
        while True:
            if (val + offset) % bus == 0:
                print("Locked:", bus, offset, val)
                step = compute_lcm(step, bus)
                print("Step:", step)
                break
            val += step
    print("Answer: ", val)
    # Part 2 Answer: 725850285300475
