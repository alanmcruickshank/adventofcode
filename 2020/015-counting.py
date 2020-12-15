"""Advent of Code Day 15

https://adventofcode.com/2020/day/15

COUNTING AND HASHMAPS
"""

from collections import defaultdict

initials = [
    [0, 3, 6],
    [1, 3, 2],
    [2, 1, 3],
    [1, 2, 3],
    [2, 3, 1],
    [3, 2, 1],
    [3, 1, 2],
    [5, 1, 9, 18, 13, 8, 0]
]

limit = 2020

for initial in initials:
    print("Initial: ", initial)
    # What number are we on?
    n = 1
    # What did we say last?
    last_spoken = 0
    # When have we said each number before?
    last_speak_map = defaultdict(list)
    while n <= limit:
        # If it's an initial number, speak it.
        if n <= len(initial):
            speak = initial[n - 1]
        else:
            # Work out what to speak based on the last number.
            # Have we said it twice before?
            spoken_history = last_speak_map[last_spoken]
            if len(spoken_history) >= 2:
                speak = spoken_history[-1] - spoken_history[-2]
            else:
                speak = 0
        # Keep track of what we speak
        last_spoken = speak
        last_speak_map[last_spoken].append(n)
        n += 1
    print("Spoke", last_spoken, "at turn", n - 1)
    # Part 1 answer: 376
    
