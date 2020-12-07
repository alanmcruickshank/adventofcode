"""Advent of Code Day 2

https://adventofcode.com/2020/day/2
"""

# Read in the file and get the values of all the rows
# Make a list of tuples with text elements.
with open("002-passwords-input.txt") as txt_file:
    raw_elements = [
        row.split(' ') for row in txt_file.read().split('\n')
    ]
print(raw_elements[:5])

# pre-process the rows to get just what we need:
elements = [
    (   
        # Two numbers, a min and max
        tuple(int(n) for n in elem[0].split('-')),
        # A middle character, with colons and spaces stripped.
        elem[1].strip(': '),
        # The actual password.
        elem[2]

    )
    for elem in raw_elements
]
print(elements[:5])

def valid_1(element):
    return element[0][1] >= element[2].count(element[1]) >= element[0][0]

# Valid?
valid = [valid_1(elem) for elem in elements]
print(valid[:5])

# Number valid
print(sum(valid))
# ANSWER = 383

# Part 2

def valid_2(element):
    matches = (element[2][element[0][0] - 1] == element[1]) + (element[2][element[0][1] - 1] == element[1])
    return matches == 1

# Valid?
valid = [valid_2(elem) for elem in elements]
print(valid[:5])

# Number valid
print(sum(valid))
# ANSWER = 272