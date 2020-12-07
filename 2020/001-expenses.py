"""Advent of Code Day 1

https://adventofcode.com/2020/day/1
"""

# Read in the file and get the values of all the expenses
with open("001-expenses-input.txt") as infile:
    expenses = [
        int(expense)
        for expense in infile.read().split('\n')
    ]

# Use a set to do efficient hash lookups
expenses = set(expenses)

# Work through each one, looking for 2020 - x.
# If we find it, then break - it's the answer.
for expense in expenses:
    if 2020 - expense in expenses:
        found_expenses = (expense, 2020 - expense)
        break
else:
    found_expenses = (None, None)
    raise ValueError("Never found a matching expense!")

# Output the answer
print("Found expenses:", found_expenses)
print("Multiple:", found_expenses[0] * found_expenses[1])
# ANSWER = 605364

# That shortcut doesn't work for part 2.
# User itertools 🤷‍♀️

import itertools

for selection in itertools.combinations(expenses, 3):
    if sum(selection) == 2020:
        found_expenses = selection
        break
else:
    found_expenses = (None, None, None)
    raise ValueError("Never found a matching expense!")

# Output the answer
print("Found expenses:", found_expenses)
print("Multiple:", found_expenses[0] * found_expenses[1] * found_expenses[2])
# ANSWER = 128397680
