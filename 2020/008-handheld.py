"""Advent of Code Day 8

https://adventofcode.com/2020/day/8
"""

from statemachine import InstructionMachine

with open("008-handheld.txt") as txt_file:
    test_instructions = txt_file.read()

test_codes = [
    # Dummy example
    "nop +0\nacc +1",
    # Given Example
    "nop +0\nacc +1\njmp +4\nacc +3\njmp -3\nacc -99\nacc +1\njmp -4\nacc +6",
    # Puzzle example
    test_instructions
]

# Part 1

for test_code in test_codes:
    sm = InstructionMachine(test_code)
    print("### BEGIN EXECUTION")
    print(sm.run())
# ANSWER = 1384

# Part 2

# Search for ONE change.
processed_instructions = InstructionMachine.process_raw_instructions(test_instructions)
# Find all the points to change.
edit_points = []
for idx, val in enumerate(processed_instructions):
    if val[0] in ('jmp', 'nop'):
        edit_points.append((idx, val[0]))

# Try each of them:
for idx, current_inst in edit_points:
    new_instructions = processed_instructions.copy()
    new_inst = 'jmp' if current_inst == 'nop' else 'nop'
    # It's a tuple so we have to make a new one.
    new_instructions[idx] = (new_inst, new_instructions[idx][1])
    sm = InstructionMachine(new_instructions)
    exit_code, acc, _ = sm.run()
    if exit_code:
        print("SUCEESS:", idx, current_inst, exit_code, acc)
        break
# ANSWER = 
