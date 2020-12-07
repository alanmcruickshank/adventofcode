"""Advent of Code Day 5

https://adventofcode.com/2020/day/5

Binary partitioning is just... BINARY!
"""

def convert_seat_position(position):
    # Get rid of any spaces and new lines.
    position = position.strip(' \n')
    # Use the base argument of int to convert to a number.
    row_number = int(position[:7].replace('F', '0').replace('B', '1'), 2)   
    row_position = int(position[-3:].replace('L', '0').replace('R', '1'), 2)
    seat_id = (row_number * 8) + row_position
    return seat_id

# Check the originals match
assert convert_seat_position('BFFFBBFRRR') == 567
assert convert_seat_position('FFFBBBFRRR') == 119
assert convert_seat_position('BBFFBBFRLL') == 820

# Read the seat IDs from the file
with open("005-seatnumbers.txt") as txt_file:
    boarding_passes = txt_file.readlines()

seat_numbers = [convert_seat_position(bpass) for bpass in boarding_passes]

# Get the highest seat number in the file
print(max(seat_numbers))
# ANSWER = 908

# Find any missing items
total_set = set(range(8 * 128))
booked_set = set(seat_numbers)
missing_set = total_set - booked_set
print(missing_set)
# Filtering out any that are at the start or end.
# By inspection, the answer is 619
