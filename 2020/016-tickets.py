"""Advent of Code Day 16

https://adventofcode.com/2020/day/16

TICKETS
"""

from collections import defaultdict


def read_ticket_file(fname):
    with open(fname) as f:
        lines = f.readlines()
    # First extract field ranges.
    fields = {}
    while lines:
        line = lines.pop(0).strip()
        # If it's a blank line, were done with fields.
        if not line:
            break
        field_name, _, conditions = line.partition(":")
        cond1, _, cond2 = conditions.partition(" or ")
        cond1_s, _, cond1_e = cond1.partition("-")
        cond2_s, _, cond2_e = cond2.partition("-")
        fields[field_name.strip()] = [
            (int(cond1_s), int(cond1_e)),
            (int(cond2_s), int(cond2_e))
        ]
    # Extract your ticket
    your_ticket = []
    while lines:
        line = lines.pop(0).strip()
        if line == 'your ticket:':
            continue
        if not line:
            break
        your_ticket = [int(val) for val in line.split(',')]
    # Extract other tickets
    nearby_tickets = []
    while lines:
        line = lines.pop(0).strip()
        if line == 'nearby tickets:':
            continue
        if not line:
            break
        nearby_tickets.append([int(val) for val in line.split(',')])
    return fields, your_ticket, nearby_tickets


def iter_fields(fields):
    for field in fields:
        for pair in fields[field]:
            yield pair

def product(iterable):
    prod = 1
    for elem in iterable:
        prod *= elem
    return prod


ticket_files = ["016-t1.txt", "016-t2.txt"]

for fname in ticket_files:
    print("### FILE:", fname)
    fields, your_ticket, nearby_tickets = read_ticket_file(fname)

    valid_tickets = []
    working_fields = {
        field: set(range(len(your_ticket)))
        for field in fields
    }
    invalid = []
    for tick in nearby_tickets:
        valid_ticket = True
        passing_fields = defaultdict(list)
        for idx, val in enumerate(tick):
            fits = False
            for field in fields:
                for start, stop in fields[field]:
                    if val <= stop and val >= start:
                        # Fits
                        fits = True
                        passing_fields[field].append(idx)
            if not fits:
                # None fit
                invalid.append(val)
                valid_ticket = False
        if valid_ticket:
            valid_tickets.append(valid_tickets)
            for field in passing_fields:
                working_fields[field] &= set(passing_fields[field])
    print("ticket scanning error rate:", sum(invalid))
    print("found", len(valid_tickets), "valid tickets.")

    print("Working Fields:", working_fields)
    certain_fields = {}
    while working_fields:
        # Find a field with a length of 1.
        target_field = None
        for field in working_fields:
            if len(working_fields[field]) == 1:
                target_field = field
                break
        else:
            print(working_fields)
            raise RuntimeError("No fields to work with!!")
        # There is only one so pop it and take the first it.
        certain_fields[target_field] = list(working_fields.pop(field))[0]
        # Remove this value value from all the other values.
        for field in working_fields:
            working_fields[field].discard(certain_fields[target_field])

    print("Certain Fields:", certain_fields)
    departure_field_vals = []
    for field in certain_fields:
        if field.startswith('departure'):
            departure_field_vals.append(your_ticket[certain_fields[field]])
    print("Departure Fields:", departure_field_vals, product(departure_field_vals))
