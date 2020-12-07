"""Advent of Code Day 4

https://adventofcode.com/2020/day/4
"""

def validate(passport):
    # (NB. cid not reqd)
    required_keys = {'byr', 'iyr', 'eyr', 'hgt', 'hcl', 'ecl', 'pid'}
    if not set(passport.keys()) >= required_keys:
        return False
    
    # Part 2 validation:
    # BYR
    if len(passport['byr']) != 4:
        return False
    byr = int(passport['byr'])
    if byr > 2002 or byr < 1920:
        return False
    
    # IYR
    if len(passport['iyr']) != 4:
        return False
    byr = int(passport['iyr'])
    if byr > 2020 or byr < 2010:
        return False
    
    # EYR
    if len(passport['eyr']) != 4:
        return False
    byr = int(passport['eyr'])
    if byr > 2030 or byr < 2020:
        return False
    
    # HGT
    if not (passport['hgt'].endswith('cm') or passport['hgt'].endswith('in')):
        return False
    hgt_unit = passport['hgt'][-2:]
    hgt_val = int(passport['hgt'][:-2])
    if hgt_unit == 'cm':
        if hgt_val < 150 or hgt_val > 193:
            return False
    elif hgt_unit == 'in':
        if hgt_val < 59 or hgt_val > 76:
            return False
    
    # HCL
    if len(passport['hcl']) != 7 or passport['hcl'][0] != '#' or set(passport['hcl'][1:]) > set('0123456789abcdef'):
        return False
    
    # ECL
    if passport['ecl'] not in {'amb', 'blu', 'brn', 'gry', 'grn', 'hzl', 'oth'}:
        return False
    
    # PID
    if len(passport['pid']) != 9 or set(passport['pid']) > set('0123456789'):
        return False
    
    return True


def validate_passport_file(fname):
    with open(fname) as txt_file:
        passport_lines = txt_file.readlines()
    
    # Parse the passport data
    passports = []
    pass_buff = {}
    for line in passport_lines:
        stripped_line = line.strip()
        # Look for blank lines
        if stripped_line:
            # Not blank, parse values
            elems = stripped_line.split(' ')
            for elem in elems:
                key, val = elem.split(":")
                pass_buff[key] = val
        else:
            # Blank, flush buffer
            passports.append(pass_buff)
            pass_buff = {}
    # If we get to the end, flush any remaining values
    if pass_buff:
        passports.append(pass_buff)
    
    # Count the valid ones.
    return len(passports), sum(
        [
            validate(elem)
            for elem in passports
        ]
    )

# Validate the test files:
for file in ["004-passport-test.txt", "004-passport-valid.txt", "004-passport-invalid.txt", "004-passport-input.txt"]:
    print(file, validate_passport_file(file))
# ANSWER #1 = 226
# ANSWER #2 = 160
