"""Advent of Code Day 21

https://adventofcode.com/2020/day/21

Allergens?
"""

for fname in ['021-1.txt']:  # 021-2.txt
    print(fname)

    # Read file, parsing and filtering as we go.
    potentials = {}
    with open(fname) as f:
        for row in f.readlines():
            ingredients, _, contains = row.replace(")", "").partition(" (contains ")
            ingredients = ingredients.strip().split(" ")
            contains = contains.strip().replace(' ', '').split(",")
            for allergen in contains:
                if allergen not in potentials:
                    potentials[allergen] = set(ingredients)
                else:
                    # Use set intersections to filter
                    potentials[allergen] &= set(ingredients)
    
    # Iteratively filter out the definites.
    definites = {}
    while potentials:
        for allergen in list(potentials.keys()):
            # Remove any existing definites.
            potentials[allergen] -= set(definites.values())
            # Store any definites
            if len(potentials[allergen]) == 1:
                definites[allergen] = potentials[allergen].pop()
                potentials.pop(allergen)
    print(definites)
