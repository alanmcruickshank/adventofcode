"""Advent of Code Day 21

https://adventofcode.com/2020/day/21

Allergens?
"""

for fname in ['021-1.txt', '021-2.txt']:
    print(fname)

    # Read file, parsing and filtering as we go.
    potentials = {}
    # Keep track of occurances for counting later.
    item_ingredients = []
    all_ingredients = set()
    with open(fname) as f:
        for row in f.readlines():
            ingredients, _, contains = row.replace(")", "").partition(" (contains ")
            ingredients = ingredients.strip().split(" ")
            item_ingredients.append(ingredients)
            all_ingredients |= set(ingredients)
            contains = contains.strip().replace(' ', '').split(",")
            for allergen in contains:
                if allergen not in potentials:
                    potentials[allergen] = set(ingredients)
                else:
                    # Use set intersections to filter
                    potentials[allergen] &= set(ingredients)
    #print(all_ingredients)
    
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

    # Impossibles are ones that are in one but not the other
    impossibles = all_ingredients - set(definites.values())
    #print(impossibles)
    impossible_count = 0
    for recipe in item_ingredients:
        for item in recipe:
            if item in impossibles:
                impossible_count += 1
    print("Occurances of non-allergens:", impossible_count)
    # Part 1 answer: 2302
    sorted_dangerous_list = sorted(definites.items())
    print("Sorted dangerous list:", ','.join(name for allergen, name in sorted_dangerous_list))
    # Part 2 answer: smfz,vhkj,qzlmr,tvdvzd,lcb,lrqqqsg,dfzqlk,shp
