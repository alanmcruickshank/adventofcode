"""Day 14 - Ore Refinery."""


class Recipe(object):
    def __init__(self, recipe_buff, raw='ORE'):
        self.recipe_buff = recipe_buff

        self.ingredient_possibilities = {}
        for k in self.recipe_buff:
            recipe_requires = list(self.recipe_buff[k][1].keys())
            for ingredient in recipe_requires:
                if ingredient not in self.ingredient_possibilities:
                    self.ingredient_possibilities[ingredient] = []
                self.ingredient_possibilities[ingredient].append(k)

        self.element_levels = {raw: 1}
        # Starting at ORE, work out levels as we go UP.
        # We want the LONGEST path from each to ORE, rather
        # than necessarily the shortest.
        while True:
            # Find an element in the next iteration
            remaining = (set(self.ingredient_possibilities.keys())
                         - set(self.element_levels.keys()))
            if len(remaining) == 0:
                break
            for k in remaining:
                # Can it be completely made?
                requires = set(self.recipe_buff[k][1].keys())
                if requires <= set(self.element_levels.keys()):
                    max_len = max([self.element_levels[i] for i in requires])
                    self.element_levels[k] = max_len + 1
                    break

    @classmethod
    def from_file(cls, filename):
        # Load file
        with open(filename, 'r') as f:
            buff = f.read()
        # Split up.
        lines = buff.split('\n')
        lines = [elem.split('=>') for elem in lines]
        lines = [(elem[1], elem[0].split(',')) for elem in lines]
        # Reformat
        recipe_buff = {}
        for r_out, r_in in lines:
            # Format outs
            r_out = r_out.strip().split(' ')
            r_out = (r_out[1], int(r_out[0]))
            # Format ins
            r_in = [elem.strip().split(' ') for elem in r_in]
            r_in = {name: int(val) for val, name in r_in}
            if r_out[0] in recipe_buff:
                raise ValueError("Double setting a recipe for {0}!".format(
                    r_out[0]))
            recipe_buff[r_out[0]] = (r_out[1], r_in)
        # Instantiate the class
        return cls(recipe_buff)

    def solve(self, goal='FUEL', raw='ORE', goal_units=1):
        goal_recipe_units = self.recipe_buff[goal][0]
        goal_multiple = goal_units // goal_recipe_units
        if goal_units % goal_recipe_units:
            goal_multiple += 1
        # Multiply up the recipe
        buff = self.recipe_buff[goal][1].copy()
        for k in buff:
            buff[k] *= goal_multiple
        # Print out result
        # # print(buff)

        # Iteratively solve
        while True:
            # Are we done?
            if len(buff) == 1:
                break
            # First, try stoichiometric breakdowns
            for k in buff:
                if k not in self.recipe_buff:
                    # This has no recipe, it's probably ORE, but continue
                    continue
                required_units = buff[k]
                recipe_units = self.recipe_buff[k][0]
                recipe_elements = self.recipe_buff[k][1]
                # Do we have an exact breakdown?
                if required_units % recipe_units == 0:
                    # Yes! Do it
                    # Remove from buffer
                    buff.pop(k)
                    # Add in required elements in quantity
                    multiples = required_units // recipe_units
                    for elem in recipe_elements:
                        # Initialise if not present
                        if elem not in buff:
                            buff[elem] = 0
                        buff[elem] += recipe_elements[elem] * multiples
                    # Start the loop again
                    break
                else:
                    # No, this is messy, try another for now
                    continue
            else:
                # No stoichiometric options available.
                # Pick the highest level one
                # to deal with first.
                # # print(self.recipe_buff)
                # # print(buff)
                # # print(self.element_levels)
                # Identify the highest level thing remaining and
                # break that wastefully.
                scores = [(k, self.element_levels[k]) for k in buff]
                # # print(scores)
                # Get the highest
                highest = scores[0]
                for elem in scores:
                    if elem[1] > highest[1]:
                        highest = elem
                highest = highest[0]
                # # print(highest)
                # Break it
                required_units = buff[highest]
                recipe_units = self.recipe_buff[highest][0]
                recipe_elements = self.recipe_buff[highest][1]
                multiples = (required_units // recipe_units) + 1
                # Do it
                buff.pop(highest)
                for elem in recipe_elements:
                    # Initialise if not present
                    if elem not in buff:
                        buff[elem] = 0
                    buff[elem] += recipe_elements[elem] * multiples
                # Carry on!
                # raise ValueError("No tidy breakdowns found in iteration. :(")

        # End Result
        return buff

    def find_for_less_than(self, goal_unit='ORE', goal_limit=1000000):
        unit_map = {
            1: self.solve(goal_units=1)[goal_unit],
            10: self.solve(goal_units=10)[goal_unit]
        }
        rough_factor = (unit_map[10] - unit_map[1]) / (10 - 1)
        print(rough_factor)
        estimate_point = int(goal_limit // rough_factor)
        unit_map[estimate_point] = self.solve(
            goal_units=estimate_point)[goal_unit]

        if estimate_point < goal_limit:
            up_down = 'up'
        else:
            up_down = 'down'

        if up_down == 'up':
            while True:
                estimate_point += 1
                unit_map[estimate_point] = self.solve(
                    goal_units=estimate_point)[goal_unit]
                if unit_map[estimate_point] > goal_limit:
                    solve_point = estimate_point - 1
                    break
        else:
            while True:
                estimate_point -= 1
                unit_map[estimate_point] = self.solve(
                    goal_units=estimate_point)[goal_unit]
                if unit_map[estimate_point] < goal_limit:
                    solve_point = estimate_point

        print(solve_point)
        print(unit_map[solve_point])


r = Recipe.from_file('014-recipe-3.txt')
# print(r.recipe_buff)
print("Soliving:")
print(r.solve(goal_units=2))
r.find_for_less_than(goal_limit=1000000000000)
