"""Day 14 - Ore Refinery."""


class Recipe(object):
    def __init__(self, recipe_buff):
        self.recipe_buff = recipe_buff
        self.goal = 'FUEL'
        self.raw = 'ORE'

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
                raise ValueError("Double setting a recipe for {0}!".format(r_out[0]))
            recipe_buff[r_out[0]] = (r_out[1], r_in)
        # Instantiate the class
        return cls(recipe_buff)

r = Recipe.from_file('014-recipe-basic.txt')
print(r.recipe_buff)

