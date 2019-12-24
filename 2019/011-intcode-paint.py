"""Day 11 - Intcode Painting Robot."""


class IntComp(object):

    inbuilt_programs = {
        'test_a': [
            109, 1, 204, -1, 1001, 100, 1, 100, 1008, 100,
            16, 101, 1006, 101, 0, 99
        ],
        'asteroid': [
            3, 225, 1, 225, 6, 6, 1100, 1, 238, 225, 104, 0, 1101, 48, 82,
            225, 102, 59, 84, 224, 1001, 224, -944, 224, 4, 224, 102, 8,
            223, 223, 101, 6, 224, 224, 1, 223, 224, 223, 1101, 92, 58, 224,
            101, -150, 224, 224, 4, 224, 102, 8, 223, 223, 1001, 224, 3, 224,
            1, 224, 223, 223, 1102, 10, 89, 224, 101, -890, 224, 224, 4, 224,
            1002, 223, 8, 223, 1001, 224, 5, 224, 1, 224, 223, 223, 1101, 29,
            16, 225, 101, 23, 110, 224, 1001, 224, -95, 224, 4, 224, 102, 8,
            223, 223, 1001, 224, 3, 224, 1, 223, 224, 223, 1102, 75, 72, 225,
            1102, 51, 8, 225, 1102, 26, 16, 225, 1102, 8, 49, 225, 1001, 122,
            64, 224, 1001, 224, -113, 224, 4, 224, 102, 8, 223, 223, 1001, 224,
            3, 224, 1, 224, 223, 223, 1102, 55, 72, 225, 1002, 174, 28, 224,
            101, -896, 224, 224, 4, 224, 1002, 223, 8, 223, 101, 4, 224, 224,
            1, 224, 223, 223, 1102, 57, 32, 225, 2, 113, 117, 224, 101, -1326,
            224, 224, 4, 224, 102, 8, 223, 223, 101, 5, 224, 224, 1,
            223, 224, 223, 1, 148, 13, 224, 101, -120, 224, 224, 4, 224, 1002,
            223, 8, 223, 101, 7, 224, 224, 1, 223, 224, 223, 4, 223, 99, 0, 0,
            0, 677, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1105, 0, 99999, 1105, 227,
            247, 1105, 1, 99999, 1005, 227, 99999, 1005, 0, 256, 1105, 1,
            99999, 1106, 227, 99999, 1106, 0, 265, 1105, 1, 99999, 1006, 0,
            99999, 1006, 227, 274, 1105, 1, 99999, 1105, 1, 280, 1105, 1,
            99999, 1, 225, 225, 225, 1101, 294, 0, 0, 105, 1, 0, 1105, 1,
            99999, 1106, 0, 300, 1105, 1, 99999, 1, 225, 225, 225, 1101,
            314, 0, 0, 106, 0, 0, 1105, 1, 99999, 8, 677, 226, 224, 102, 2,
            223, 223, 1006, 224, 329, 101, 1, 223, 223, 107, 677, 677, 224,
            1002, 223, 2, 223, 1006, 224, 344, 101, 1, 223, 223, 8, 226, 677,
            224, 102, 2, 223, 223, 1006, 224, 359, 101, 1, 223, 223, 107, 226,
            226, 224, 102, 2, 223, 223, 1005, 224, 374, 1001, 223, 1, 223,
            1108, 677, 226, 224, 1002, 223, 2, 223, 1006, 224, 389, 101, 1,
            223, 223, 107, 677, 226, 224, 102, 2, 223, 223, 1006, 224, 404,
            1001, 223, 1, 223, 1107, 226, 677, 224, 1002, 223, 2, 223, 1006,
            224, 419, 1001, 223, 1, 223, 108, 677, 677, 224, 102, 2, 223, 223,
            1005, 224, 434, 1001, 223, 1, 223, 1008, 677, 226, 224, 1002, 223,
            2, 223, 1006, 224, 449, 1001, 223, 1, 223, 7, 226, 677, 224, 1002,
            223, 2, 223, 1006, 224, 464, 1001, 223, 1, 223, 1007, 677, 677,
            224, 102, 2, 223, 223, 1005, 224, 479, 1001, 223, 1, 223, 1007,
            226, 226, 224, 1002, 223, 2, 223, 1005, 224, 494, 1001, 223, 1,
            223, 108, 226, 226, 224, 1002, 223, 2, 223, 1005, 224, 509, 1001,
            223, 1, 223, 1007, 226, 677, 224, 1002, 223, 2, 223, 1006, 224,
            524, 101, 1, 223, 223, 1107, 677, 677, 224, 102, 2, 223, 223,
            1005, 224, 539, 101, 1, 223, 223, 1107, 677, 226, 224, 102, 2,
            223, 223, 1005, 224, 554, 1001, 223, 1, 223, 108, 677, 226,
            224, 1002, 223, 2, 223, 1006, 224, 569, 1001, 223, 1, 223, 1108,
            226, 677, 224, 1002, 223, 2, 223, 1006, 224,
            584, 101, 1, 223, 223, 8, 677, 677, 224, 1002, 223, 2, 223, 1006,
            224, 599, 1001, 223, 1, 223, 1008, 226, 226, 224, 102, 2, 223,
            223, 1006, 224, 614, 101, 1, 223, 223, 7, 677, 677, 224, 1002,
            223, 2, 223, 1006, 224, 629, 101, 1, 223, 223, 1008, 677, 677,
            224, 102, 2, 223, 223, 1005, 224, 644, 101, 1, 223, 223, 7, 677,
            226, 224, 1002, 223, 2, 223, 1005, 224, 659, 101, 1, 223, 223,
            1108, 226, 226, 224, 102, 2, 223, 223, 1006, 224, 674, 1001,
            223, 1, 223, 4, 223, 99, 226
        ]
    }

    def __init__(self, code):
        # If code is a string, it might be a reference to an inbuilt program
        if isinstance(code, str):
            if code in self.inbuilt_programs:
                code = self.inbuilt_programs[code]
            else:
                raise KeyError(
                    "Provided unknown program reference: {0!r}".format(
                        code
                    ))
        # Make the memory a dictionary, so we can assign arbitrary values
        self.memory = {idx: val for idx, val in enumerate(code)}
        self.halt = False
        self.prog_pointer = 0
        self.await_input = False
        self.rel_base = 0

    def get_from_memory(self, offset, mode):
        val = self.memory[self.prog_pointer + offset]
        if mode == 0:
            # Default to zero
            return self.memory.get(val, 0)
        elif mode == 1:
            return val
        elif mode == 2:
            # Use relative base (default to zero)
            return self.memory.get(self.rel_base + val, 0)
        else:
            raise ValueError("Unexpected Parameter Mode: {0}".format(mode))

    def take_step(self, idx=0, input_buffer=None):
        op_code = self.memory[self.prog_pointer]
        instruction = op_code % 100
        p1_mode = (op_code % 1000) // 100
        p2_mode = (op_code % 10000) // 1000
        p3_mode = (op_code % 100000) // 10000
        output = None
        # print("...", self.prog_pointer, op_code, instruction, p1_mode,
        #       p2_mode, p3_mode, input_buffer, self.rel_base)

        if instruction == 99:
            self.await_input = False
            self.halt = True
            self.prog_pointer += 1
            return None, output
        elif instruction in (1, 2, 7, 8):
            arg1 = self.get_from_memory(1, p1_mode)
            arg2 = self.get_from_memory(2, p2_mode)
            if instruction == 1:
                # add
                res = arg1 + arg2
            elif instruction == 2:
                # multiply
                res = arg1 * arg2
            elif instruction == 7:
                res = int(arg1 < arg2)
            elif instruction == 8:
                res = int(arg1 == arg2)
            if p3_mode == 0:
                # direct mode
                self.memory[self.memory[self.prog_pointer + 3]] = res
            elif p3_mode == 2:
                # relative mode
                self.memory[
                    self.rel_base + self.memory[
                        self.prog_pointer + 3]] = res
            else:
                raise ValueError(
                    ("Unexpected Parameter Mode for "
                     "input (opcode {1}): {0}").format(
                        p3_mode, op_code))
            self.prog_pointer += 4
        elif instruction == 3:
            # Get input
            if input_buffer is not None:
                i = input_buffer
            else:
                # We should pause and await input
                self.await_input = True
                return None, output
            i = int(i)
            if p1_mode == 0:
                # direct mode
                self.memory[self.memory[self.prog_pointer + 1]] = i
            elif p1_mode == 2:
                # relative mode
                self.memory[
                    self.rel_base + self.memory[
                        self.prog_pointer + 1]] = i
            else:
                raise ValueError(
                    ("Unexpected Parameter Mode for "
                     "input (opcode 3): {0}").format(
                        p1_mode))
            self.prog_pointer += 2
        elif instruction == 4:
            # Put Output
            arg1 = self.get_from_memory(1, p1_mode)
            output = arg1
            self.prog_pointer += 2
        elif instruction in (5, 6):
            # Jump if true/false
            arg1 = self.get_from_memory(1, p1_mode)
            arg2 = self.get_from_memory(2, p2_mode)
            if (instruction == 5 and arg1 != 0) or (instruction == 6 and arg1 == 0):  # noqa
                self.prog_pointer = arg2
            else:
                self.prog_pointer += 3
        elif instruction == 9:
            # Set Relative Base
            arg1 = self.get_from_memory(
                1, p1_mode)
            self.rel_base += arg1
            self.prog_pointer += 2
        else:
            raise ValueError("Unexpected Opcode: {0}".format(instruction))
        self.await_input = False
        return None, output

    def run(self, input_buffer=None, interactive=False):
        if self.halt:
            raise RuntimeError("IntComp is in a halt state already!")
        elif self.await_input and input_buffer is None:
            return
        else:
            while True:
                input_buffer, output = self.take_step(
                    input_buffer=input_buffer)
                if output is not None:
                    if interactive:
                        print("OUT:   {}".format(output))
                    else:
                        return output
                if self.halt:
                    if interactive:
                        print("HALT")
                    break
                if self.await_input:
                    if interactive:
                        input_buffer = input("INPUT: ")
                        if input_buffer != '':
                            self.await_input = False
                        else:
                            print("FAILED INPUT")
                            return
                    else:
                        return


class HullGrid(object):
    """A theoretical infinite hull grid."""
    def __init__(self, init_white=None):
        # We initialise the hull black
        self._white_squares = set()
        self._ever_painted = set()
        if init_white:
            for elem in init_white:
                self._white_squares.add(elem)

    def get_colour(self, x, y):
        if (x, y) in self._white_squares:
            return 1
        else:
            return 0

    def set_colour(self, x, y, col):
        addr = (x, y)
        # Add this to the ever painted list
        self._ever_painted.add(addr)
        if col == 1:
            # Paint it white
            self._white_squares.add(addr)
        elif col == 0:
            if addr in self._white_squares:
                self._white_squares.remove(addr)
            else:
                pass
        else:
            raise ValueError("Unexpected colour: {0}".format(col))

    def num_painted(self):
        return len(self._ever_painted)

    def to_str(self):
        x_cords = [elem[0] for elem in self._white_squares]
        y_cords = [elem[1] for elem in self._white_squares]
        x_extents = (min(x_cords), max(x_cords))
        y_extents = (min(y_cords), max(y_cords))
        x0 = x_extents[0]
        y0 = y_extents[0]
        # Make a blank grid and add white.
        grid = []
        for y in range(0, y_extents[1] + 1 - y_extents[0]):
            buff = []
            for x in range(0, x_extents[1] + 1 - x_extents[0]):
                buff.append('.')
            grid.append(buff)
        # Add all the white
        for x, y in self._white_squares:
            grid[y - y0][x - x0] = '#'

        # Need to reverse list so it's the right way up.
        grid.reverse()
        return '\n'.join([''.join(elem) for elem in grid])


paint_code = [3,8,1005,8,334,1106,0,11,0,0,0,104,1,104,0,3,8,102,-1,8,10,101,1,10,10,4,10,108,0,8,10,4,10,1002,8,1,28,2,1108,5,10,3,8,102,-1,8,10,101,1,10,10,4,10,1008,8,1,10,4,10,1001,8,0,55,1,102,18,10,1,2,5,10,3,8,1002,8,-1,10,1001,10,1,10,4,10,108,1,8,10,4,10,1001,8,0,84,1,106,11,10,2,1008,6,10,1,4,4,10,1006,0,55,3,8,1002,8,-1,10,1001,10,1,10,4,10,108,0,8,10,4,10,102,1,8,121,1,107,9,10,3,8,102,-1,8,10,101,1,10,10,4,10,108,1,8,10,4,10,101,0,8,147,2,1002,4,10,2,104,18,10,1,107,16,10,1,108,8,10,3,8,102,-1,8,10,101,1,10,10,4,10,108,0,8,10,4,10,102,1,8,185,3,8,1002,8,-1,10,1001,10,1,10,4,10,1008,8,0,10,4,10,101,0,8,208,2,1009,16,10,1006,0,7,1006,0,18,1,1105,8,10,3,8,1002,8,-1,10,101,1,10,10,4,10,108,1,8,10,4,10,101,0,8,243,2,1105,20,10,2,106,10,10,1006,0,67,3,8,1002,8,-1,10,101,1,10,10,4,10,108,0,8,10,4,10,1001,8,0,276,2,1103,5,10,2,1104,7,10,1006,0,35,2,1105,3,10,3,8,1002,8,-1,10,101,1,10,10,4,10,1008,8,1,10,4,10,1002,8,1,314,101,1,9,9,1007,9,1097,10,1005,10,15,99,109,656,104,0,104,1,21102,936995824532,1,1,21101,0,351,0,1105,1,455,21102,1,387508445964,1,21102,362,1,0,1106,0,455,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,21102,1,235244973059,1,21101,409,0,0,1106,0,455,21102,179410541659,1,1,21101,0,420,0,1105,1,455,3,10,104,0,104,0,3,10,104,0,104,0,21101,868402070292,0,1,21102,1,443,0,1106,0,455,21102,1,709584749324,1,21102,454,1,0,1106,0,455,99,109,2,22102,1,-1,1,21101,40,0,2,21102,486,1,3,21101,0,476,0,1106,0,519,109,-2,2105,1,0,0,1,0,0,1,109,2,3,10,204,-1,1001,481,482,497,4,0,1001,481,1,481,108,4,481,10,1006,10,513,1101,0,0,481,109,-2,2106,0,0,0,109,4,2102,1,-1,518,1207,-3,0,10,1006,10,536,21102,0,1,-3,21202,-3,1,1,22102,1,-2,2,21102,1,1,3,21102,555,1,0,1106,0,560,109,-4,2106,0,0,109,5,1207,-3,1,10,1006,10,583,2207,-4,-2,10,1006,10,583,21201,-4,0,-4,1106,0,651,21201,-4,0,1,21201,-3,-1,2,21202,-2,2,3,21102,602,1,0,1106,0,560,22102,1,1,-4,21101,0,1,-1,2207,-4,-2,10,1006,10,621,21102,0,1,-1,22202,-2,-1,-2,2107,0,-3,10,1006,10,643,21201,-1,0,1,21102,643,1,0,106,0,518,21202,-2,-1,-2,22201,-4,-2,-4,109,-5,2106,0,0]  # noqa


class HullPainterSim(object):
    def __init__(self, init_white=None):
        # Set us up with a new hull
        self.hull = HullGrid(init_white=init_white)
        # Set up with a fresh intcomp
        self.comp = IntComp(code=paint_code)
        # Initialise position
        self.pos = (0, 0)  # x, y
        # 0 means up. 1 is right, 2 is down, 3 is left (-1 is also left)
        self.dir = 0

    def run(self):
        input_buffer = None
        next_out_paint = True
        while True:
            r = self.comp.run(input_buffer=input_buffer)
            input_buffer = None
            if r is not None:
                # Got an output. What are we waiting for?
                if next_out_paint:
                    self.hull.set_colour(self.pos[0], self.pos[1], r)
                    # Expect direction next: next_out_paint
                    next_out_paint = False
                else:
                    # Turn
                    if r == 1:
                        self.dir += 1
                    elif r == 0:
                        self.dir -= 1
                    else:
                        raise ValueError(
                            "Unexpected direction output: {0}".format(
                                self.dir))
                    self.dir %= 4
                    # Move
                    if self.dir % 2 == 0:
                        # We're pointing up/down
                        self.pos = (self.pos[0], self.pos[1] - (self.dir - 1))
                    else:
                        # We're pointing left/right
                        self.pos = (self.pos[0] - (self.dir - 2), self.pos[1])
                    # Expect colour next: next_out_paint
                    next_out_paint = True
            if self.comp.halt:
                print("> Halt!: Ever painted: {0}".format(
                    self.hull.num_painted()))
                break
            if self.comp.await_input:
                hull_col = self.hull.get_colour(*self.pos)
                input_buffer = hull_col

    def to_str(self):
        return self.hull.to_str()


def test():
    # Test
    c = IntComp('asteroid')
    c.run(interactive=True)


def challenge_1():
    r = HullPainterSim()
    r.run()


def challenge_2():
    r = HullPainterSim(init_white=[(0, 0)])
    r.run()
    print(r.to_str())


challenge_2()
