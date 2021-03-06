"""Day 13 - Intcode Arcade."""

import time


class IntComp(object):

    inbuilt_programs = {
        'test_a': [
            109, 1, 204, -1, 1001, 100, 1, 100, 1008, 100,
            16, 101, 1006, 101, 0, 99
        ],
        'asteroid': '005-asteroid.txt',
        'arcade': '013-arcade.txt',
    }

    def __init__(self, code, input_processor=None):
        # If code is a string, it might be a reference to an inbuilt program
        if isinstance(code, str):
            if code in self.inbuilt_programs:
                if isinstance(self.inbuilt_programs[code], str):
                    with open(self.inbuilt_programs[code], 'r') as f:
                        code = f.read().split(',')
                        # convert to integers
                        code = [int(c) for c in code]
                else:
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
        self.input_processor = input_processor

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

    def process_input(self, val):
        if self.input_processor is None:
            return int(val)
        elif isinstance(self.input_processor, dict):
            return int(self.input_processor.get(val, val))
        else:
            raise RuntimeError("Unknown input_processor")

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
                self.memory[self.rel_base + self.memory[
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
                self.memory[self.rel_base + self.memory[
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
                    if interactive is True or interactive == 'out':
                        print("OUT:   {}".format(output))
                    else:
                        return output
                if self.halt:
                    if interactive is not False:
                        print("HALT")
                    break
                if self.await_input:
                    if interactive is True or interactive == 'in':
                        input_buffer = input("INPUT: ")
                        if input_buffer != '':
                            input_buffer = self.process_input(input_buffer)
                            self.await_input = False
                        else:
                            print("FAILED INPUT")
                            return
                    else:
                        return


class _Getch:
    """Gets a single character from standard input.

    Does not echo to the screen."""
    def __init__(self):
        try:
            self.impl = _GetchWindows()
        except ImportError:
            self.impl = _GetchUnix()

    def __call__(self): return self.impl()


class _GetchUnix:
    def __init__(self):
        import tty, sys  # noqa

    def __call__(self):
        import sys, tty, termios  # noqa
        fd = sys.stdin.fileno()
        old_settings = termios.tcgetattr(fd)
        try:
            tty.setraw(sys.stdin.fileno())
            ch = sys.stdin.read(1)
        finally:
            termios.tcsetattr(fd, termios.TCSADRAIN, old_settings)
        return ch


class _GetchWindows:
    def __init__(self):
        import msvcrt  # noqa

    def __call__(self):
        import msvcrt
        return msvcrt.getch()


getch = _Getch()


class Arcade(object):
    def __init__(self, quarters=0, input_processor=None):
        self.comp = IntComp('arcade', input_processor=input_processor)
        self.elems = []
        # Set quarters (2 for free play)
        self.comp.memory[0] = quarters
        self.input_processor = input_processor

    @classmethod
    def find_x(self, view, char):
        for idx, row in enumerate(view.split('\n')):
            if idx < 2:
                # ignore the first couple of rows
                continue
            for idx, c in enumerate(row):
                if c == char:
                    return idx
        raise RuntimeError("Cannot detect {0!r} in view!".format(char))

    def run(self, interactive=False, auto=False, t_step=0.01):
        buff = []
        input_buffer = None
        while True:
            if self.comp.halt:
                view = self.to_str()
                print(view)
                break
            if self.comp.await_input:
                view = self.to_str()
                if auto:
                    if t_step > 0:
                        print(view)
                        time.sleep(t_step)
                    ball_x = self.find_x(view, 'o')
                    pad_x = self.find_x(view, '=')
                    if t_step > 0:
                        print(ball_x, pad_x)
                    if ball_x == pad_x:
                        input_buffer = 0
                    elif ball_x < pad_x:
                        input_buffer = -1
                    elif ball_x > pad_x:
                        input_buffer = 1
                    else:
                        raise RuntimeError("Unexpected! falsuihaelsfkj")
                else:
                    print(view)
                    print("CONTROL?")
                    input_buffer = getch().decode()
                    input_buffer = self.input_processor[input_buffer]
            out = self.comp.run(
                interactive=interactive,
                input_buffer=input_buffer
            )
            if out is not None:
                buff.append(out)
            if len(buff) == 3:
                self.elems.append(buff)
                buff = []

    def to_str(self):
        x_cords = [elem[0] for elem in self.elems]
        y_cords = [elem[1] for elem in self.elems]
        x_extents = (min(x_cords), max(x_cords))
        y_extents = (min(y_cords), max(y_cords))
        score = 0

        grid = []
        for y in range(0, y_extents[1] + 1):
            buff = []
            for x in range(0, x_extents[1] + 1):
                buff.append([' '])
            grid.append(buff)

        lookup = {
            0: ' ',  # Empty
            1: '+',  # Wall
            2: '#',  # Block
            3: '=',  # Paddle
            4: 'o'   # Ball
        }

        for x, y, code in self.elems:
            if x >= 0:
                grid[y][x] = lookup[code]
            elif x == -1 and y == 0:
                score = code
            else:
                raise ValueError("Unexpected coord")

        return (
            'score: {score}\n'.format(score=score)
            + '\n'.join([''.join(elem) for elem in grid])
        )


def test():
    # Test
    c = IntComp('asteroid')
    c.run(interactive=True)


def arcade_1():
    a = Arcade()
    a.run()
    cnt = 0
    for x, y, code in a.elems:
        if code == 2:
            cnt += 1
            print(x, y)
    print(cnt)
    print(a.to_str())


def arcade_2():
    a = Arcade(quarters=2,
               input_processor={'j': -1, 'k': 0, 'l': 1})
    a.run(auto=True, t_step=0)


arcade_2()
