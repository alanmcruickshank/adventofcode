"""Day 9 - Intcode Boost."""


class IntComp(object):
    def __init__(self, code):
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
        # print(op_code, instruction, self.prog_pointer, input_buffer)

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
                self.memory[self.rel_base + self.memory[self.prog_pointer + 3]] = res
            else:
                raise ValueError("Unexpected Parameter Mode for input (opcode {1}): {0}".format(p3_mode, op_code))
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
                self.memory[self.rel_base + self.memory[self.prog_pointer + 1]] = i
            else:
                raise ValueError("Unexpected Parameter Mode for input (opcode 3): {0}".format(p1_mode))
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
            if (instruction == 5 and arg1 != 0) or (instruction == 6 and arg1 == 0):
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

    def run(self, input_buffer=None):
        if not self.halt:
            while True:
                input_buffer, output = self.take_step(input_buffer=input_buffer)
                if output:
                    return output
                if self.halt:
                    break
                if self.await_input:
                    return
        else:
            raise RuntimeError("IntComp is in a halt state already!")



boost_code = [1102,34463338,34463338,63,1007,63,34463338,63,1005,63,53,1102,1,3,1000,109,988,209,12,9,1000,209,6,209,3,203,0,1008,1000,1,63,1005,63,65,1008,1000,2,63,1005,63,904,1008,1000,0,63,1005,63,58,4,25,104,0,99,4,0,104,0,99,4,17,104,0,99,0,0,1101,0,641,1026,1101,0,24,1014,1101,30,0,1015,1101,0,0,1020,1101,35,0,1000,1101,0,708,1029,1101,0,27,1009,1102,38,1,1007,1102,638,1,1027,1101,1,0,1021,1102,32,1,1003,1101,0,34,1012,1102,20,1,1017,1102,1,37,1010,1101,0,713,1028,1101,33,0,1019,1102,1,36,1001,1102,22,1,1005,1101,23,0,1018,1101,21,0,1016,1102,28,1,1006,1101,0,26,1011,1102,1,215,1022,1102,1,29,1013,1102,25,1,1004,1102,1,31,1008,1102,1,292,1025,1102,297,1,1024,1101,208,0,1023,1102,1,39,1002,109,12,1206,9,197,1001,64,1,64,1106,0,199,4,187,1002,64,2,64,109,11,2105,1,0,1001,64,1,64,1105,1,217,4,205,1002,64,2,64,109,2,21107,40,41,-9,1005,1016,235,4,223,1105,1,239,1001,64,1,64,1002,64,2,64,109,-28,1207,3,36,63,1005,63,261,4,245,1001,64,1,64,1105,1,261,1002,64,2,64,109,5,1207,1,31,63,1005,63,281,1001,64,1,64,1105,1,283,4,267,1002,64,2,64,109,22,2105,1,0,4,289,1105,1,301,1001,64,1,64,1002,64,2,64,109,-16,1201,0,0,63,1008,63,31,63,1005,63,323,4,307,1106,0,327,1001,64,1,64,1002,64,2,64,109,18,1205,-5,345,4,333,1001,64,1,64,1105,1,345,1002,64,2,64,109,-21,2101,0,-2,63,1008,63,32,63,1005,63,367,4,351,1106,0,371,1001,64,1,64,1002,64,2,64,109,6,21102,41,1,7,1008,1018,38,63,1005,63,395,1001,64,1,64,1105,1,397,4,377,1002,64,2,64,109,-1,21107,42,41,2,1005,1012,413,1106,0,419,4,403,1001,64,1,64,1002,64,2,64,109,-10,2107,36,0,63,1005,63,435,1106,0,441,4,425,1001,64,1,64,1002,64,2,64,109,9,21108,43,44,9,1005,1018,461,1001,64,1,64,1105,1,463,4,447,1002,64,2,64,109,-10,2102,1,8,63,1008,63,39,63,1005,63,483,1105,1,489,4,469,1001,64,1,64,1002,64,2,64,109,21,21108,44,44,-1,1005,1019,511,4,495,1001,64,1,64,1106,0,511,1002,64,2,64,109,-18,1208,1,32,63,1005,63,533,4,517,1001,64,1,64,1105,1,533,1002,64,2,64,109,5,2101,0,-5,63,1008,63,37,63,1005,63,557,1001,64,1,64,1105,1,559,4,539,1002,64,2,64,109,8,1208,-8,35,63,1005,63,575,1105,1,581,4,565,1001,64,1,64,1002,64,2,64,109,-5,1202,-3,1,63,1008,63,38,63,1005,63,607,4,587,1001,64,1,64,1106,0,607,1002,64,2,64,109,-17,2107,31,10,63,1005,63,629,4,613,1001,64,1,64,1106,0,629,1002,64,2,64,109,31,2106,0,3,1105,1,647,4,635,1001,64,1,64,1002,64,2,64,109,-7,1201,-9,0,63,1008,63,32,63,1005,63,667,1106,0,673,4,653,1001,64,1,64,1002,64,2,64,109,-5,1202,-5,1,63,1008,63,41,63,1005,63,693,1105,1,699,4,679,1001,64,1,64,1002,64,2,64,109,16,2106,0,0,4,705,1105,1,717,1001,64,1,64,1002,64,2,64,109,-6,1205,-2,729,1105,1,735,4,723,1001,64,1,64,1002,64,2,64,109,-18,2102,1,1,63,1008,63,22,63,1005,63,761,4,741,1001,64,1,64,1105,1,761,1002,64,2,64,109,-2,2108,32,1,63,1005,63,783,4,767,1001,64,1,64,1105,1,783,1002,64,2,64,109,13,21102,45,1,-2,1008,1013,45,63,1005,63,809,4,789,1001,64,1,64,1105,1,809,1002,64,2,64,109,-13,2108,24,3,63,1005,63,829,1001,64,1,64,1106,0,831,4,815,1002,64,2,64,109,13,21101,46,0,-3,1008,1012,43,63,1005,63,851,1106,0,857,4,837,1001,64,1,64,1002,64,2,64,109,14,1206,-9,875,4,863,1001,64,1,64,1106,0,875,1002,64,2,64,109,-3,21101,47,0,-7,1008,1019,47,63,1005,63,901,4,881,1001,64,1,64,1105,1,901,4,64,99,21101,27,0,1,21101,0,915,0,1106,0,922,21201,1,66926,1,204,1,99,109,3,1207,-2,3,63,1005,63,964,21201,-2,-1,1,21102,942,1,0,1105,1,922,21202,1,1,-1,21201,-2,-3,1,21101,957,0,0,1106,0,922,22201,1,-1,-2,1106,0,968,22102,1,-2,-2,109,-3,2106,0,0]  # noqa
test_code_1 = [
    109, 1, 204, -1, 1001, 100, 1, 100, 1008, 100,
    16, 101, 1006, 101, 0, 99]
test_code_2 = [1102,34915192,34915192,7,4,7,99,0]
test_code_3 = [104,1125899906842624,99]


c = IntComp(code=boost_code)  #boost_code
input_buffer = None
while True:
    r = c.run(input_buffer=input_buffer)
    if r is not None:
        print(r)
    if c.halt:
        break
    if c.await_input:
        input_buffer = input('INPUT-->')
