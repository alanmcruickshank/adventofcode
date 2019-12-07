"""Day 5 - Intcode Asteroid."""


def make_code(buff, noun, verb):
    rbuff = buff.copy()
    rbuff[1] = noun
    rbuff[2] = verb
    return rbuff


def get_from_buff(buff, val, mode):
    if mode == 0:
        return buff[val]
    elif mode == 1:
        return val
    else:
        raise ValueError("Unexpected Parameter Mode: {0}".format(mode))


def take_step(buff, idx=0, input_buffer=None):
    op_code = buff[idx]
    instruction = op_code % 100
    p1_mode = (op_code % 1000) // 100
    p2_mode = (op_code % 10000) // 1000
    # p3_mode = (op_code % 100000) // 10000
    output = None
    # print(op_code, instruction, idx, input_buffer)

    if instruction == 99:
        return buff, idx + 1, True, None, output, False
    elif instruction in (1, 2, 7, 8):
        arg1 = get_from_buff(buff, buff[idx + 1], p1_mode)
        arg2 = get_from_buff(buff, buff[idx + 2], p2_mode)
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
        buff[buff[idx + 3]] = res
        idx += 4
    elif instruction == 3:
        # Get input
        if input_buffer is not None:
            i = input_buffer
        else:
            # We should pause and await input
            return buff, idx, False, None, output, True
        i = int(i)
        buff[buff[idx + 1]] = i
        idx += 2
    elif instruction == 4:
        # Put Output
        arg1 = get_from_buff(buff, buff[idx + 1], p1_mode)
        output = arg1
        idx += 2
    elif instruction in (5, 6):
        # Jump if true/false
        arg1 = get_from_buff(buff, buff[idx + 1], p1_mode)
        arg2 = get_from_buff(buff, buff[idx + 2], p2_mode)
        if (instruction == 5 and arg1 != 0) or (instruction == 6 and arg1 == 0):
            idx = arg2
        else:
            idx += 3
    else:
        raise ValueError("Unexpected Opcode: {0}".format(instruction))
    return buff, idx, False, None, output, False


class Amp(object):
    amp_code = [3,8,1001,8,10,8,105,1,0,0,21,34,47,72,93,110,191,272,353,434,99999,3,9,102,3,9,9,1001,9,3,9,4,9,99,3,9,102,4,9,9,1001,9,4,9,4,9,99,3,9,101,3,9,9,1002,9,3,9,1001,9,2,9,1002,9,2,9,101,4,9,9,4,9,99,3,9,1002,9,3,9,101,5,9,9,102,4,9,9,1001,9,4,9,4,9,99,3,9,101,3,9,9,102,4,9,9,1001,9,3,9,4,9,99,3,9,101,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,2,9,9,4,9,99,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,2,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,99]
    def __init__(self, code=None):
        self.code = code or self.amp_code
        self.halt = False
        self.prog_pointer = 0
        self.await_input = False

    def run(self, input_buffer=None):
        if not self.halt:
            while True:
                self.code, self.prog_pointer, self.halt, input_buffer, output, self.await_input = take_step(
                    self.code, idx=self.prog_pointer, input_buffer=input_buffer)
                if output:
                    return output
                if self.halt:
                    break
                if self.await_input:
                    return
        else:
            raise RuntimeError("Amp is in a halt state already!")

#def amplifier(phase, in_signal):
#    return iter_codes(amp_code, input_buff=[phase, in_signal])[0]

#def try_comb(phaselist):
#    sig = 0
#    o = []
#    for p in phaselist:
#        sig = amplifier(p, sig)
#    return sig

# print(try_comb([0,1,2,3,4]))



def init_bank(amp_bank, phase_list):
    """Run each amp long enough for them to accept their phase command."""
    p_buff = tuple(phase_list)
    for idx, a in enumerate(amp_bank):
        print("Amp {0}. -> {1}".format(idx, p_buff[idx]))
        a.run(input_buffer=p_buff[idx])
        # print(a, r)
    # Let's just check they're all awaiting instruction at this point
    assert all([a.await_input for a in amp_bank])
    # print("All amps awaiting input")
    return amp_bank


def feedback_run(phase_list):
    # Five amplifiers
    amp_bank = [Amp(), Amp(), Amp(), Amp(), Amp()]
    # Initialise
    amp_bank = init_bank(amp_bank, phase_list)
    sig = 0
    a_idx = 0
    a_4_out = None
    halt = False
    # Iterate
    while True:
        if a_idx < 5:
            print("Amp {0}. -> {1}".format(a_idx, sig))
            while True:
                if amp_bank[a_idx].halt:
                    print("HALT! sig: {0}".format(sig))
                    halt = True
                    break 
                out = amp_bank[a_idx].run(input_buffer=sig)
                if out is not None:
                    sig = out
                    break
            if a_idx == 4:
                a_4_out = sig
            if amp_bank[a_idx].halt or halt:
                print("HALT! sig: {0}".format(sig))
                break
            a_idx += 1
        else:
            a_idx = 0
    return a_4_out


print("IterRun: {0}".format(feedback_run([0,1,2,3,4])))

def brute_force():
    best_val = -9999999
    best_comb = None
    for i1 in range(5,10):
        for i2 in range(5,10):
            for i3 in range(5,10):
                for i4 in range(5,10):
                    for i5 in range(5,10):
                        t = (i1, i2, i3, i4, i5)
                        # Check no repetitions
                        if len(t) == len(set(t)):
                            o = feedback_run(t)
                            if o > best_val:
                                best_val = o
                                best_comb = t
    return best_comb, best_val

print(brute_force())