"""Define the statemachine."""


class NoMoreInstructions(StopIteration):
    pass


class LoopDetected(StopIteration):
    pass


class InstructionMachine:

    def __init__(self, instructions):
        self._memory = 0
        if isinstance(instructions, str):
            instructions = self.process_raw_instructions(instructions)
        self.instructions = instructions
        self.instruction_pointer = 0
        self._instruction_idx_history = []
    
    @staticmethod
    def process_raw_instructions(raw_instructions):
        raw_instruction_list = [inst.split(' ') for inst in raw_instructions.split('\n')]
        return [(inst[0], int(inst[1])) for inst in raw_instruction_list]
    
    def run(self):
        finished_gracefully = False
        while True:
            try:
                self.execute_next()
            except StopIteration as err:
                if isinstance(err, NoMoreInstructions):
                    finished_gracefully = True
                break
        return finished_gracefully, self._memory, self.instruction_pointer

    def execute_next(self):
        return self.execute_loc(self.instruction_pointer)

    def execute_loc(self, idx):
        # Have we run out of instructions?
        if idx >= len(self.instructions):
            raise NoMoreInstructions("No More Instructions...")
        # Have we been here before?
        if idx in self._instruction_idx_history:
            raise LoopDetected("Loop Detected...")
        resp = self.execute(*self.instructions[idx])
        # Is the instruction pointer untouched?
        if self.instruction_pointer == idx:
            # Increment the instruction pointer
            self.instruction_pointer += 1
        # Store the current execution in the history
        self._instruction_idx_history.append(idx)
        # Return the result
        return resp

    def execute(self, cmd, val):
        # print("Executing: ", cmd, val)
        if cmd == 'nop':
            pass
        elif cmd == 'jmp':
            # Minus one because we'll add one at the end of this anyway
            self.instruction_pointer += val
        elif cmd == 'acc':
            self._memory += val
        else:
            raise ValueError("Unexpected command {0!r}".format())
        
