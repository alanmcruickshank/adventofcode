"""Advent of Code Day 18

https://adventofcode.com/2020/day/18

Wrong Maths.
"""

import re

examples = [
    "1 + 2 * 3 + 4 * 5 + 6",
    "1 + (2 * 3) + (4 * (5 + 6))",
    "2 * 3 + (4 * 5)",
    "5 + (8 * 3 + 9 + 3 * 4 * 3)",
    "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))",
    "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2",
]


class ExpressionElement:
    matchers = {
        'numeric': re.compile(r"[0-9]+"),
        'operator': re.compile(r"[\+\*]"),
        'start_bracket': re.compile(r"\("),
        'end_bracket': re.compile(r"\)"),
        'whitespace': re.compile(r"\s")
    }

    def __init__(self, name, val):
        if name not in self.matchers and name != 'expression':
            raise ValueError("Invalid expression name.")
        self.name = name

        if name == 'numeric':
            val = int(val)
        if name == 'expression' and not isinstance(val, list):
            raise ValueError("Expression must have a list of values.")
        self.val = val
    
    def to_int(self, **kwargs):
        return int(self)

    def __int__(self):
        if self.name == 'numeric':
            return self.val
        else:
            raise ValueError("{} has no numeric value".format(self.name))
    
    def __repr__(self):
        return "<{0}: {1!r}>".format(self.name, self.val)

    def __eq__(self, other):
        return self.name == other.name and self.val == other.val


class NumericElement(ExpressionElement):
    def __init__(self, val):
        super().__init__(name='numeric', val=val)


class Expression(ExpressionElement):
    def __init__(self, lexed):
        # Handle bracket counting
        subdivided = []
        bracket_depth = 0
        lexed_buffer = []
        for elem in lexed:
            if elem.name == "start_bracket":
                # Increment depth
                bracket_depth += 1
                # If this was the first, don't buffer it.
                if bracket_depth == 1:
                    continue
            elif elem.name == "end_bracket":
                # Decrement depth
                bracket_depth -= 1
            elif elem.name == "whitespace":
                # Ignore whitespace
                continue

            if bracket_depth:
                # We're in a bracket, just buffer the token
                lexed_buffer.append(elem)
            else:
                if lexed_buffer:
                    # We're not in a bracket but have a buffer
                    # Append the section. It will subdivide it.
                    subdivided.append(Expression(lexed_buffer))
                    # Clean the buffer
                    lexed_buffer = []
                # Buffer the token if it's of the right type.
                if elem.name in ('numeric', 'operator'):
                    subdivided.append(elem)
        # Initialise
        super().__init__('expression', subdivided)
    
    @classmethod
    def from_raw(cls, raw):
        return cls(lexed=cls.lex(raw))

    @classmethod
    def lex(cls, s):
        tokens = []
        while s:
            for key in cls.matchers:
                m = cls.matchers[key].match(s)
                if m:
                    tokens.append(ExpressionElement(key, m.group()))
                    s = s[len(m.group()):]
                    break
            else:
                raise RuntimeError("Lex Failed: {!r}".format(s))
        return tokens
    
    def __repr__(self):
        return "<Expression: {!r}>".format(self.val)

    @classmethod
    def eval(cls, elems, level="basic", with_subdivide=True):
        # print("EVAL: ", level, with_subdivide, elems)
        if elems[0].name == 'operator':
            raise ValueError("Expression starts with operator! {0}".format(self))
        # In advanced maths, preprocess any additions
        if level == 'advanced' and with_subdivide:
            new_elems = []
            elem_buff = []
            for elem in elems:
                if elem.name == 'operator' and elem.val == '*':
                    new_elems.append(NumericElement(cls.eval(elem_buff, level=level, with_subdivide=False)))
                    new_elems.append(elem)
                    elem_buff = []
                else:
                    elem_buff.append(elem)
            if elem_buff:
                new_elems.append(NumericElement(cls.eval(elem_buff, level=level, with_subdivide=False)))
            # if elems != new_elems:
            #     print("Reduce: ", elems, " TO ", new_elems)
            elems = new_elems

        running = elems[0].to_int(level=level)
        idx = 1
        while elems[idx:]:
            if elems[idx].name != 'operator':
                raise ValueError("Expected operator!? {0} in {1}".format(elems[idx:], elems))
            elif elems[idx].val == '+':
                running += elems[idx + 1].to_int(level=level)
            elif elems[idx].val == '*':
                running *= elems[idx + 1].to_int(level=level)
            # Advance by two (operator and value)
            idx += 2
        return running
    
    def to_int(self, level="basic"):
        return self.eval(self.val, level=level)

    def __int__(self):
        return self.to_int()


for example in examples:
    print("Example:", example)
    exp = Expression.from_raw(example)
    print(exp)
    for level in ["basic", "advanced"]:
        print(level, exp.to_int(level=level))


print("Homework!")
running = 0
with open("018-maths-input.txt") as txt_file:
    for line in txt_file:
        running += int(Expression.from_raw(line))
print("Homework sum:", running)
# Part 1 answer: 5019432542701
running = 0
with open("018-maths-input.txt") as txt_file:
    for line in txt_file:
        running += Expression.from_raw(line).to_int(level="advanced")
print("Advanced Homework sum:", running)
# Part 2 answer: 70518821989947
