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
    
    def __int__(self):
        if self.name == 'numeric':
            return self.val
        else:
            raise ValueError("{} has no numeric value".format(self.name))
    
    def __repr__(self):
        return "<{0}: {1!r}>".format(self.name, self.val)


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
    
    def __int__(self):
        if self.val[0].name == 'operator':
            raise ValueError("Expression starts with operator! {0}".format(self))
        
        running = int(self.val[0])
        idx = 1

        while self.val[idx:]:
            if self.val[idx].name != 'operator':
                raise ValueError("Expected operator!? {0}".format(self.val[idx:]))
            elif self.val[idx].val == '+':
                running += int(self.val[idx + 1])
            elif self.val[idx].val == '*':
                running *= int(self.val[idx + 1])
            # Advance by two (operator and value)
            idx += 2
        
        return running


for example in examples:
    print("Example:", example)
    exp = Expression.from_raw(example)
    print(exp)
    print(int(exp))

print("Homework!")
running = 0
with open("018-maths-input.txt") as txt_file:
    for line in txt_file:
        running += int(Expression.from_raw(line))
print("Homework sum:", running)
# Part 1 answer: 5019432542701