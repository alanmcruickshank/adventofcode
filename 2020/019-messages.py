"""Advent of Code Day 19

https://adventofcode.com/2020/day/19

REGEX.
"""


class MatcherLibrary:
    def __init__(self, raw_rules):
        if isinstance(raw_rules, str):
            raw_rules = raw_rules.split('\n')
        self.raw_rules = raw_rules

        self._rules = {}

        for raw_rule in self.raw_rules:
            rule_no, _, rule_def = raw_rule.partition(':')
            self._rules[int(rule_no)] = Matcher(rule_def)
        
    def match_ref(self, s, ref):
        # print("...matchref", s, ref)
        if ref in (8, 11):
            print("......match_ref IN", ref, s)
            m = self._rules[ref].match(s, self)
            print("......match_ref OUT", ref, s, m)
            return m
        else:
            return self._rules[ref].match(s, self)
    
    def is_total_match_ref(self, s, ref):
        print("...is_total_match_ref IN", s)
        matched, unmatched = self.match_ref(s, ref)
        print("...is_total_match_ref OUT", s, unmatched == "")
        return unmatched == ""
    
    def count_total_matches(self, messages, ref=0):
        return sum(self.is_total_match_ref(elem, ref) for elem in messages)


class Matcher:
    def __init__(self, definition):
        self.definition = definition
        self.options = []
        for opt in definition.split('|'):
            option = []
            for seq_elem in opt.strip().split(' '):
                # Attempt to coerce to integer
                try:
                    seq_elem = int(seq_elem)
                except:
                    # Otherwise strip quotes
                    seq_elem = seq_elem.strip('"')
                option.append(seq_elem)
            self.options.append(option)

    def match(self, s, lib):
        """Match returns (matched, unmatched) as a list of tuples."""
        # print("...match", s, self)
        matches = []
        for option in self.options:
            print(".........matchopt", repr(s), option)
            temp_s = s
            matched_s = ""
            for seq_elem in option:
                # print(".......matchoptseq", temp_s, seq_elem)
                if isinstance(seq_elem, str):
                    # try matching text
                    if temp_s.startswith(seq_elem):
                        matched_s += seq_elem
                        temp_s = temp_s[len(seq_elem):]
                        continue
                elif isinstance(seq_elem, int):
                    matched, unmatched = lib.match_ref(temp_s, seq_elem)
                    if matched:
                        matched_s += matched
                        temp_s = unmatched
                        continue
                
                # If we get here we didn't match an element in the sequence
                matched_s = ""
                break

            if matched_s:
                matches.append((matched_s, temp_s))
                
        if matches:
            if len(matches) == 1:
                return matches[0]
            else:
                # Multiple matches!
                # Try returning the longest.
                matches = sorted(matches, key=lambda m: len(m[0]), reverse=True)
                return matches[0]
                #raise ValueError("Multiple Matches! {0}".format(matches))

        # If we get here we didn't match any entirely
        print(".........matchopt", s, option, "NO")
        return "", s
    
    def __repr__(self):
        return "<Matcher: {0}>".format(self.options)


test_rule_sets = [
    [
        "0: 1 2",
        '1: "a"',
        '2: 1 3 | 3 1',
        '3: "b"',
    ]
]

print("### Testing...")
for rule_set in test_rule_sets:
    lib = MatcherLibrary(rule_set)
    for test_str in ["aab", "aba", "bbb"]:
        print(".", test_str, lib.is_total_match_ref(test_str, 0))

print("### For Reals...")
for fname in ["019-messages-test.txt", "019-messages-test-2.txt"]: # "019-messages-input.txt"
    print(fname)
    with open(fname) as f:
        rules, tests = f.read().split("\n\n")
    lib = MatcherLibrary(rules)
    # print(lib._rules)
    #print(">> Matches:", lib.count_total_matches(tests.split('\n')))
    # Part 1 answer: 192

    ## Part 2:
    if "11: 42 31" in rules:
        print("> Part 2:")
        rules = rules.replace("11: 42 31", "11: 42 31 | 42 11 31").replace("8: 42", "8: 42 | 42 8")
        lib = MatcherLibrary(rules)
        # NEEDED
        # print(">> Matches:", lib.count_total_matches(tests.split('\n')))
        print(lib.is_total_match_ref("aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba", 0))
        




