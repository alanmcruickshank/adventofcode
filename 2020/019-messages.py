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
        return self._rules[ref].match(s, self)
    
    def is_total_match_ref(self, s, ref):
        matches = self.match_ref(s, ref)
        return any(unmatched == "" for _, unmatched in matches)
    
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
        """Match returns list of [(matched, unmatched)] as a list of tuples."""
        matches = []
        for option in self.options:
            forks = [("", s)] 
            for seq_elem in option:
                # Have we purged all the forks?
                if not forks:
                    break
                new_forks = []
                for matched_so_far, remainder in forks:
                    if isinstance(seq_elem, str):
                        # try matching text
                        if remainder.startswith(seq_elem):
                            new_forks.append((matched_so_far + seq_elem, remainder[len(seq_elem):]))
                            continue
                    elif isinstance(seq_elem, int):
                        sub_matches = lib.match_ref(remainder, seq_elem)
                        for matched, unmatched in sub_matches:
                            if matched:
                                new_forks.append((matched_so_far + matched, unmatched))
                                continue
                    
                    # If we get here we didn't match an element in the sequence for
                    # this fork. Therefore it won't be added to new_forks and will be
                    # abandonded.
                forks = new_forks

            # Any forks that are left at this stage are matches
            matches += forks 
                
        if matches:
            return matches
        else:
            return [("", s)]
    
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
for fname in ["019-messages-test.txt", "019-messages-test-2.txt", "019-messages-input.txt"]:
    print(fname)
    with open(fname) as f:
        rules, tests = f.read().split("\n\n")
    lib = MatcherLibrary(rules)
    print(">> Matches:", lib.count_total_matches(tests.split('\n')))
    # Part 1 answer: 192

    ## Part 2:
    if "11: 42 31" in rules:
        print("> Part 2:")
        rules = rules.replace("11: 42 31", "11: 42 31 | 42 11 31").replace("8: 42", "8: 42 | 42 8")
        lib = MatcherLibrary(rules)
        print(">> Matches:", lib.count_total_matches(tests.split('\n')))
        # Part 2 answer: 296
