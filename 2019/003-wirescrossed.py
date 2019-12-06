"""Crossed Wires. Puzzle Day 3."""

# Don't do it using a grid, use line segments.
# We need to convert relative line segments into
# absolute line segments.

# An absolute line segment has a start (x, y) a
# length (l) and a direction (u,d,l,r)

# coordinates are up is +y and r is +x.

class LineSeg(object):
    __slots__ = ['x', 'y', 'l', 'd', 'cd']

    def __init__(self, x, y, l, d, cd):
        self.x = x
        self.y = y
        self.l = l
        d = d.lower()
        if d not in ('u', 'd', 'l', 'r'):
            raise ValueError("Unexpected direction {0}".format(d))
        self.d = d
        # cd is the cumulative distance to the *start* of this
        # segment.
        self.cd = cd

    def get_end(self):
        if self.d == 'u':
            return (self.x, self.y + self.l)
        elif self.d == 'd':
            return (self.x, self.y - self.l)
        elif self.d == 'l':
            return (self.x - self.l, self.y)
        elif self.d == 'r':
            return (self.x + self.l, self.y)
        else:
            raise ValueError("Unexpected direction {0}".format(self.d))

    @property
    def orientation(self):
        if self.d in ('u', 'd'):
            return 'v'
        elif self.d in ('r', 'l'):
            return 'h'
        else:
            raise ValueError("Unexpected direction {0}".format(self.d))

    @property
    def start(self):
        return (self.x, self.y)

    @property
    def end(self):
        return self.get_end()

    @classmethod
    def from_raw(cls, raw_code, last_seg=None):
        if last_seg:
            start = last_seg.get_end()
            cd = last_seg.cd + last_seg.l
        else:
            start = (0, 0)
            cd = 0

        raw_d = raw_code[0]
        raw_l = int(raw_code[1:])
        return cls(start[0], start[1], raw_l, raw_d.lower(), cd)

    def __str__(self):
        return "<LineSeg: {s!r} {d}:{l}>".format(
            s=self.start, d=self.d, l=self.l)

    def __repr__(self):
        return str(self)

    @classmethod
    def list_from_raw(cls, raw_list, start_seg=None):
        buff = []
        if isinstance(raw_list, str):
            raw_list = raw_list.split(',')
        seg = start_seg
        for elem in raw_list:
            seg = cls.from_raw(elem, last_seg=seg)
            buff.append(seg)
        return buff
    
    @staticmethod
    def is_between(a, b1, b2):
        """Return true if a is between b1 and b2."""
        # TODO: check whether this should be < or <=
        if b1 == b2:
            # Return true if they're all the same
            return a == b1
        elif b1 > b2:
            return b1 >= a >= b2
        elif b2 > b1:
            return b2 >= a >= b1
        else:
            raise ValueError("Unexpected is_between, {a}, {b}, {c}".format(**locals()))

    def insersects(self, other):
        """Does this line segment intersect the other.

        Returns:
            Tuple of ((ix, iy), (cd1, cd2)) or None if not.

        """

        # Check for orthogonality
        if self.orientation == other.orientation:
            return None
        
        # Test based on whether this one is horizontal or vertical
        if self.orientation == 'v':
            # This means that for self, the x is the same at the start and end
            if self.is_between(self.start[0], other.start[0], other.end[0]):
                # AND it means that for other the y is the same at the start and end
                if self.is_between(other.start[1], self.start[1], self.end[1]):
                    cd1 = self.cd + abs(other.start[1] - self.start[1])
                    cd2 = other.cd + abs(other.start[0] - self.start[0])
                    return ((self.start[0], other.start[1]), (cd1, cd2))
        elif self.orientation == 'h':
            # Ditto from above but the other way round.
            if self.is_between(self.start[1], other.start[1], other.end[1]):
                # AND it means that for other the y is the same at the start and end
                if self.is_between(other.start[0], self.start[0], self.end[0]):
                    cd1 = self.cd + abs(other.start[0] - self.start[0])
                    cd2 = other.cd + abs(other.start[1] - self.start[1])
                    return ((other.start[0], self.start[1]), (cd1, cd2))
        return None

    @classmethod
    def list_intersections(cls, l1, l2):
        """Compare two lists for intersections."""
        intersection_buff = []
        for elem1 in l1:
            for elem2 in l2:
                i = elem1.insersects(elem2)
                if i:
                    intersection_buff.append(i)
        return intersection_buff

    @staticmethod
    def best_manhattan_distance(intersection_list):
        """Returns the best manhattan distance from the list of tuples."""
        best = None
        for (x, y), _ in intersection_list:
            d = abs(x) + abs(y)
            if best:
                if d < best:
                    best = d
            else:
                best = d
        return best

    @staticmethod
    def best_timing_distance(intersection_list):
        """Returns the best timing distance from the list of tuples."""
        best = None
        for (x, y), (cd1, cd2) in intersection_list:
            t = cd1 + cd2
            if best:
                if t < best:
                    best = t
            else:
                best = t
        return best


def compare(l1, l2):
    ib = LineSeg.list_intersections(LineSeg.list_from_raw(l1), LineSeg.list_from_raw(l2))
    print(ib)
    bd = LineSeg.best_manhattan_distance(ib)
    print(bd)
    td = LineSeg.best_timing_distance(ib)
    print(td)
    return td

# Example 1
compare(
    "R75,D30,R83,U83,L12,D49,R71,U7,L72",
    "U62,R66,U55,R34,D71,R55,D58,R83"
)
# Example 2
compare(
    "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51",
    "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"
)
# Test
compare(
    "R999,D586,L462,D725,L236,U938,R366,D306,R263,D355,R354,D332,L599,U48,R829,U210,R697,D534,L19,U991,L110,U981,L954,U323,R851,U290,R76,D513,R844,D780,L257,D24,L586,U865,L341,U572,L122,D304,R398,D641,L221,U726,R270,D321,R503,D112,L151,D179,R439,U594,R242,U1,L484,D259,L604,U760,R362,D93,R29,D647,R482,U814,L214,D510,R281,U327,L170,D993,R191,D33,L305,D657,L897,U609,R512,D866,R654,U980,L899,D602,L141,D365,L13,D584,L706,U404,L238,U720,L732,U716,R672,U979,L49,D352,R712,U396,L843,D816,L276,U906,L375,D410,R275,U664,R487,D158,L713,D451,L859,U194,L736,D51,R659,U632,R586,U342,L222,U184,R741,U989,L175,D521,R820,U183,L950,D888,R54,D149,R776,D200,R939,U529,L377,D226,R769,U395,R392,U570,L398,D358,L644,D975,R578,D687,L133,D884,R822,D226,L527,U439,R175,D388,L539,D450,L391,U392,L131,U134,R873,U741,R761,U620,R667,D31,R481,D945,L373,D463,R57,D402,R181,U340,L835,U81,R908,U257,R592,U702,R713,D352,R418,D486,L904,U866,R828,D545,R578,U469,L845,D437,R371,D246,L996,D920,L171,U83,R471,D152,R550,U344,L390,U287,L126,D883,L576,U303,L68,U854,L463,D915,R184,D282,L513,U909,R770,U638,L751,U168,R354,D480,R19,U144,R381,D554,R594,D526,L957,D464,R267,D802,L709,U306,L907,D266,L871,U286,R975,D549,L732,U721,R825,U753,R443,U465,L966,U982,L833,D62,L5,U299,R500,D168,R155,D102,R455,D855,L376,D479,L469,D6,R588,U301,R329,U19,L63,D488,L936,D238,L798,D452,L231,D652,R935,D522,L401,U234,L20,U285,L949,D88,L120,D159,R641,D960,L946,U516,L530,D447,R23,U962,R860,D352,R904,D241,R702,U108,L155,U99,L43,D401,R19",
    "L1008,U23,L793,D944,L109,U830,L103,U255,L391,D574,R433,U468,R800,D831,L39,U8,L410,D467,R655,D287,R550,U467,L627,D529,R361,D865,L755,D895,L148,U110,R593,U567,L646,D89,L133,D552,R576,U228,L119,U734,R591,U680,L163,D498,L394,U884,R217,U46,R684,D499,L522,U373,L322,U347,R48,D459,L692,U569,R267,U296,L949,U915,R599,D113,R770,U322,R304,U920,L880,D257,R915,D672,L950,U209,R601,U663,R461,D514,R415,U82,L396,U233,R606,U500,R70,D696,R945,D686,L405,U176,R728,U562,L710,D35,R707,D931,L857,U792,R337,D490,L963,U731,R909,U532,R375,D990,L154,U660,L17,U32,R593,U529,R136,U835,R717,U255,L93,D295,L473,U608,L109,D858,R719,U207,R60,D36,R790,D382,L684,D233,R988,U625,R410,U804,R552,D578,L440,D749,R653,U362,L900,U549,R790,D870,R672,U503,R343,D343,R738,D270,R494,D527,L182,U654,R933,D594,R447,U933,R4,U364,L309,U967,R648,U537,R990,U203,R584,D474,L852,U736,R305,D781,R774,D92,L398,U207,R472,D664,R369,U807,L474,U588,R339,D536,R305,D506,R516,U772,R177,U450,L211,U850,R777,U483,L595,U104,L916,U548,R256,U173,L27,D167,L574,D288,R569,U192,R771,D98,R432,U165,L651,D524,L582,D698,L393,D152,L280,U461,R573,D771,R833,D409,R991,U996,R780,U617,R63,U563,L844,D63,R15,U634,R643,D124,L147,D583,R716,D28,L799,D59,R819,D723,L43,D975,L755,D635,R118,U325,L969,D445,R374,D797,L821,U118,R962,D643,R127,U267,R768,D50,L343,U80,R281,U575,R618,D718,L74,U146,R242,D547,L492,U71,R826,D483,L402,U953,R184,U707,L973,D550,L593,U281,L652,D247,L254,D60,R908,U581,L731,D634,R286,D186,R9,D983,L181,U262,R241,D674,R463,U238,R600"
)

