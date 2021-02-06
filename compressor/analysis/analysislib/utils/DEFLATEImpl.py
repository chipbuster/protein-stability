## Contains details of implementation in DEFLATE, including codepoint info
class CodePointInfo(object):
    def __init__(self, code, exbits, vals):
        self.code = code
        self.extrabits = exbits
        self.minval = vals[0]
        self.maxval = vals[1]


DistCodePoints = [
    CodePointInfo(0, 0, (1, 1)),
    CodePointInfo(1, 0, (2, 2)),
    CodePointInfo(2, 0, (3, 3)),
    CodePointInfo(3, 0, (4, 4)),
    CodePointInfo(4, 1, (5, 6)),
    CodePointInfo(5, 1, (7, 8)),
    CodePointInfo(6, 2, (9, 12)),
    CodePointInfo(7, 2, (13, 16)),
    CodePointInfo(8, 3, (17, 24)),
    CodePointInfo(9, 3, (25, 32)),
    CodePointInfo(10, 4, (33, 48)),
    CodePointInfo(11, 4, (49, 64)),
    CodePointInfo(12, 5, (65, 96)),
    CodePointInfo(13, 5, (97, 128)),
    CodePointInfo(14, 6, (129, 192)),
    CodePointInfo(15, 6, (193, 256)),
    CodePointInfo(16, 7, (257, 384)),
    CodePointInfo(17, 7, (385, 512)),
    CodePointInfo(18, 8, (513, 768)),
    CodePointInfo(19, 8, (769, 1024)),
    CodePointInfo(20, 9, (1025, 1536)),
    CodePointInfo(21, 9, (1537, 2048)),
    CodePointInfo(22, 10, (2049, 3072)),
    CodePointInfo(23, 10, (3073, 4096)),
    CodePointInfo(24, 11, (4097, 6144)),
    CodePointInfo(25, 11, (6145, 8192)),
    CodePointInfo(26, 12, (8193, 12288)),
    CodePointInfo(27, 12, (12289, 16384)),
    CodePointInfo(28, 13, (16385, 24576)),
    CodePointInfo(29, 13, (24577, 32768)),
]

LenLitCodePoints = [
    CodePointInfo(257, 0, (3, 3)),
    CodePointInfo(258, 0, (4, 4)),
    CodePointInfo(259, 0, (5, 5)),
    CodePointInfo(260, 0, (6, 6)),
    CodePointInfo(261, 0, (7, 7)),
    CodePointInfo(262, 0, (8, 8)),
    CodePointInfo(263, 0, (9, 9)),
    CodePointInfo(264, 0, (10, 10)),
    CodePointInfo(265, 1, (11, 12)),
    CodePointInfo(266, 1, (13, 14)),
    CodePointInfo(267, 1, (15, 16)),
    CodePointInfo(268, 1, (17, 18)),
    CodePointInfo(269, 2, (19, 22)),
    CodePointInfo(270, 2, (23, 26)),
    CodePointInfo(271, 2, (27, 30)),
    CodePointInfo(272, 2, (31, 34)),
    CodePointInfo(273, 3, (35, 42)),
    CodePointInfo(274, 3, (43, 50)),
    CodePointInfo(275, 3, (51, 58)),
    CodePointInfo(276, 3, (59, 66)),
    CodePointInfo(277, 4, (67, 82)),
    CodePointInfo(278, 4, (83, 98)),
    CodePointInfo(279, 4, (99, 114)),
    CodePointInfo(280, 4, (115, 130)),
    CodePointInfo(281, 5, (131, 162)),
    CodePointInfo(282, 5, (163, 194)),
    CodePointInfo(283, 5, (195, 226)),
    CodePointInfo(284, 5, (227, 257)),
    CodePointInfo(285, 0, (258, 258)),
]


def find_lenlit_codepoint(val):
    for x in LenLitCodePoints:
        if x.minval <= val <= x.maxval:
            return x
    raise ValueError("Unable to find lenlit codepoint")


def find_dist_codepoint(val):
    for x in DistCodePoints:
        if x.minval <= val <= x.maxval:
            return x
    raise ValueError("Unable to find lenlit codepoint")
