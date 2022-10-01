module grammar.htmlent;
import pegged.grammar: ParseTree;
import grammar.util;

// dfmt off
// Although HTML5 does accept some entity references without a trailing semicolon (such as &copy), these are not recognized here,
@safe pure nothrow
ParseTree HTML5EntityName(ParseTree p) {
/*
    outer switch is needed to not go over the bounds of the input
        e.g. only testing first 31 charactes of input if input has at least 31 characters
    every case of teh outer switch will fall through, allowing shrter and shorter strings to get matched
 */
    auto ret = ParseTree("HTML5EntityName");
    if (p.input.length <= p.end)
        return ret;
    immutable search = p.input[p.end .. $];
    if (search.length > 31)
        goto lenStart;
    switch (search.length) {
    lenStart:
        case 31:
            switch (search[0 .. 31]) {
                case "CounterClockwiseContourIntegral":
                    return ret.wordSuccess(p, search[0 .. 31]);
                default:
                    break;
            }
            goto case;
        case 24: .. case 30:
            switch (search[0 .. 24]) {
                case "DoubleLongLeftRightArrow":
                case "ClockwiseContourIntegral":
                    return ret.wordSuccess(p, search[0 .. 24]);
                default:
                    break;
            }
            goto case;
        case 23:
            switch (search[0 .. 23]) {
                case "NotNestedGreaterGreater":
                    return ret.wordSuccess(p, search[0 .. 23]);
                default:
                    break;
            }
            goto case;
        case 22:
            switch (search[0 .. 22]) {
                case "NotSquareSupersetEqual": case "DiacriticalDoubleAcute":
                    return ret.wordSuccess(p, search[0 .. 22]);
                default:
                    break;
            }
            goto case;
        case 21:
            switch (search[0 .. 21]) {
                case "NotSucceedsSlantEqual": case "NotRightTriangleEqual": case "NotPrecedesSlantEqual":
                case "NegativeVeryThinSpace": case "FilledVerySmallSquare": case "DoubleContourIntegral":
                case "CloseCurlyDoubleQuote":
                    return ret.wordSuccess(p, search[0 .. 21]);
                default:
                    break;
            }
            goto case;
        case 20:
            switch (search[0 .. 20]) {
                case "ReverseUpEquilibrium": case "OpenCurlyDoubleQuote": case "NotSquareSubsetEqual":
                case "NotLeftTriangleEqual": case "NotGreaterSlantEqual": case "NotDoubleVerticalBar":
                case "NestedGreaterGreater": case "EmptyVerySmallSquare": case "DoubleLongRightArrow":
                case "DoubleLeftRightArrow": case "CapitalDifferentialD":
                    return ret.wordSuccess(p, search[0 .. 20]);
                default:
                    break;
            }
            goto case;
        case 19:
            switch (search[0 .. 19]) {
                case "leftrightsquigarrow": case "SquareSupersetEqual": case "RightArrowLeftArrow":
                case "NotRightTriangleBar": case "NotGreaterFullEqual": case "NegativeMediumSpace":
                case "LeftArrowRightArrow": case "DownLeftRightVector": case "DoubleLongLeftArrow":
                    return ret.wordSuccess(p, search[0 .. 19]);
                default:
                    break;
            }
            goto case;
        case 18:
            switch (search[0 .. 18]) {
                case "longleftrightarrow": case "blacktriangleright": case "SucceedsSlantEqual":
                case "SquareIntersection": case "RightTriangleEqual": case "RightDownVectorBar":
                case "RightDownTeeVector": case "RightDoubleBracket": case "ReverseEquilibrium":
                case "PrecedesSlantEqual": case "NotLeftTriangleBar": case "NegativeThickSpace":
                case "Longleftrightarrow": case "LongLeftRightArrow": case "DownRightVectorBar":
                case "DownRightTeeVector":
                    return ret.wordSuccess(p, search[0 .. 18]);
                default:
                    break;
            }
            goto case;
        case 17:
            switch (search[0 .. 17]) {
                case "twoheadrightarrow": case "rightleftharpoons": case "leftrightharpoons": case "blacktriangleleft":
                case "blacktriangledown": case "VerticalSeparator": case "SquareSubsetEqual": case "RightUpDownVector":
                case "RightAngleBracket": case "NotTildeFullEqual": case "NotSquareSuperset": case "NotReverseElement":
                case "NotNestedLessLess": case "NotLessSlantEqual": case "NotGreaterGreater": case "NegativeThinSpace":
                case "LeftTriangleEqual": case "LeftDownVectorBar": case "LeftDownTeeVector": case "LeftDoubleBracket":
                case "GreaterSlantEqual": case "FilledSmallSquare": case "DownLeftVectorBar": case "DownLeftTeeVector":
                case "DoubleVerticalBar": case "DoubleUpDownArrow":
                    return ret.wordSuccess(p, search[0 .. 17]);
                default:
                    break;
            }
            goto case;
        case 16:
            switch (search[0 .. 16]) {
                case "vartriangleright": case "twoheadleftarrow": case "rightrightarrows": case "rightharpoondown":
                case "ntrianglerighteq": case "downharpoonright": case "circlearrowright": case "UpArrowDownArrow":
                case "UnderParenthesis": case "RightUpVectorBar": case "RightUpTeeVector": case "RightTriangleBar":
                case "NotSupersetEqual": case "NotSucceedsTilde": case "NotSucceedsEqual": case "NotRightTriangle":
                case "NotPrecedesEqual": case "NonBreakingSpace": case "LessEqualGreater": case "LeftUpDownVector":
                case "LeftAngleBracket": case "GreaterFullEqual": case "GreaterEqualLess": case "EmptySmallSquare":
                case "DownArrowUpArrow": case "DoubleRightArrow": case "DiacriticalTilde": case "DiacriticalGrave":
                case "DiacriticalAcute":
                    return ret.wordSuccess(p, search[0 .. 16]);
                default:
                    break;
            }
            goto case;
        case 15:
            switch (search[0 .. 15]) {
                case "vartriangleleft": case "trianglerighteq": case "straightepsilon": case "rightthreetimes":
                case "rightsquigarrow": case "rightleftarrows": case "ntrianglelefteq": case "nleftrightarrow":
                case "nLeftrightarrow": case "leftrightarrows": case "leftharpoondown": case "downharpoonleft":
                case "curvearrowright": case "circlearrowleft": case "bigtriangledown": case "UpperRightArrow":
                case "ShortRightArrow": case "RightDownVector": case "OverParenthesis": case "NotSquareSubset":
                case "NotLeftTriangle": case "NotHumpDownHump": case "NotGreaterTilde": case "NotGreaterEqual":
                case "LowerRightArrow": case "LeftUpVectorBar": case "LeftUpTeeVector": case "LeftTriangleBar":
                case "LeftRightVector": case "DownRightVector": case "DoubleLeftArrow": case "DoubleDownArrow":
                case "ContourIntegral": case "CloseCurlyQuote":
                    return ret.wordSuccess(p, search[0 .. 15]);
                default:
                    break;
            }
            goto case;
        case 14:
            switch (search[0 .. 14]) {
                case "upharpoonright": case "trianglelefteq": case "rightharpoonup": case "rightarrowtail":
                case "ntriangleright": case "nshortparallel": case "looparrowright": case "longrightarrow":
                case "leftthreetimes": case "leftrightarrow": case "leftleftarrows": case "hookrightarrow":
                case "downdownarrows": case "doublebarwedge": case "curvearrowleft": case "ZeroWidthSpace":
                case "UpperLeftArrow": case "TildeFullEqual": case "SquareSuperset": case "ShortLeftArrow":
                case "ShortDownArrow": case "RightVectorBar": case "RightTeeVector": case "ReverseElement":
                case "OpenCurlyQuote": case "NotVerticalBar": case "NotSubsetEqual": case "NotLessGreater":
                case "NotGreaterLess": case "NestedLessLess": case "LowerLeftArrow": case "Longrightarrow":
                case "LongRightArrow": case "LessSlantEqual": case "Leftrightarrow": case "LeftRightArrow":
                case "LeftDownVector": case "InvisibleTimes": case "InvisibleComma": case "HorizontalLine":
                case "GreaterGreater": case "DownLeftVector": case "DoubleRightTee": case "DiacriticalDot":
                    return ret.wordSuccess(p, search[0 .. 14]);
                default:
                    break;
            }
            goto case;
        case 13:
            switch (search[0 .. 13]) {
                case "varsupsetneqq": case "varsubsetneqq": case "upharpoonleft": case "triangleright":
                case "smallsetminus": case "shortparallel": case "ntriangleleft": case "measuredangle":
                case "looparrowleft": case "longleftarrow": case "leftharpoonup": case "leftarrowtail":
                case "hookleftarrow": case "fallingdotseq": case "divideontimes": case "blacktriangle":
                case "bigtriangleup": case "VeryThinSpace": case "VerticalTilde": case "UpEquilibrium":
                case "SupersetEqual": case "SucceedsTilde": case "SucceedsEqual": case "RightUpVector":
                case "RightTriangle": case "RightTeeArrow": case "RightArrowBar": case "PrecedesTilde":
                case "PrecedesEqual": case "Poincareplane": case "NotTildeTilde": case "NotTildeEqual":
                case "NotEqualTilde": case "Longleftarrow": case "LongLeftArrow": case "LessFullEqual":
                case "LeftVectorBar": case "LeftTeeVector": case "DoubleUpArrow": case "DoubleLeftTee":
                case "DifferentialD": case "ApplyFunction":
                    return ret.wordSuccess(p, search[0 .. 13]);
                default:
                    break;
            }
            goto case;
        case 12:
            switch (search[0 .. 12]) {
                case "varsupsetneq": case "varsubsetneq": case "triangleleft": case "triangledown": case "risingdotseq":
                case "exponentiale": case "blacklozenge": case "VerticalLine": case "UnderBracket": case "SquareSubset":
                case "ShortUpArrow": case "RoundImplies": case "RightCeiling": case "Proportional": case "NotLessTilde":
                case "NotLessEqual": case "NotHumpEqual": case "NotCongruent": case "LeftUpVector": case "LeftTriangle":
                case "LeftTeeArrow": case "LeftArrowBar": case "Intersection": case "HumpDownHump": case "HilbertSpace":
                case "GreaterTilde": case "GreaterEqual": case "ExponentialE": case "DownTeeArrow": case "DownArrowBar":
                    return ret.wordSuccess(p, search[0 .. 12]);
                default:
                    break;
            }
            goto case;
        case 11:
            switch (search[0 .. 11]) {
                case "updownarrow": case "thickapprox": case "succnapprox": case "succcurlyeq": case "straightphi":
                case "quaternions": case "precnapprox": case "preccurlyeq": case "nrightarrow": case "nRightarrow":
                case "expectation": case "eqslantless": case "diamondsuit": case "curlyeqsucc": case "curlyeqprec":
                case "circleddash": case "circledcirc": case "blacksquare": case "backepsilon": case "VerticalBar":
                case "Updownarrow": case "UpDownArrow": case "SubsetEqual": case "SquareUnion": case "SmallCircle":
                case "RuleDelayed": case "Rrightarrow": case "RightVector": case "OverBracket": case "NotSuperset":
                case "NotSucceeds": case "NotPrecedes": case "NotLessLess": case "MediumSpace": case "LessGreater":
                case "LeftCeiling": case "GreaterLess": case "Equilibrium": case "CircleTimes": case "CircleMinus":
                    return ret.wordSuccess(p, search[0 .. 11]);
                default:
                    break;
            }
            goto case;
        case 10:
            switch (search[0 .. 10]) {
                case "varnothing": case "varepsilon": case "upuparrows": case "supsetneqq": case "succapprox":
                case "subsetneqq": case "sqsupseteq": case "sqsubseteq": case "rmoustache": case "rightarrow":
                case "precapprox": case "nsupseteqq": case "nsubseteqq": case "nleftarrow": case "nLeftarrow":
                case "mapstoleft": case "mapstodown": case "longmapsto": case "lmoustache": case "lesseqqgtr":
                case "lessapprox": case "gtreqqless": case "eqslantgtr": case "curlywedge": case "complement":
                case "circledast": case "UpTeeArrow": case "UpArrowBar": case "UnderBrace": case "TildeTilde":
                case "TildeEqual": case "ThickSpace": case "Rightarrow": case "RightFloor": case "RightArrow":
                case "Proportion": case "NotGreater": case "NotElement": case "Lleftarrow": case "LeftVector":
                case "Laplacetrf": case "ImaginaryI": case "Fouriertrf": case "EqualTilde": case "CirclePlus":
                case "Bernoullis":
                    return ret.wordSuccess(p, search[0 .. 10]);
                default:
                    break;
            }
            goto case;
        case 9:
            switch (search[0 .. 9]) {
                case "varpropto": case "triangleq": case "therefore": case "supsetneq": case "supseteqq":
                case "subsetneq": case "subseteqq": case "spadesuit": case "rationals": case "pitchfork":
                case "nsupseteq": case "nsubseteq": case "nshortmid": case "nparallel": case "nleqslant":
                case "ngeqslant": case "lvertneqq": case "lesseqgtr": case "leftarrow": case "heartsuit":
                case "gvertneqq": case "gtreqless": case "gtrapprox": case "downarrow": case "dotsquare":
                case "complexes": case "checkmark": case "centerdot": case "bigotimes": case "backsimeq":
                case "backprime": case "UnionPlus": case "TripleDot": case "ThinSpace": case "Therefore":
                case "PlusMinus": case "OverBrace": case "NotSubset": case "NotExists": case "NotCupCap":
                case "MinusPlus": case "Mellintrf": case "LessTilde": case "Leftarrow": case "LeftFloor":
                case "LeftArrow": case "HumpEqual": case "Downarrow": case "DownBreve": case "DownArrow":
                case "DoubleDot": case "Coproduct": case "Congruent": case "CircleDot": case "CenterDot":
                case "Backslash":
                    return ret.wordSuccess(p, search[0 .. 9]);
                default:
                    break;
            }
            goto case;
        case 8:
            switch (search[0 .. 8]) {
                case "vartheta": case "varsigma": case "varkappa": case "urcorner": case "ulcorner": case "trpezium":
                case "triminus": case "triangle": case "timesbar": case "thicksim": case "thetasym": case "supseteq":
                case "succnsim": case "succneqq": case "subseteq": case "sqsupset": case "sqsubset": case "smeparsl":
                case "shortmid": case "setminus": case "scpolint": case "rtriltri": case "rppolint": case "realpart":
                case "raemptyv": case "profsurf": case "profline": case "profalar": case "precnsim": case "precneqq":
                case "pointint": case "plusacir": case "parallel": case "otimesas": case "notindot": case "ncongdot":
                case "naturals": case "multimap": case "mapstoup": case "lurdshar": case "lrcorner": case "lnapprox":
                case "llcorner": case "lesdotor": case "leqslant": case "ldrushar": case "laemptyv": case "intlarhk":
                case "intercal": case "integers": case "infintie": case "imagpart": case "imagline": case "hkswarow":
                case "hksearow": case "gnapprox": case "gesdotol": case "geqslant": case "fpartint": case "eqvparsl":
                case "emptyset": case "elinters": case "dzigrarr": case "drbkarow": case "dotminus": case "doteqdot":
                case "cwconint": case "curlyvee": case "cupbrcap": case "clubsuit": case "cirfnint": case "circledS":
                case "circledR": case "capbrcup": case "bsolhsub": case "boxtimes": case "boxminus": case "bigwedge":
                case "biguplus": case "bigsqcup": case "bigoplus": case "bbrktbrk": case "barwedge": case "backcong":
                case "awconint": case "approxeq": case "angrtvbd": case "angmsdah": case "angmsdag": case "angmsdaf":
                case "angmsdae": case "angmsdad": case "angmsdac": case "angmsdab": case "angmsdaa": case "andslope":
                case "UnderBar": case "Uarrocir": case "Superset": case "SuchThat": case "Succeeds": case "RightTee":
                case "Precedes": case "PartialD": case "NotTilde": case "NotEqual": case "LessLess": case "Integral":
                case "DotEqual": case "DDotrahd":
                    return ret.wordSuccess(p, search[0 .. 8]);
                default:
                    break;
            }
            goto case;
        case 7:
            switch (search[0 .. 7]) {
                case "zigrarr": case "vzigzag": case "uwangle": case "upsilon": case "uparrow": case "tritime":
                case "triplus": case "topfork": case "swarrow": case "supplus": case "supmult": case "suplarr":
                case "suphsub": case "suphsol": case "supedot": case "supdsub": case "succsim": case "subrarr":
                case "subplus": case "submult": case "subedot": case "simrarr": case "simplus": case "searrow":
                case "ruluhar": case "rotimes": case "realine": case "rdldhar": case "rbrkslu": case "rbrksld":
                case "rarrsim": case "rarrbfs": case "questeq": case "quatint": case "precsim": case "plustwo":
                case "plussim": case "pluscir": case "planckh": case "pertenk": case "orslope": case "orderof":
                case "omicron": case "olcross": case "nwarrow": case "nvrtrie": case "nvltrie": case "nvinfin":
                case "nsupset": case "nsucceq": case "nsubset": case "nsqsupe": case "nsqsube": case "npreceq":
                case "npolint": case "notnivc": case "notnivb": case "notniva": case "notinvc": case "notinvb":
                case "notinva": case "nexists": case "nearrow": case "natural": case "napprox": case "minusdu":
                case "maltese": case "luruhar": case "ltquest": case "lozenge": case "lotimes": case "lesssim":
                case "lessgtr": case "lessdot": case "lesdoto": case "ldrdhar": case "lbrkslu": case "lbrksld":
                case "larrsim": case "larrbfs": case "isindot": case "intprod": case "harrcir": case "gtrless":
                case "gtquest": case "gesdoto": case "equivDD": case "eqcolon": case "epsilon": case "dwangle":
                case "dotplus": case "digamma": case "diamond": case "demptyv": case "ddotseq": case "ddagger":
                case "dbkarow": case "curarrm": case "cularrp": case "cudarrr": case "cudarrl": case "congdot":
                case "coloneq": case "cirscir": case "cemptyv": case "ccupssm": case "boxplus": case "bnequiv":
                case "bigstar": case "bigodot": case "bigcirc": case "between": case "bemptyv": case "because":
                case "backsim": case "asympeq": case "angzarr": case "angrtvb": case "alefsym": case "Upsilon":
                case "Uparrow": case "UpArrow": case "Product": case "OverBar": case "Omicron": case "NotLess":
                case "NoBreak": case "NewLine": case "LeftTee": case "Implies": case "Epsilon": case "Element":
                case "DownTee": case "Diamond": case "Cedilla": case "Cconint": case "Cayleys": case "Because":
                    return ret.wordSuccess(p, search[0 .. 7]);
                default:
                    break;
            }
            goto case;
        case 6:
            switch (search[0 .. 6]) {
                case "zeetrf": case "zcaron": case "zacute": case "yacute": case "xwedge": case "xuplus": case "xsqcup":
                case "xotime": case "xoplus": case "wreath": case "weierp": case "wedgeq": case "wedbar": case "vsupne":
                case "vsupnE": case "vsubne": case "vsubnE": case "verbar": case "vellip": case "veebar": case "varrho":
                case "varphi": case "vangrt": case "utilde": case "urcrop": case "urcorn": case "ulcrop": case "ulcorn":
                case "ugrave": case "ufisht": case "udblac": case "ubreve": case "uacute": case "tstrok": case "tridot":
                case "tprime": case "topcir": case "topbot": case "timesd": case "timesb": case "thksim": case "thinsp":
                case "thetav": case "there4": case "telrec": case "tcedil": case "tcaron": case "target": case "swnwar":
                case "swarhk": case "supsup": case "supsub": case "supsim": case "supset": case "supdot": case "succeq":
                case "subsup": case "subsub": case "subsim": case "subset": case "subdot": case "sstarf": case "ssmile":
                case "ssetmn": case "squarf": case "square": case "sqsupe": case "sqsube": case "sqcups": case "sqcaps":
                case "spades": case "solbar": case "softcy": case "smashp": case "simdot": case "sigmav": case "sigmaf":
                case "shchcy": case "sfrown": case "seswar": case "searhk": case "scnsim": case "scedil": case "scaron":
                case "sacute": case "rtimes": case "rthree": case "rsquor": case "rsaquo": case "rpargt": case "roplus":
                case "rmoust": case "rharul": case "rfloor": case "rfisht": case "rdquor": case "rcedil": case "rcaron":
                case "rbrack": case "rbrace": case "ratail": case "rarrtl": case "rarrpl": case "rarrlp": case "rarrhk":
                case "rarrfs": case "rarrap": case "rangle": case "racute": case "rAtail": case "qprime": case "puncsp":
                case "prurel": case "propto": case "prnsim": case "primes": case "preceq": case "plusmn": case "plusdu":
                case "plusdo": case "plankv": case "planck": case "phmmat": case "permil": case "period": case "percnt":
                case "parsim": case "otimes": case "otilde": case "oslash": case "origof": case "ominus": case "ograve":
                case "odsold": case "odblac": case "oacute": case "nwnear": case "nwarhk": case "nvrArr": case "nvlArr":
                case "nvdash": case "nvHarr": case "nvDash": case "numero": case "ntilde": case "nsimeq": case "nsccue":
                case "nrtrie": case "nrarrw": case "nrarrc": case "nprcue": case "nparsl": case "notinE": case "nltrie":
                case "nexist": case "nesear": case "nequiv": case "nearhk": case "ncedil": case "ncaron": case "nbumpe":
                case "nacute": case "nVdash": case "nVDash": case "mstpos": case "models": case "mnplus": case "minusd":
                case "minusb": case "middot": case "midcir": case "midast": case "mcomma": case "marker": case "mapsto":
                case "ltrPar": case "ltlarr": case "ltimes": case "lthree": case "lstrok": case "lsquor": case "lsaquo":
                case "lrhard": case "lparlt": case "lowbar": case "lowast": case "loplus": case "lmoust": case "lmidot":
                case "llhard": case "lharul": case "lfloor": case "lfisht": case "lesges": case "lesdot": case "ldquor":
                case "lcedil": case "lcaron": case "lbrack": case "lbrace": case "latail": case "larrtl": case "larrpl":
                case "larrlp": case "larrhk": case "larrfs": case "langle": case "lambda": case "lagran": case "lacute":
                case "lAtail": case "kgreen": case "kcedil": case "kappav": case "jsercy": case "itilde": case "isinsv":
                case "iquest": case "intcal": case "inodot": case "incare": case "iinfin": case "iiiint": case "igrave":
                case "iacute": case "hyphen": case "hybull": case "hstrok": case "hslash": case "horbar": case "homtht":
                case "hercon": case "hellip": case "hearts": case "hardcy": case "hamilt": case "hairsp": case "gtrsim":
                case "gtrdot": case "gtrarr": case "gtlPar": case "gesles": case "gesdot": case "gbreve": case "gammad":
                case "gacute": case "frac78": case "frac58": case "frac56": case "frac45": case "frac38": case "frac35":
                case "frac34": case "frac25": case "frac23": case "frac18": case "frac16": case "frac15": case "frac14":
                case "frac13": case "frac12": case "forall": case "ffllig": case "ffilig": case "female": case "equest":
                case "equals": case "eqcirc": case "eparsl": case "emsp14": case "emsp13": case "emptyv": case "elsdot":
                case "egsdot": case "egrave": case "ecolon": case "ecaron": case "easter": case "eacute": case "dstrok":
                case "drcrop": case "drcorn": case "dollar": case "dlcrop": case "dlcorn": case "divonx": case "divide":
                case "dfisht": case "dcaron": case "daleth": case "dagger": case "cylcty": case "curren": case "curarr":
                case "cupdot": case "cupcup": case "cupcap": case "cularr": case "copysr": case "coprod": case "conint":
                case "compfn": case "commat": case "colone": case "cirmid": case "circeq": case "ccedil": case "ccaron":
                case "capdot": case "capcup": case "capcap": case "capand": case "cacute": case "bumpeq": case "bullet":
                case "brvbar": case "bprime": case "boxbox": case "bowtie": case "bottom": case "bkarow": case "bigvee":
                case "bigcup": case "bigcap": case "bernou": case "becaus": case "barwed": case "barvee": case "atilde":
                case "approx": case "apacir": case "angsph": case "angmsd": case "andand": case "agrave": case "abreve":
                case "aacute": case "Zcaron": case "Zacute": case "Yacute": case "Vvdash": case "Verbar": case "Vdashl":
                case "Utilde": case "Ugrave": case "Udblac": case "Ubreve": case "Uacute": case "Tstrok": case "Tcedil":
                case "Tcaron": case "Supset": case "Subset": case "Square": case "Scedil": case "Scaron": case "Sacute":
                case "SOFTcy": case "SHCHcy": case "Rcedil": case "Rcaron": case "Rarrtl": case "Racute": case "Otimes":
                case "Otilde": case "Oslash": case "Ograve": case "Odblac": case "Oacute": case "Ntilde": case "Ncedil":
                case "Ncaron": case "Nacute": case "Lstrok": case "Lmidot": case "Lcedil": case "Lcaron": case "Lambda":
                case "Lacute": case "Kcedil": case "Jsercy": case "Itilde": case "Igrave": case "Iacute": case "Hstrok":
                case "HARDcy": case "Gcedil": case "Gbreve": case "Gammad": case "ForAll": case "Exists": case "Egrave":
                case "Ecaron": case "Eacute": case "Dstrok": case "DotDot": case "Dcaron": case "Dagger": case "CupCap":
                case "Conint": case "Colone": case "Ccedil": case "Ccaron": case "Cacute": case "Bumpeq": case "Barwed":
                case "Atilde": case "Assign": case "Agrave": case "Abreve": case "Aacute":
                    return ret.wordSuccess(p, search[0 .. 6]);
                default:
                    break;
            }
            goto case;
        case 5:
            switch (search[0 .. 5]) {
                case "ycirc": case "xutri": case "xrarr": case "xrArr": case "xodot": case "xlarr": case "xlArr":
                case "xharr": case "xhArr": case "xdtri": case "xcirc": case "wedge": case "wcirc": case "vrtri":
                case "vprop": case "vnsup": case "vnsub": case "vltri": case "veeeq": case "vdash": case "varpi":
                case "vDash": case "vBarv": case "uuarr": case "utrif": case "utdot": case "urtri": case "uring":
                case "upsih": case "uplus": case "uogon": case "umacr": case "ultri": case "uhblk": case "uharr":
                case "uharl": case "udhar": case "udarr": case "ucirc": case "ubrcy": case "twixt": case "tshcy":
                case "trisb": case "trade": case "times": case "tilde": case "thorn": case "thkap": case "theta":
                case "szlig": case "swarr": case "swArr": case "supne": case "supnE": case "subne": case "subnE":
                case "strns": case "starf": case "srarr": case "sqsup": case "sqsub": case "sqcup": case "sqcap":
                case "smtes": case "smile": case "slarr": case "simne": case "simlE": case "simgE": case "simeq":
                case "sigma": case "sharp": case "setmn": case "searr": case "seArr": case "sdote": case "sdotb":
                case "scsim": case "scnap": case "scirc": case "sccue": case "sbquo": case "rtrif": case "rtrie":
                case "rsquo": case "rrarr": case "ropar": case "robrk": case "roarr": case "roang": case "rnmid":
                case "rlhar": case "rlarr": case "rharu": case "rhard": case "reals": case "rdquo": case "rceil":
                case "rbrke": case "rbbrk": case "rbarr": case "ratio": case "rarrw": case "rarrc": case "rarrb":
                case "raquo": case "range": case "rangd": case "radic": case "rBarr": case "rAarr": case "quest":
                case "prsim": case "prnap": case "prime": case "prcue": case "pound": case "pluse": case "plusb":
                case "phone": case "parsl": case "ovbar": case "order": case "orarr": case "oplus": case "operp":
                case "omega": case "omacr": case "oline": case "olcir": case "olarr": case "ohbar": case "ofcir":
                case "oelig": case "odash": case "ocirc": case "nwarr": case "nwArr": case "nvsim": case "numsp":
                case "nsupe": case "nsupE": case "nsucc": case "nsube": case "nsubE": case "nspar": case "nsmid":
                case "nsime": case "nrtri": case "nrarr": case "nrArr": case "nprec": case "npart": case "notni":
                case "notin": case "nltri": case "nlsim": case "nless": case "nleqq": case "nlarr": case "nlArr":
                case "nhpar": case "nharr": case "nhArr": case "ngsim": case "ngeqq": case "nesim": case "nedot":
                case "nearr": case "neArr": case "ndash": case "ncong": case "nbump": case "natur": case "napos":
                case "napid": case "nabla": case "mumap": case "minus": case "micro": case "mdash": case "mDDot":
                case "ltrif": case "ltrie": case "ltdot": case "ltcir": case "lsquo": case "lsimg": case "lsime":
                case "lrtri": case "lrhar": case "lrarr": case "lopar": case "lobrk": case "loarr": case "loang":
                case "lnsim": case "lneqq": case "lltri": case "llarr": case "lhblk": case "lharu": case "lhard":
                case "lescc": case "ldquo": case "lceil": case "lbrke": case "lbbrk": case "lbarr": case "lates":
                case "larrb": case "laquo": case "langd": case "lBarr": case "lAarr": case "kappa": case "jukcy":
                case "jmath": case "jcirc": case "iukcy": case "isinv": case "isins": case "isinE": case "iprod":
                case "iogon": case "infin": case "imped": case "imath": case "image": case "imacr": case "ijlig":
                case "iiota": case "iiint": case "iexcl": case "icirc": case "hoarr": case "hcirc": case "harrw":
                case "gtdot": case "gtcir": case "gsiml": case "gsime": case "grave": case "gnsim": case "gneqq":
                case "gimel": case "gescc": case "gcirc": case "gamma": case "frown": case "frasl": case "forkv":
                case "fltns": case "fllig": case "fjlig": case "filig": case "fflig": case "exist": case "esdot":
                case "erarr": case "erDot": case "equiv": case "eqsim": case "epsiv": case "eplus": case "eogon":
                case "empty": case "emacr": case "efDot": case "ecirc": case "eDDot": case "duhar": case "duarr":
                case "dtrif": case "dtdot": case "doteq": case "disin": case "diams": case "dharr": case "dharl":
                case "delta": case "ddarr": case "dblac": case "dashv": case "cwint": case "cuwed": case "cuvee":
                case "cupor": case "cuesc": case "cuepr": case "ctdot": case "csupe": case "csube": case "cross":
                case "crarr": case "comma": case "colon": case "clubs": case "check": case "cedil": case "ccups":
                case "ccirc": case "ccaps": case "caron": case "caret": case "bumpe": case "bumpE": case "bsolb":
                case "bsime": case "bsemi": case "breve": case "boxvr": case "boxvl": case "boxvh": case "boxvR":
                case "boxvL": case "boxvH": case "boxur": case "boxul": case "boxuR": case "boxuL": case "boxhu":
                case "boxhd": case "boxhU": case "boxhD": case "boxdr": case "boxdl": case "boxdR": case "boxdL":
                case "boxVr": case "boxVl": case "boxVh": case "boxVR": case "boxVL": case "boxVH": case "boxUr":
                case "boxUl": case "boxUR": case "boxUL": case "boxHu": case "boxHd": case "boxHU": case "boxHD":
                case "boxDr": case "boxDl": case "boxDR": case "boxDL": case "block": case "blk34": case "blk14":
                case "blk12": case "blank": case "bepsi": case "bdquo": case "bcong": case "awint": case "asymp":
                case "aring": case "aogon": case "angst": case "angrt": case "angle": case "amalg": case "amacr":
                case "alpha": case "aleph": case "aelig": case "acute": case "acirc": case "Ycirc": case "Wedge":
                case "Wcirc": case "Vdash": case "VDash": case "Uring": case "UpTee": case "Uogon": case "Union":
                case "Umacr": case "Ucirc": case "Ubrcy": case "Tilde": case "Theta": case "TSHcy": case "TRADE":
                case "THORN": case "Sigma": case "Scirc": case "RBarr": case "Prime": case "Omega": case "Omacr":
                case "Ocirc": case "OElig": case "Kappa": case "Jukcy": case "Jcirc": case "Iukcy": case "Iogon":
                case "Imacr": case "Icirc": case "IJlig": case "Hcirc": case "Hacek": case "Gcirc": case "Gamma":
                case "Equal": case "Eogon": case "Emacr": case "Ecirc": case "Delta": case "Dashv": case "Cross":
                case "Colon": case "Ccirc": case "Breve": case "Aring": case "Aogon": case "Amacr": case "Alpha":
                case "Acirc": case "AElig":
                    return ret.wordSuccess(p, search[0 .. 5]);
                default:
                    break;
            }
            goto case;
        case 4:
            switch (search[0 .. 4]) {
                case "zwnj": case "zscr": case "zopf": case "zhcy": case "zeta": case "zdot": case "yuml": case "yucy":
                case "yscr": case "yopf": case "yicy": case "yacy": case "xvee": case "xscr": case "xopf": case "xnis":
                case "xmap": case "xcup": case "xcap": case "wscr": case "wopf": case "vscr": case "vopf": case "vert":
                case "varr": case "vBar": case "vArr": case "uuml": case "utri": case "uscr": case "upsi": case "uopf":
                case "uarr": case "uHar": case "uArr": case "tscy": case "tscr": case "trie": case "tosa": case "topf":
                case "toea": case "tint": case "tdot": case "tbrk": case "supe": case "supE": case "sup3": case "sup2":
                case "sup1": case "sung": case "succ": case "sube": case "subE": case "star": case "sscr": case "squf":
                case "spar": case "sopf": case "solb": case "smte": case "smid": case "siml": case "simg": case "sime":
                case "shcy": case "sext": case "semi": case "sect": case "sdot": case "scnE": case "scap": case "rtri":
                case "rsqb": case "rscr": case "rpar": case "ropf": case "ring": case "rhov": case "rect": case "real":
                case "rdsh": case "rdca": case "rcub": case "rarr": case "rang": case "race": case "rHar": case "rArr":
                case "quot": case "qscr": case "qopf": case "qint": case "pscr": case "prop": case "prod": case "prnE":
                case "prec": case "prap": case "popf": case "plus": case "phiv": case "perp": case "part": case "para":
                case "ouml": case "osol": case "oscr": case "oror": case "ordm": case "ordf": case "opar": case "oopf":
                case "omid": case "oint": case "ogon": case "odot": case "odiv": case "ocir": case "oast": case "nvlt":
                case "nvle": case "nvgt": case "nvge": case "nvap": case "ntlg": case "ntgl": case "nsup": case "nsub":
                case "nsim": case "nscr": case "nsce": case "npre": case "npar": case "nopf": case "nmid": case "nles":
                case "nleq": case "nldr": case "njcy": case "nisd": case "ngtr": case "nges": case "ngeq": case "ncup":
                case "ncap": case "nbsp": case "napE": case "nang": case "nLtv": case "nGtv": case "mscr": case "mopf":
                case "mldr": case "mlcp": case "malt": case "male": case "macr": case "lvnE": case "ltri": case "ltcc":
                case "lsqb": case "lsim": case "lscr": case "lpar": case "lozf": case "lopf": case "lneq": case "lnap":
                case "ljcy": case "lesg": case "leqq": case "ldsh": case "ldca": case "lcub": case "late": case "larr":
                case "lang": case "lHar": case "lArr": case "kscr": case "kopf": case "kjcy": case "khcy": case "jscr":
                case "jopf": case "iuml": case "isin": case "iscr": case "iota": case "iopf": case "iocy": case "imof":
                case "iecy": case "hscr": case "hopf": case "hbar": case "harr": case "half": case "hArr": case "gvnE":
                case "gtcc": case "gsim": case "gscr": case "gopf": case "gneq": case "gnap": case "gjcy": case "gesl":
                case "geqq": case "gdot": case "fscr": case "fork": case "fopf": case "fnof": case "flat": case "excl":
                case "euro": case "euml": case "esim": case "escr": case "epsi": case "epar": case "eopf": case "ensp":
                case "emsp": case "edot": case "ecir": case "eDot": case "dzcy": case "dtri": case "dsol": case "dscy":
                case "dscr": case "dopf": case "djcy": case "diam": case "dash": case "darr": case "dHar": case "dArr":
                case "cups": case "csup": case "csub": case "cscr": case "copy": case "copf": case "cong": case "comp":
                case "cire": case "circ": case "cirE": case "chcy": case "cent": case "cdot": case "caps": case "bump":
                case "bull": case "bsol": case "bsim": case "bscr": case "boxv": case "boxh": case "boxV": case "boxH":
                case "bopf": case "bnot": case "beth": case "beta": case "bbrk": case "bNot": case "auml": case "ascr":
                case "apos": case "apid": case "aopf": case "ange": case "andv": case "andd": case "Zscr": case "Zopf":
                case "Zeta": case "Zdot": case "ZHcy": case "Yuml": case "Yscr": case "Yopf": case "YUcy": case "YIcy":
                case "YAcy": case "Xscr": case "Xopf": case "Wscr": case "Wopf": case "Vscr": case "Vopf": case "Vert":
                case "Vbar": case "Uuml": case "Uscr": case "Upsi": case "Uopf": case "Uarr": case "Tscr": case "Topf":
                case "TScy": case "Star": case "Sscr": case "Sqrt": case "Sopf": case "SHcy": case "Rscr": case "Ropf":
                case "Rarr": case "Rang": case "Qscr": case "Qopf": case "QUOT": case "Pscr": case "Popf": case "Ouml":
                case "Oscr": case "Oopf": case "Nscr": case "Nopf": case "NJcy": case "Mscr": case "Mopf": case "Lscr":
                case "Lopf": case "Larr": case "Lang": case "LJcy": case "Kscr": case "Kopf": case "KJcy": case "KHcy":
                case "Jscr": case "Jopf": case "Iuml": case "Iscr": case "Iota": case "Iopf": case "Idot": case "IOcy":
                case "IEcy": case "Hscr": case "Hopf": case "Gscr": case "Gopf": case "Gdot": case "GJcy": case "Fscr":
                case "Fopf": case "Euml": case "Esim": case "Escr": case "Eopf": case "Edot": case "Dscr": case "Dopf":
                case "Darr": case "DZcy": case "DScy": case "DJcy": case "Cscr": case "Copf": case "Cdot": case "COPY":
                case "CHcy": case "Bscr": case "Bopf": case "Beta": case "Barv": case "Auml": case "Ascr": case "Aopf":
                    return ret.wordSuccess(p, search[0 .. 4]);
                default:
                    break;
            }
            goto case;
        case 3:
            switch (search[0 .. 3]) {
                case "zwj": case "zfr": case "zcy": case "yfr": case "yen": case "ycy": case "xfr": case "wfr":
                case "vfr": case "vee": case "vcy": case "uml": case "ufr": case "ucy": case "top": case "tfr":
                case "tcy": case "tau": case "sup": case "sum": case "sub": case "squ": case "sol": case "smt":
                case "sim": case "shy": case "sfr": case "scy": case "sce": case "scE": case "rsh": case "rlm":
                case "rho": case "rfr": case "reg": case "rcy": case "qfr": case "psi": case "pre": case "prE":
                case "piv": case "phi": case "pfr": case "pcy": case "par": case "orv": case "ord": case "olt":
                case "ohm": case "ogt": case "ofr": case "ocy": case "num": case "nsc": case "npr": case "not":
                case "nlt": case "nle": case "nlE": case "niv": case "nis": case "ngt": case "nge": case "ngE":
                case "nfr": case "ncy": case "nap": case "nLt": case "nLl": case "nGt": case "nGg": case "mid":
                case "mho": case "mfr": case "mcy": case "map": case "lsh": case "lrm": case "loz": case "lne":
                case "lnE": case "lgE": case "lfr": case "les": case "leq": case "leg": case "lcy": case "lat":
                case "lap": case "lEg": case "kfr": case "kcy": case "jfr": case "jcy": case "int": case "ifr":
                case "iff": case "icy": case "hfr": case "gne": case "gnE": case "glj": case "gla": case "glE":
                case "ggg": case "gfr": case "ges": case "geq": case "gel": case "gcy": case "gap": case "gEl":
                case "ffr": case "fcy": case "eth": case "eta": case "eng": case "els": case "ell": case "egs":
                case "efr": case "ecy": case "dot": case "div": case "die": case "dfr": case "deg": case "dcy":
                case "cup": case "cir": case "chi": case "cfr": case "cap": case "bot": case "bne": case "bfr":
                case "bcy": case "ast": case "ape": case "apE": case "ang": case "and": case "amp": case "afr":
                case "acy": case "acd": case "acE": case "Zfr": case "Zcy": case "Yfr": case "Ycy": case "Xfr":
                case "Wfr": case "Vfr": case "Vee": case "Vcy": case "Ufr": case "Ucy": case "Tfr": case "Tcy":
                case "Tau": case "Tab": case "Sup": case "Sum": case "Sub": case "Sfr": case "Scy": case "Rsh":
                case "Rho": case "Rfr": case "Rcy": case "REG": case "Qfr": case "Psi": case "Phi": case "Pfr":
                case "Pcy": case "Ofr": case "Ocy": case "Not": case "Nfr": case "Ncy": case "Mfr": case "Mcy":
                case "Map": case "Lsh": case "Lfr": case "Lcy": case "Kfr": case "Kcy": case "Jfr": case "Jcy":
                case "Int": case "Ifr": case "Icy": case "Hfr": case "Hat": case "Gfr": case "Gcy": case "Ffr":
                case "Fcy": case "Eta": case "Efr": case "Ecy": case "ETH": case "ENG": case "Dot": case "Dfr":
                case "Del": case "Dcy": case "Cup": case "Chi": case "Cfr": case "Cap": case "Bfr": case "Bcy":
                case "And": case "Afr": case "Acy": case "AMP":
                    return ret.wordSuccess(p, search[0 .. 3]);
                default:
                    break;
            }
            goto case;
        case 2:
            switch (search[0 .. 2]) {
                case "xi": case "wr": case "wp": case "sc": case "rx": case "pr": case "pm": case "pi": case "or":
                case "oS": case "nu": case "ni": case "ne": case "mu": case "mp": case "lt": case "ll": case "lg":
                case "le": case "lE": case "it": case "in": case "ii": case "ic": case "gt": case "gl": case "gg":
                case "ge": case "gE": case "el": case "eg": case "ee": case "dd": case "ap": case "af": case "ac":
                case "Xi": case "Sc": case "Re": case "Pr": case "Pi": case "Or": case "Nu": case "Mu": case "Lt":
                case "Ll": case "LT": case "Im": case "Gt": case "Gg": case "GT": case "DD":
                    return ret.wordSuccess(p, search[0 .. 2]);
                default:
                    break;
            }
            goto default;
        default:
            return ret.charFail(p, "an HTML5 entity reference");
    }
}
// dfmt on
