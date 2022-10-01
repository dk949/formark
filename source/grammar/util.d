module grammar.util;

import pegged.grammar: ParseTree;

@safe pure nothrow
ref ParseTree charSeccess(ref return ParseTree ret, ref const ParseTree p) {
    ret.successful = true;
    ret.matches = [p.input[p.end .. p.end + 1]];
    ret.input = p.input;
    ret.begin = p.end;
    ret.end = p.end + 1;
    return ret;
}

@safe pure nothrow
ref ParseTree wordSuccess(ref return ParseTree ret, ref const ParseTree p, string word) {
    ret.successful = true;
    ret.matches = [p.input[p.end .. p.end + word.length]];
    ret.input = p.input;
    ret.begin = p.end;
    ret.end = p.end + word.length;
    return ret;
}

@safe pure nothrow
ref ParseTree charFail(ref return ParseTree ret, ref const ParseTree p, string expected) {
    ret.successful = false;
    ret.matches = [expected];
    ret.input = p.input;
    ret.begin = p.end;
    ret.end = p.end;
    return ret;
}
