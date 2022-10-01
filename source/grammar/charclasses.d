module grammar.charclasses;
import grammar.util;
import pegged.grammar: ParseTree;
public import grammar.htmlent;

//A Unicode punctuation character is an ASCII punctuation character or anything in the general Unicode categories Pc, Pd, Pe, Pf, Pi, Po, or Ps.
@safe pure nothrow
ParseTree UnicodePunctuationChar(ParseTree p) {
    import std.uni;

    auto ret = ParseTree("UnicodePunctuationChar");
    if (p.input.length <= p.end)
        return ret;
    return isPunctuation(p.input[p.end])
        ? ret.charSeccess(p) : ret.charFail(p, "a unicode punctuation character");
}

//An ASCII control character is a character between U+0000–1F (both including) or U+007F.
@safe pure nothrow
ParseTree ControlChar(ParseTree p) {
    // Note: cannot use isControl, as that covers a wider range of characters
    auto ret = ParseTree("ControlChar");
    if (p.input.length <= p.end)
        return ret;
    switch (p.input[p.end]) {
        case '\u0000': .. case '\u001F':
        case '\u007F':
            return ret.charSeccess(p);
        default:
            return ret.charFail(p, "an ASCII control character");
    }
}

// A Unicode whitespace character is any code point in the Unicode Zs general category, or a tab (U+0009), line feed (U+000A), form feed (U+000C), or carriage return (U+000D).
@safe pure nothrow
ParseTree WhitespaceCharacter(ParseTree p) {
    import std.uni;

    auto ret = ParseTree("WhitespaceCharacter");
    if (p.input.length <= p.end)
        return ret;
    return isSpace(p.input[p.end])
        ? ret.charSeccess(p) : ret.charFail(p, "a whitespace character");
}
