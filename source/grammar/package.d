module grammar;
import pegged.grammar;
import grammar.charclasses;
import grammar.htmlent;

private immutable g = import("prelim.pegd");
public mixin(pegged.grammar.grammar(g));
