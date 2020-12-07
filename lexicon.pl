/* Lexicon */

%%% determiners

lex('A', det).
lex('An', det).
lex('Another', det).
lex('The', det).
lex(a, det).
lex(an, det).
lex(another, det).
lex(the, det).

lex(the, definite_det).
lex('The', definite_det).
lex('A', indefinite_det).
lex('An', indefinite_det).
lex('Another', indefinite_det).
lex(a, indefinite_det).
lex(an, indefinite_det).
lex(another, indefinite_det).


lex(if, logical).
lex(not, logical).
lex(and, logical).
lex(or, logical).
lex(then, logical).
lex(all, logical).
lex(some, logical).
lex('If', logical).
lex('Not', logical).
lex('And', logical).
lex('Or', logical).
lex('Then', logical).
lex('All', logical).
lex('Some', logical).


lex(of, prep).
lex(in, prep).
lex(to, prep).
lex(for, prep).
lex(with, prep).
lex(on, prep).
lex(at, prep).
lex(from, prep).
lex(by, prep).
lex(about, prep).
lex(as, prep).
lex(into, prep).
lex(before, prep).
lex(after, prep).
lex(during, prep).


lex(';', con).
lex(',', con).

lex(more, comp, '#>').
lex(greater, comp, '#>').
lex(less, comp, '#<').
lex(smaller,comp, '#<').
lex(smaller,comp, '#<').
lex(equal,comp, '#=').

%%%
