/* The below defines the predicate extract_pred(+Sentence, -Predicate) 
   which takes in a simple sentence as a string and coverts it to 
   a Prolog predicate. */

/* Implementation */

% libraries and modules
:- use_module(library(clpfd)). % finite domain reasoning with #=, #>, etc...

% determiners
lex(a, det).
lex(an, det).
lex(another, det).
lex(the, det).

lex(the, definite_det).

lex(if, key_word).

% convert a string of lower case words with no punctuation to atoms
stringList_to_atomList([], []).
stringList_to_atomList([HS|TS], [HA|TA]) :-
    atom_string(HA, HS),
    stringList_to_atomList(TS, TA).

% format sentence and split into a list of words
format_sentence(UnformattedSentence, WordsList) :-
    string_codes(UnformattedSentence, Codes),
    string_codes(",.", Remove),
    subtract(Codes, Remove, Removed),
    string_codes(UpCase, Removed),
    downcase_atom(UpCase, FormattedSentence),
    split_string(FormattedSentence, " ","", WordsAsStrings),
    stringList_to_atomList(WordsAsStrings, WordsList).


% create a list of common nouns

% obtain a list of the variables
var_list([], A, A).

var_list([DET, H|T], A, Vars) :-
    lex(DET, det),
    var_list(T, [H|A], Vars).

var_list([H|T], A, Vars) :-
    \+ lex(H, det),
    var_list(T, A, Vars).

var_list(L, I) :-
    var_list(L, [], Rev),
    length(Rev, I).

var_list1(L, Vars) :-
    var_list(L, [], Rev),
    reverse(Rev, Vars).

% obtain a list of predicate words
pred_list([], A, A).

pred_list([DET, _|T], A, Vars) :-
    lex(DET,det),
    pred_list(T, A, Vars).

pred_list([H|T], A, Vars) :-
    \+ lex(H, det),
    pred_list(T, [H|A], Vars).

pred_list(L, Vars) :-
    pred_list(L, [], Rev), reverse(Rev, Vars).


% extract predicate
extract_pred(String, Pred) :-
    format_sentence(String, Atoms),
    var_list(Atoms, I),
    length(FreeVars, I),
    pred_list(Atoms, FunctorWords),
    atomic_list_concat(FunctorWords, '_', Functor),
    append([Functor], FreeVars, Input),
    Pred =.. Input.

extract_pred1(Atoms, Pred) :-
    var_list(Atoms, I),
    length(FreeVars, I),
    pred_list(Atoms, FunctorWords),
    atomic_list_concat(FunctorWords, '_', Functor),
    append([Functor], FreeVars, Input),
    Pred =.. Input.

% build a conditional
split(L, X, A, B) :-
    N = [X|B],
    append(A, N, L).

conditional(L, Rule) :-
    split(L, if, A, B),
    extract_pred1(A, Head),
    extract_pred1(B, Body),
    term_variables(Body, _),
    Rule =..[:-, Head, Body].


% build conditional try 2


%?- var_list1([a, man, loves, a, woman], X).
%@ X = [man, woman] .

%@ Rule =  (loves(_7990, _7996):-loves(_8050, _8056)) .

%?- functor(X, love, 2), term_variables(X, Y), Y=[H|T].
%@ X = love(H, _6312),
%@ Y = [H, _6312],
%@ T = [_6312].

/* Examples */
%?- extract_pred("A man loves an object.", X), call(X).
%@ X = loves(_13690, _13696) .

%?- extract_pred("A person gives a person an award for a competition.", X).
%@ X = gives_for(_18714, _18720, _18726, _18732) .



%?- term_string(Term, "loves(X, Y, person)", [variable_names(VNames)]).
%@ Term = loves(_2094, _2096, person),
%@ VNames = ['X'=_2094, 'Y'=_2096].




