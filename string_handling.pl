/*******************************************************/ 
/* Module contains predicates for dealing with strings */
/*******************************************************/ 



/* Generic string handling (unrelated to grammar) */ 
%%% parse a string representing sentences where only punctuation is '.'
text_to_sentences(StringText, ListStrings) :-
    split_string(StringText, ".", " ", L),
    reverse(L, [_|T]),
    reverse(T, ListStrings).

%?- text_to_sentences("Vesko is studying. A person will succeed if the person is studying.", X).
%@ X = ["Vesko is studying", "A person will succeed if the person is studying"].


%%% split single string into constituent words
% no commas, only fullstops allowed
string_to_atoms(String, WordsList) :-
    split_string(String, " ",".", WordsAsStrings),
    string_list_to_atoms_list(WordsAsStrings, WordsList).

%?- string_to_atoms("A person loves another person.", X).
%@ X = ['A', person, loves, another, person].



%%% parse text composed of more than one sentence into a list of lists of atoms
% every list is sentence
% commas are removed
text_to_list_of_lists(Text, LoL) :-
    text_to_sentences(Text, LoS),
    text_to_list_of_lists_(LoS, [], R),
    reverse(R, LoL).

text_to_list_of_lists_([], A, A).

text_to_list_of_lists_([H|T], A, R) :-
    remove_char(H, ',', HO),
    string_to_atoms(HO, WL),
    text_to_list_of_lists_(T, [WL|A], R).

%?- text_to_list_of_lists("Vesko is studying. If a person studies, the person succeeds.", X).
%@ X = [['Vesko', is, studying], ['If', a, person, studies, the, person, succeeds]].



%%% remove char from string
remove_char(StrIn, Char, StrOut) :-
    string_chars(StrIn, X),
    delete(X, Char, Y),
    string_chars(StrOut, Y).

%?- remove_char("Guy, please remove the comma after you name.", ',', X).
%@ X = "Guy please remove the comma after you name.".


%%% check if a string contains the character combination :- 
contains_char(CharAsString, String) :-
    string_codes(CharAsString, C),
    string_codes(String, Codes),
    C= [I],
    memberchk(I, Codes).

%?- sub_string("Hell):-HO", _, _, 2,":-").


/* End */ 



/* Specific string handling: Construct string from atomic, conjunction or logical */
string_from_atomic([H|T], String) :-
    append_commas(T, TC),
    append([H], ['('], A),
    append(A, TC, B),
    append(B, [')'], Full),
    atomic_list_concat(Full,'', Concat),
    atom_string(Concat, String).

%?- string_from_atomic([is_, hungry, tom], String).
%@ String = "is_(hungry, tom)".

string_from_conjunction(In, Out) :-
    string_from_conjunction_(In, [], Rev),!,
    reverse(Rev, Pre),
    atomics_to_string(Pre, Out).

string_from_conjunction_([], A, A).

string_from_conjunction_([H, Conj|T], A, R):-
    (Conj = ',';
     Conj = ';'),
    string_from_atomic(H, S),
    string_from_conjunction_(T, [Conj, S|A], R).

string_from_conjunction_([H|T], A, R):-
    string_from_atomic(H, S),
    string_from_conjunction_(T, [S|A], R).

%?- string_from_conjunction([[is_, slow, 'V_2'],',', [is_a, 'V_2', tom]], S).
%@ S = is_(slow, V_2),is_a(V_2, tom).


string_from_conditional(Head, BodyConj, String) :-
    string_from_atomic(Head, StringHead),
    string_from_conjunction(BodyConj, StringBodyConj),
    atomics_to_string([StringHead, ":-",'(',StringBodyConj, ')'], String).

%?- string_from_conditional([ancestor, 'V_1', 'V_2'], [[ancestor, 'V_1', 'V_2'],',', [is_, bit, tom]], S).
%@ S = ancestor(V_1, V_2):-(is_(bit, tom),ancestor(V_1, V_2)).





/* End */
