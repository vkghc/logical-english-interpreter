/*************/
/* Utilities */
/*************/



/* Generic utilities (unrelated to grammar) */

%%% assert a list of statements
assert_list([]).

assert_list([H|T]) :-
    assert(H),
    assert_list(T).



%%% append a ', ' afer every element in a list of atoms
append_commas([], A, A).

append_commas([H|T], A, L) :-
    append_commas(T, [H, ', '|A], L).

append_commas(In, Out) :-
    append_commas(In, [], Rev),
    reverse(Rev, [', '|Out]).

%?- append_empty([a, big, deal], X).
%@ X = [a, ', ', big, ', ', deal].



%%%% make atom lower case
lowercase(Atom, Lower) :-
    atom_codes(Atom, [H|T]),
    to_lower(H, HL),
    atom_codes(Lower, [HL|T]).
%?- lowercase('Lowercase', X).
%@ X = lowercase.



%%%% convert a list of strings with no punctuation to a list of atoms
string_list_to_atoms_list([], []).

string_list_to_atoms_list([HS|TS], [HA|TA]) :-
    atom_string(HA, HS),
    string_list_to_atoms_list(TS, TA).

%?- string_list_to_atoms_list(["Convert", "me", "to", "atoms", "please"], X).
%@ X = ['Convert', me, to, atoms, please].



%%% compute the index of the first appearance of an element in a list
% start counting from 1
index(Elem, [Elem|_], 1) :- !.

index(Elem, [_|T], I) :-
    index(Elem, T, I1),
    I #= I1 +1,
    !.

%?- index(me, [me, you, [1, 2], me], I).
%@ I = 1.

/* End */



/* Specific utilities: Dealing with variables, constants, and predicate words */ 
%%% split a list on a key word
split(L, X, A, B) :-
    N = [X|B],
    append(A, N, L).

split(L, _, L, []).

%?- split([p, if, q], if, A, B).
%@ A = [p],
%@ B = [q] .



%%% remove all variables from a list

rm_vars(In, Out) :- 
    rm_vars_(In, [], Rev), !,
    reverse(Rev, Out).

rm_vars_([],  A, A).

rm_vars_([H|T], A, Rev) :- 
    (H = [_,_]; H = [_,_,_]), !,
    rm_vars_(T, A, Rev).

rm_vars_([H|T], A, Rev) :-
    rm_vars_(T, [H|A], Rev).

%?- rm_vars([[a, person, 1], [a, person, 1], [a, person, 2],[a, person, 3]],X).

%%%



%%% keep only variables from a list
keep_vars(In, Out) :- 
    keep_vars_(In, [], Rev), !,
    reverse(Rev, Out).

keep_vars_([],  A, A).

keep_vars_([H|T], A, Rev) :- 
    (H = [_,_]; H = [_,_,_]),!,
    keep_vars_(T, [H|A], Rev).

keep_vars_([_|T], A, Rev) :-
    keep_vars_(T, A, Rev).

%?- keep_vars([hello, hello, goodbye,[a, person, 1], [a, person, 1], [a, person, 2],[a, person, 3]],X).
%@ X = [[a, person, 1], [a, person, 1], [a, person, 2], [a, person, 3]].



/* End */ 
