/* Modules and self-defined files*/
:- use_module(library(apply)).
:- use_module(library(clpfd)).
:- load_files('lexicon.pl').
:- load_files('string_handling.pl').
:- load_files('utilities.pl').

:-set_prolog_flag(answer_write_options,[max_depth(0)]).
/* End */


:- op(500,fx, translate).
translate X :- parse(X, _).

%?- parse("John is an animal if John
%is a person.",Output).
%@ Output = [(is_(animal,john):-is_(person,john))].

%@ Output = [(is_(animal,_8150):-is_(person,_8150))].

/* Predicates which check whether the input term is a variable, constant, or predicate word */
%%% recognises constants
is_constant(C) :-
    simple(C),
    \+ lex(C, det),
    \+ lex(C, logical),
    atom_chars(C, [H|_]),
    upper_lower(H,_).

%?- is_constant('Tom').
%@ true.

%%% recognises variables
is_variable([D, _]) :-
    lex(D, det).



%%% recognises predicate words
is_predicate_word([D, V]) :-
    simple(D),
    \+ lex(D, det),
    \+ lex(D, logical),
    atom_chars(V, [H|_]),
    upper_lower(_,H).

/* End */



/* termify/2 creates variable and constant structures */

% [a,dog] -> [a, dog, -1]
% 'Tom' -> [tom, const]
% except when is in front; then do not reform

termify(In, Out) :-
    termify_(In, [], R),!,
    reverse(R, Out).

termify_([], A, A).



% deal with 'is' before variable
termify_([is, D, W|T], A, R) :-
    lex(D, det),
    termify_(T, [W, is|A], R). % need to reverse as re-reversed at termify().



% deal with 'is' before constant
termify_([is, C|T], A, R) :-
    is_constant(C),
    lowercase(C, CL),
    termify_(T, [CL, is|A], R). % need to reverse as re-reversed at termify().


% deal with 'is' before not + variable
termify_([is, not, D, W|T], A, R) :-
    lex(D, det),
    termify_(T, [W, not, is|A], R). % need to reverse as re-reversed at termify().



% deal with 'is' before not + constant
termify_([is, not, C|T], A, R) :-
    is_constant(C),
    lowercase(C, CL),
    termify_(T, [CL, not, is|A], R). % need to reverse as re-reversed at termify().



% deal with variable
termify_([D, W|T], A, R) :-
    lex(D, det),
    termify_(T, [[D, W, -1]|A], R).



% deal with constant
termify_([W|T], A, R) :-
    is_constant(W),
    lowercase(W, WL),
    termify_(T, [[WL, const]|A], R).



% deal with variable
termify_([W|T], A, R) :-
    \+is_constant(W),
    lowercase(W, WL),
    termify_(T, [WL|A], R).


%?- termify(['A', lion, eats, 'Tom', if, 'Tom',is, near, the, lion], Terms).
%@ Terms = [[A,lion,-1],eats,[tom,const],if,[tom,const],is,near,[the,lion,-1]].

%?- termify(['Vesko', is, a, lion], X).
%@ X = [[vesko,const],is,lion].


/* End */



/* Predicates to number variables on a sigle pass through a sentence */

% indefinite variables, [a, var, _], are numbered in order of appearance starting for 1
% definite variables, [the, some_var, I], take the same index
% as the first occurrence of [a, some_var, I] earlier in the 

index_vars(Terms, IndexedTerms) :-
    index_vars_(Terms, 0,[], Rev),
    reverse(Rev, IndexedTerms),!.

index_vars_([], _, A, A).

index_vars_([H|T], I, A, IT) :-
    (simple(H); H = [_,const]),
    index_vars_(T, I, [H|A], IT).

index_vars_([[D, V, _]|T], I, A, IT) :-
    lex(D, indefinite_det),
    I1 #= I + 1,
    index_vars_(T, I1, [[D, V,I1]|A], IT).

index_vars_([[D, V, _]|T], I, A, IT) :-
    lex(D, definite_det),
    member([_, V, I1], A),
    index_vars_(T, I, [[D,V,I1]|A], IT).

%?- termify([a,lion,eats,'Tom',if,'Tom',is,near,the,lion], L), index_vars(L, X).
%@ L = [[a,lion,-1],eats,[tom,const],if,[tom,const],is,near,[the,lion,-1]],
%@ X = [[a,lion,1],eats,[tom,const],if,[tom,const],is,near,[the,lion,1]].



/* End */



/* Split sentences with logical connetives into sentences into atomic sentences */

%%% split sentence on 'if' in the middle
split_if(Sen, Head, Body) :-
    append(Head, [if|Body], Sen), !.

%%% when if is at start
split_if(Sen, Head, Body) :-
    append(['If'|Head], [','|Body], Sen), !.

split_if(Sen, Sen, _).

%?- termify([a,lion,and,another,lion,eat,'Tom',if,'Tom',is,near,the,lion], L), index_vars(L, X), split_if(X, Head, Body).
%@ L = [[a,lion,-1],and,[another,lion,-1],eat,[tom,const],if,[tom,const],is,near,[the,lion,-1]],
%@ X = [[a,lion,1],and,[another,lion,2],eat,[tom,const],if,[tom,const],is,near,[the,lion,2]],
%@ Head = [[a,lion,1],and,[another,lion,2],eat,[tom,const]],
%@ Body = [[tom,const],is,near,[the,lion,2]].

%?- split_if(['If', a, person, lives,',', the, person, will, die], H, B).
%@ H = [a,person,lives],
%@ B = [the,person,will,die].


%%% handle 'and' and 'or'
handle_and_or_after_if(Sen, ConjList) :-
    handle_and_or_after_if_(Sen, [], Rev),
    reverse(Rev, ConjList), !.

handle_and_or_after_if_([], A, A).

% handle conjuncts 
handle_and_or_after_if_(Sen, A, ConjList) :-
    append(Conj, [and|Rest], Sen),
    \+contains_term(and, Conj),
    handle_and_or_after_if_(Rest, [',',Conj|A], ConjList).

% handle disjuncts 
handle_and_or_after_if_(Sen, A, ConjList) :-
    append(Conj, [or|Rest], Sen),
    \+contains_term(or, Conj),
    handle_and_or_after_if_(Rest,[';',Conj|A], ConjList).

% when no disjuncts or conjuncts
handle_and_or_after_if_(Sen, A, ConjList) :-
    \+contains_term(and, Sen),
    \+contains_term(or, Sen),
    handle_and_or_after_if_([], [Sen|A], ConjList).


%?- handle_and_or_after_if([[tom,const], cries, and, [harry, const], cries, or, [a, 'V_1', 1], laughs], X).
%@ X = [[[tom,const],cries],,,[[harry,const],cries],;,[[a,V_1,1],laughs]].


/* End */



/* Process predicate words via reification; only take in sentences without logical connectives */

%%% define [has,  adjective]
reify(Sen, FuncVars) :-
    rm_vars(Sen, Preds),
    keep_vars(Sen, Vars),
    Preds = [has, Adj],!,
    append([has], [Adj], FV),
    append(FV, Vars, FuncVars).
%?- reify([[john, const], has, hair], X).
%@ X = [has,hair,[john,const]].



%%% define [has, a noun, prep, adjective]
reify(Sen, FuncVars) :-
    rm_vars(Sen, Preds),
    keep_vars(Sen, Vars),
    Preds = [has, W1, Prep],
    lex(Prep, prep),!,
    append([has], [W1], FV),
    append(FV, Vars, FuncVars).
%?- reify([[john, const], has, [a, sister, -1]], X).
%@ X = [has,[john,const],[a,sister,-1]].



%%% define [is_,  adjective]
reify(Sen, FuncVars) :-
    rm_vars(Sen, Preds),
    keep_vars(Sen, Vars),
    Preds = [is, Adj],!,
    append([is_], [Adj], FV),
    append(FV, Vars, FuncVars).

%?- reify([[john, const], is, tall], X).
%@ X = [is_,tall,[john,const]].



%%% define [is_, not,  adjective]
reify(Sen, FuncVars) :-
    rm_vars(Sen, Preds),
    keep_vars(Sen, Vars),
    Preds = [is,not, Adj],!,
    append([is_], [not,Adj], FV),
    append(FV, Vars, FuncVars).

%?- reify([[john, const], is,not, tall], X).
%@ X = [is_,not,tall,[john,const]].



%%% define [is, a noun, prep, adjective]
reify(Sen, FuncVars) :-
    rm_vars(Sen, Preds),
    keep_vars(Sen, Vars),
    Preds = [is, W1, Prep],
    lex(Prep, prep),!,
    append([is_], [W1], FV),
    append(FV, Vars, FuncVars).

%?- reify([[john, const], is, father, of, [mike, const]], X).
%@ X = [is_,father,[john,const],[mike,const]].



%%% define [is, a noun, prep, adjective]
reify(Sen, FuncVars) :-
    rm_vars(Sen, Preds),
    keep_vars(Sen, Vars),
    Preds = [is, not, W1, Prep],
    lex(Prep, prep),!,
    append([is_], [not,W1], FV),
    append(FV, Vars, FuncVars).

%?- reify([[john, const], is,not, father, of, [mike, const]], X).
%@ X = [is_,not,father,[john,const],[mike,const]].



%%% define [is_, more/less/greater/smaller, than, number]
reify(Sen, FuncVars) :-
    rm_vars(Sen, Preds),
    keep_vars(Sen, Vars),
    (Preds = [is, W, _, Number];
     Preds = [is, W, _, Number, _]), !, % for units
    Vars =[X],
    lex(W, comp, Sym),!,
    append([Sym], [X, Number], FuncVars).

%?- reify([[the, grade], is, greater, than, 70, percent], Preds).
%@ Preds = [#>,[the,grade],70].

%?- reify([[the, grade], is, equal, to, 70, percent], Preds).
%@ Preds = [#=,[the,grade],70].



reify(Sen, FuncVars) :-
    rm_vars(Sen, Preds),
    keep_vars(Sen, Vars),
    Preds \= [is],
    Preds \= [is, _],
    atomic_list_concat(Preds, ConcatPreds),
    append([ConcatPreds], Vars , FuncVars).

%?- reify([[john, const], voraciously, eats, [a, man, 1]], X).
%@ X = [voraciouslyeats,[john,const],[a,man,1]].



reify_conj(In, Out) :-
    reify_conj_(In, [], Rev),!,
    reverse(Rev, Out).

reify_conj_([], A, A).

reify_conj_([H, Conj|T], A, B) :-
    (Conj = ',';
     Conj = ';'),
    reify(H, Res),
    reify_conj_(T, [Conj,Res|A], B).

reify_conj_([H|T], A, B) :-
    reify(H, Res),
    reify_conj_(T, [Res|A], B).

%?- reify_conj([[[a, lion, 1], eats, [a, man]],';',[[a, lion, 1], eats, [a, man]]], X).
%@ X = [[eats,[a,lion,1],[a,man]],;,[eats,[a,lion,1],[a,man]]].

/* End */


/* simplified_term_list/2 converts a list of terms into a flat list */ 
% constants are lowercased
% variables structures are replced by names like  'V_1'


simplify_term_list(In, Out) :-
    simplify_term_list_(In, [], Rev),!,
    reverse(Rev, Out).

simplify_term_list_([], A, A).

simplify_term_list_([H|T], A, L) :-
    simple(H),
    simplify_term_list_(T, [H|A], L).

simplify_term_list_([[H,const]|T], A, L) :-
    simplify_term_list_(T, [H|A], L).

simplify_term_list_([[_,_,I]|T], A, L) :-
    atomic_list_concat(['V', I], '_', Var),
    simplify_term_list_(T, [Var|A], L).

%?- simplify_term_list([[a,lion,1],and,[another,lion,2],eat,[tom,const],if,[tom,const],is,near,[the,lion,2]], S).
%@ S = [V_1,and,V_2,eat,tom,if,tom,is,near,V_2].


simplify_conj(In, Out) :-
    simplify_conj_(In, [], Rev),!,
    reverse(Rev, Out).

simplify_conj_([], A, A).

simplify_conj_([H, Conj|T], A, B) :-
    simplify_term_list(H, Res),
    (Conj = ',';
     Conj = ';'),
    simplify_conj_(T, [Conj, Res|A], B).

simplify_conj_([H|T], A, B) :-
    simplify_term_list(H, Res),
    simplify_conj_(T, [Res|A], B).

/* End */



/* Bring to logical form */

%%% when you have a conditional
list_to_logical(In, Logical) :-
    termify(In, Termified),
    index_vars(Termified, Indexed),
    split_if(Indexed, Head, Body), 
    Body \= [],
    handle_and_or_after_if(Body, BodyConj),
    reify(Head, RHead),
    simplify_term_list(RHead, SHead),
    reify_conj(BodyConj, RBodyConj),
    simplify_conj(RBodyConj, SBodyConj),
    put_recursive_pred_last(SHead, SBodyConj, OSBodyConj), 
    string_from_conditional(SHead, OSBodyConj, String),
    term_string(Logical, String).

list_to_logical(In, Logical) :-
    termify(In, Termified), 
    index_vars(Termified, Indexed),
    split_if(Indexed, Head, _),
    reify(Head, RHead),
    simplify_term_list(RHead, SHead),
    string_from_atomic(SHead, Str),
    term_string(Logical, Str).


%?- list_to_logical(
% [a, lion, eats, a, person, if, the, person, is, slow, and, the, person, is, 'Tom'], BC.
%@ BC =  (eats(_4502,_4504):-is_(_4504,slow),is_(_4504,tom)).

%?- list_to_logical(['Vesko', is , studying], X).
%@ X = is_(vesko,studying).

%?- list_to_logical(['Vesko', is , studying]).
%@ true.


list_of_lists_to_logical(In, Out) :-
    list_of_lists_to_logical_(In, [], R),!,
    order_facts_before_rules(R, Out),
    assert_list(Out).
%?- termify(['A', thing, is , a, person, if, the, thing, is, an , animal], O).
%@ O = [[A,thing,-1],is,person,if,[the,thing,-1],is,animal].

list_of_lists_to_logical_([], A, A).

list_of_lists_to_logical_([H|T], A, R) :-
    list_to_logical(H, Log),
    list_of_lists_to_logical_(T, [Log|A], R).

list_of_lists_to_logical([]).

list_of_lists_to_logical([H|T]) :-
    list_to_logical(H),
    list_of_lists_to_logical(T).

%?- list_of_lists_to_logical([['Sam', is, tall], ['Bob', is, short]],X).
%@ X = [is_(sam,tall),is_(bob,short)].


%%% order facts before rules
order_facts_before_rules(In, Out) :-
    order_facts_before_rules_(In, [], [], Out).

order_facts_before_rules_([], F, R,  End) :-
    append(F, R, End).

order_facts_before_rules_([H|T], F, R,  End) :-
    term_string(H, S),
    sub_string(S, _, _, _,":-"),!,
    order_facts_before_rules_(T, F, [H|R], End).

order_facts_before_rules_([H|T], F, R,  End) :-
    order_facts_before_rules_(T, [H|F], R, End).

%%% given head and body, order 

%?-order_facts_before_rules([(p:-q), (a), (b), (q:-r)], Z).
%@ Z = [b,a,(q:-r),(p:-q)].





%%% given a simplified head and a simplified body conj list, check for
% recursion in body and put recursive predicate in the last place
rm_cap_words(In, Out) :-
    rm_cap_words_(In, [], Out).

rm_cap_words_([], A, A).

rm_cap_words_([H|T], A, R) :-
    is_constant(H),
    rm_cap_words_(T, A, R).

rm_cap_words_([H|T], A, R) :-
    rm_cap_words_(T, [H|A], R).


same_pred(L1, L2) :-
    rm_cap_words(L1, L11),
    rm_cap_words(L2, L22),
    length(L1, I),
    length(L2, I),!,
    L11= L22.

%?- same_pred([isa,go, 'V1','V2'], [isa,go, 'V3','V5']).
%@ true.

find_same_pred(L, [H|_], H) :-
    same_pred(L, H).

find_same_pred(L, [_|T], X) :-
    find_same_pred(L, T, X), !.

%?- find_same_pred([isa,go, 'V3','V1'], [[isa,go, 'V1','V2', 'V4'], [isa,go, 'V3','V5']], X).
%@ X = [isa,go,V3,V5].



put_recursive_pred_last(Head, BodyConj, OrderedBodyConj) :-
    find_same_pred(Head, BodyConj, SP),
    append(L1, [',', SP, ','|T], BodyConj), !,
    append([','|T], [',', SP], L2),
    append(L1, L2, OrderedBodyConj).

put_recursive_pred_last(Head, BodyConj, OrderedBodyConj) :-
    find_same_pred(Head, BodyConj, SP),
    append(L1, [SP, ','|T], BodyConj), !,
    append(T, [',', SP], L2),
    append(L1, L2, OrderedBodyConj).

put_recursive_pred_last(_, BodyConj, BodyConj).

%?- put_recursive_pred_last([ancestor, 'V_1', 'V_2'], [[ancestor, 'V_1', 'V_2'],',', [is_a, 'V_2', tom]], OBC).
%@ OBC = [[is_a,V_2,tom],,,[ancestor,V_1,V_2]].

%?- put_recursive_pred_last([ancestor, 'V_1', 'V_2'], [[is_a, 'V_2', tom],',',[ancestor, 'V_1', 'V_2'],',', [is_a, 'V_2', tom]], OBC).
%@ OBC = [[is_a,V_2,tom],,,[is_a,V_2,tom],,,[ancestor,V_1,V_2]].

%?- put_recursive_pred_last([ancestor, 'V_1', 'V_2'], [[is_a, 'V_2', tom],',', [is_a, 'V_2', tom]], OBC).
%@ OBC = [[is_a,V_2,tom],,,[is_a,V_2,tom]].

/* End */ 


/* Parse text */
parse(Text, ListOfRules) :-
    text_to_list_of_lists(Text, LoL),
    list_of_lists_to_logical(LoL, ListOfRules).

%?- parse("Vesko is studying. A person succeeds if the person is studying. John is studying.", X).
%@ X = [is_(studying,vesko),is_(studying,john),(succeeds(_152854):-is_(studying,_152854))].


/* End */



/* Meta rules */
:- dynamic is_/3.
:- dynamic is_/2.
is_(not, Person, NounDescriptor) :- \+ is_(Person, NounDescriptor).
is_(someone, something).

is_(not, Relationship, P1, P2) :- \+ is_(Relationship, P1, P2).
is(_, p1, p2).
/* End */ 
