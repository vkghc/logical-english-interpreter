:- load_files('grammar.pl').
:- abolish(is_/3).
:- abolish(is_/2).


%?- translate
% "A person succeeds if
%   the person studies. Mary studies. Vesko studies.".


%?- parse("A person is an ancestor of another person2 if the person is
%a parent of the person2. A person is an ancestor of another person2
%if the person is an ancestor of a person3 and the person3 is a parent
%of the person2. John is a father of Molly. A person is a parent of
%another person2 if the person is a mother of the person2 or the
%person is a father of the person2. Molly is the mother of Janet.",
%X).

% @ X =
% [is_(father,john,molly),is_(mother,molly,janet),(is_(ancestor,_18120,_18122):-is_(parent,_18120,_18122)),(is_(ancestor,_20946,_20948):-is_(parent,_20954,_20948),is_(ancestor,_20946,_20954)),(is_(parent,_24422,_24424):-is_(mother,_24422,_24424);is_(father,_24422,_24424))].

%?- is_(ancestor, Ancestor, Descendant).
%@ Ancestor = molly,
%@ Descendant = janet ;
%@ Ancestor = john,
%@ Descendant = molly ;
%@ Ancestor = john,
%@ Descendant = janet ;
%@ false.


%?- translate "A number is small if the number is less than 10.".
%?- is_(small, 11).

%?- translate "A person is good if the person is not a criminal. Phillip is a criminal.".
%?- is_(good, phillip).

%?- parse("A person1 is british if the person1 was born in UK, and another person2 is parent of the person1, and the person2 is british. A person1 is parent of another person2 if the person1 is father of the person2. A person1 is parent of another person2 if the person1 is mother of the person2. John is british. Mary was born in UK. John is father of Mary.", X).
%@ X = [is_(british,john),wasbornin(mary,uK),is_(father,john,mary),(is_(british,_27244):-wasbornin(_27244,uK),is_(parent,_27256,_27244),is_(british,_27256)),(is_(parent,_29108,_29110):-is_(father,_29108,_29110)),(is_(parent,_30950,_30952):-is_(mother,_30950,_30952))].


% translate  "A student receives distinction if the student passes, and the student receives more than seventy on exams, and the student receives more than seventy on personal project. Helen passes. Helen receives more than seventy on exams.".

