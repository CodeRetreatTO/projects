%% people(['Alex', 'Beatrice', 'Clarence', 'Diego', 'Ernest', 'Francesca', 'Gwen', 'Heidi', 'Ingrid']).

person('Alex').
person('Beatrice').
person('Clarence').
person('Diego').
person('Ernest').
person('Francesca').
person('Gwen').
person('Heidi').
person('Ingrid').

giving(A, B) :- 
    person(A),
    person(B),
    A \= B.

something([A, B], [[A, B]]) :-
    giving(A, B).

something([A, B | Rest], [[A, B] | Pairings]) :-
    giving(A, B),
    something([B | Rest], Pairings),
    not(member([A, B], Pairings)),
    not(member([B, A], Pairings)).
