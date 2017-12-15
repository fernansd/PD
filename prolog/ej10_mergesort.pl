/*
  Fichero: "ej10_mergesort.pl"
  Descripción: Ejercicio 10
 */

split([],[],[]).

split([X], [X], []).

split([E1| [E2| Cola]], [E1|R1], [E2|R2]):-
	split(Cola, R1, R2).
	
merge([], L, L).

merge(L, [], L).

merge([E1 | Cola1],  [E2 | Cola2], R):-
	E1 < E2,
	merge(Cola1, [E2 | Cola2], R1),
	R = [E1 | R1].

merge([E1 | Cola1],  [E2 | Cola2], R):-
	E1 >= E2,
	merge([E1 | Cola1], Cola2, R1),
	R = [E2 | R1].
	
mergesort([], []).

mergesort([X], [X]).

mergesort(L, R):-
	split(L, L1, L2),
	mergesort(L1, R1),
	mergesort(L2, R2),
	merge(R1, R2, R),!.
