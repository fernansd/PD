/*
  Fichero: "ej8_invertir.pl"
  Descripción: Ejercicio 8
 */

concatenar([], L, L).

concatenar([E1 | Cola1], L2, [E1 | R]):-
	concatenar(Cola1, L2, R).
	
es_lista([]).

es_lista([_|Cola]):-
	es_lista(Cola).

invertir([], []).

invertir([X | Cola], R):-
	es_lista(X),
	invertir(X, R1),
	invertir(Cola, R2),
	concatenar(R2, [R1], R),!.

invertir([X | Cola], R):-
	invertir(Cola, R1),
	concatenar(R1, [X], R),!.
