/*
  Fichero: "ej11_listas_numericas.pl"
  Descripción: Ejercicio 11
 */

contar([], 0).

contar([_|Cola], R):-
	contar(Cola, R1),
	R is R1 + 1.

sumar_lista([], 0).

sumar_lista([X], X).

sumar_lista([X | Cola], R):-
	sumar_lista(Cola, R1),
	R is R1 + X.

media_lista(L, R):-
	sumar_lista(L, Suma),
	contar(L, N),
	R is Suma/N.

maximo_lista([X], X). 

maximo_lista([E1 | [E2 | Cola]], R):-
	E1 > E2,
	maximo_lista([E1 | Cola], R).
	
maximo_lista([E1 | [E2 | Cola]], R):-
	E1 =< E2,
	maximo_lista([E2 | Cola], R).
	
mediana_lista(L, R):-
	contar(L, N),
	N mod 2 = 1,
	!,
	mergesort(L, L1),
	Mediana is N/2,
	get(Mediana, L, R).
	
