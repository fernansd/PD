/*
  Fichero: "ej9_monumentos.pl"
  Descripción: Ejercicio 9
 */
 
monumento("Mezquita", "Córdoba", "Árabe").
monumento("Medina Azahara", "Córdoba", "Árabe").
monumento("Catedral", "Santiago de Compostela", "Románico").

contar([], 0).

contar([_ | Cola], N):-
	contar(Cola, N1),
	N is N1+1.

contar_monumentos(Localidad, N):-
	findall(M, monumento(M, Localidad, _), L),
	contar(L, N).
