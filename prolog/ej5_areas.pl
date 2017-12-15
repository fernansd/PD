/*
  Fichero: "ej5_areas.pl"
  Descripción: Ejercicio 5
 */
 
area_circulo(Radio, Area):-
	Area is pi * Radio ** 2.

area_trapecio(Bmenor, Bmayor, Altura, Area):-
	Area is (Bmenor + Bmayor) * Altura / 2.

	
producto_rango(N1, N2, P):-
	N1 = N2,
	P is 1.

producto_rango(N1, N2, P):-
	N1 < N2,
	N is N1+1,
	producto_rango(N, N2, P2),
	P is N1*P2.
	
producto_rango(N1, N2, P):-
	N1 > N2,
	N is N2+1,
	producto_rango(N, N1, P2),
	P is N1*P2.
