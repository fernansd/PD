/* Fichero: "familia.pl"
 */
 
/*
  hombre(Persona)
  
  Predicado que indica si una Persona es un hombre
  
hombre(antonio).
hombre(juan).
hombre(luis).
hombre(rodrigo).
hombre(ricardo).

mujer(isabel).
mujer(ana).
mujer(marta).
mujer(carmen).
mujer(laura).
mujer(alicia).

matrimonio(antonio, ana).
matrimonio(juan, carmen).
matrimonio(luis, isabel).
matrimonio(rodrigo, laura).

/* hijo_de(hijo, padre, madre */
hijo_de(juan, antonio, ana).
hijo_de(rodrigo, antonio, ana).
hijo_de(marta, antonio, ana).
hijo_de(carmen, luis, isabel).
hijo_de(ricardo, juan, carmen).
hijo_de(alicia, rodrigo, isabel).


/* TODO: regla que indice que le predicado matrimonio es reflexivo */

padre_de(Persona, Hijo) :-
	es_hijo(Hijo, Persona, _);
	es_hijo(Hijo, _, Persona).
