ama(juan, ana).
ama(ana, miguel).
ama(miguel, ana).
ama(laura, juan).
ama(juan, laura).
ama(isabel, luis).

/* Preguntas en Prolog
ama(juan, X).
ama(X, ana).
ama(X, _).
ama(_, X).
ama(X, Y), ama(Y, X).
ama(X, Y), not(ama(Y, X).
*/
amantes:-
	ama(X,Y),
	ama(Y,X),
	write(X), nl,
	write(Y).
