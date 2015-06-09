%%%%%%%%%%%%%
%% Tablero %%
%%%%%%%%%%%%%

%% Ejercicio 1
% tablero(+Filas, +Columnas, -Tablero)
% Instancia una estructura de tablero en blanco de Filas x
% Columnas, con todas las celdas libres.

tablero(Filas, Columnas, Tablero) :-
	length(Tablero, Filas),
	maplist(flip(length, Columnas), Tablero).

% flip(+R, ?X, ?Y)
flip(R, X, Y) :- call(R, Y, X).

% Ejemplos
:- tablero(1, 1, T), T =@= [[_]].
:- tablero(3, 4, T), T =@= [[_,_,_,_],[_,_,_,_],[_,_,_,_]].

%% Ejercicio 2
% ocupar(+Pos, ?Tablero)
% Será verdadero cuando la posición indicada esté ocupada.
ocupar(pos(F, C), Tablero) :- posicion(pos(F, C), Tablero, ocupada).

% posicion(+Pos, +Tablero, ?Contenido)
% Será verdadero cuando el valor en la posicón Pos en el
% Tablero unifique con Contenido.
posicion(pos(F, C), Tablero, Contenido) :-
	nth0(F, Tablero, Fila),
	nth0(C, Fila, Contenido).

% Ejemplos
:- tablero(2, 3, T), ocupar(pos(0, 0), T), posicion(pos(0, 0), T, ocupada).

%% Ejercicio 3
% vecino(+Pos, +Tablero, -PosVecino)
% Será verdadero cuando PosVecino sea un átomo de la forma
% pos(F', C') y pos(F', C') sea una celda contigua a pos(F,
% C), donde Pos=pos(F, C). Las celdas contiguas puede ser a
% lo sumo cuatro dado que el robot se moverá en forma
% ortogonal.

% Usamos Generate And Test
vecino(pos(F, C), Tablero, pos(FV, CV)) :-
	posibleVecino(pos(F, C), pos(FV, CV)),
	posicionValida(pos(FV, CV), Tablero).

% posibleVecino(+Pos1,-Pos2)
% Instancia Pos2 en uno de los cuatro posibles vecinos de
% Pos1, moviéndose ortogonalmente.
posibleVecino(pos(F, C), pos(FV, CV)) :- FV is F - 1, CV is C.
posibleVecino(pos(F, C), pos(FV, CV)) :- FV is F + 1, CV is C.
posibleVecino(pos(F, C), pos(FV, CV)) :- FV is F, CV is C - 1.
posibleVecino(pos(F, C), pos(FV, CV)) :- FV is F, CV is C + 1.

% posicionValida(+Pos, +Tablero)
posicionValida(pos(F, C), Tablero) :-
	tablero(NumeroF, NumeroC, Tablero),
	F >= 0, F < NumeroF,
	C >= 0, C < NumeroC.

% Ejemplos

% mismos_elementos(+L1, +L2)
% Es verdadero si L1 y L2 tienen los mismos elementos,
% posiblemente en distinto orden.
mismos_elementos(L1, L2) :- msort(L1, L1S), msort(L2, L2S), L1S == L2S.

:-  tablero(3, 3, T), bagof(Pos, vecino(pos(0, 0), T, Pos), Vecinos),
    mismos_elementos(Vecinos, [pos(0, 1), pos(1, 0)]).
:-  tablero(3, 3, T), bagof(Pos, vecino(pos(1, 1), T, Pos), Vecinos),
    mismos_elementos(Vecinos, [pos(0, 1), pos(1, 0), pos(1, 2), pos(2, 1)]).

%% Ejercicio 4
% vecinoLibre(+Pos, +Tablero, -PosVecino)
% Ídem vecino/3 pero además PosVecino debe ser una celda
% transitable (no ocupada) en el Tablero.
vecinoLibre(Pos, Tablero, PosVecino) :-
	vecino(Pos, Tablero, PosVecino),
	libre(PosVecino, Tablero).

% libre(+Pos, +Tablero) 
libre(Pos, Tablero) :-
	posicion(Pos, Tablero, Celda),
	var(Celda).

% Ejemplos

:-  tablero(2, 2, T), ocupar(pos(0, 1), T),
    bagof(Pos, vecinoLibre(pos(0, 0), T, Pos), VecinosLibres),
    VecinosLibres == [pos(1, 0)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Definicion de caminos %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 5
% camino(+Inicio, +Fin, +Tablero, -Camino)
% Será verdadero cuando Camino sea una lista [pos(F1, C1),
% pos(F2, C2), ..., pos(Fn, Cn)] que denoten un camino desde
% Inicio hasta Fin pasando solo por celdas transitables.
% Además se espera que Camino no contenga ciclos.
% Notar que la cantidad de caminos es finita y por ende se
% tiene que poder recorrer todas las alternativas
% eventualmente.
camino(Inicio, Fin, Tablero, Camino) :-
        libre(Inicio, Tablero),
        posicionValida(Fin, Tablero),
	caminoSinVisitadas(Inicio, Fin, Tablero, Camino, [Inicio]).

% caminoSinVisitadas(+Inicio, +Fin, +Tablero, -Camino, +Visitadas)
caminoSinVisitadas(Pos, Pos, Tablero, [Pos], _) :- !, libre(Pos, Tablero).
caminoSinVisitadas(Inicio, Fin, Tablero,[Inicio|RestoCamino], Visitadas) :-
	vecinoLibre(Inicio, Tablero, SiguientePaso),
	not(member(SiguientePaso, Visitadas)),
	caminoSinVisitadas(SiguientePaso, Fin, Tablero, RestoCamino, [SiguientePaso|Visitadas]).

%% Ejercicio 6
% cantidadDeCaminos(+Inicio, +Fin, +Tablero, ?N)
% Indica la cantidad de caminos posibles sin ciclos entre
% Inicio y Fin.
cantidadDeCaminos(Inicio, Fin, Tablero, N) :- 
	findall(Camino, camino(Inicio, Fin, Tablero, Camino), Bag),
	length(Bag, N).

%% Ejercicio 7
% camino2(+Inicio, +Fin, +Tablero, -Camino)
% Ídem camino/4 pero se espera una heurística
% que mejore las soluciones iniciales.
% No se espera que la primera solución sea necesariamente la mejor.
% Una solución es mejor mientras menos pasos se deba dar para llegar a
% destino (distancia Manhattan). Por lo tanto, el predicado deberá devolver de a uno,
% todos los caminos pero en orden creciente de longitud.

camino2(Inicio, Fin, Tablero, Camino) :-
        libre(Inicio, Tablero),
	caminoSinVisitadas2(Inicio, Fin, Tablero, Camino, [Inicio]).

% caminoSinVisitadas2(+Inicio, +Fin, +Tablero, -Camino, +Visitadas)
caminoSinVisitadas2(Pos, Pos, Tablero, [Pos], _) :- !, libre(Pos, Tablero).
caminoSinVisitadas2(Inicio, Fin, Tablero,[Inicio|RestoCamino], Visitadas) :-
	Inicio \= Fin,
	posicionValida(Inicio, Tablero),
        posicionValida(Fin, Tablero),
	bagof(Sig, vecinoLibre(Inicio, Tablero, Sig), Siguientes),
	segunDistancia(Fin, SiguientePaso, Siguientes),
	not(member(SiguientePaso, Visitadas)),
	caminoSinVisitadas2(SiguientePaso, Fin, Tablero, RestoCamino, [SiguientePaso|Visitadas]).

segunDistancia(Fin, Pos, List) :- predsort(predDistancia(Fin), List, Sorted), !, member(Pos, Sorted).

predDistancia(Fin, Delta, Pos1, Pos2) :-
	distancia(Fin, Pos1, D1), distancia(Fin, Pos2, D2),
	(compare(Delta, D1, D2), Delta \= '='); Delta = '<'.

distancia(pos(X1, Y1), pos(X2, Y2), D) :- D is abs(X1 - X2) + abs(Y1 - Y2).


cantidadDeCaminos2(Inicio, Fin, Tablero, N) :- 
	findall(Camino, camino2(Inicio, Fin, Tablero, Camino), Bag),
	length(Bag, N).

%% Ejercicio 8
%% camino3(+Inicio, +Fin, +Tablero, -Camino) ídem camino2/4 pero se espera que
%% se reduzca drásticamente el espacio de búsqueda.
%% En el proceso de generar los potenciales caminos, se pueden ir sacando algunas conclusiones.
%% Por ejemplo, si se está en la celda (3, 4) y se dieron ya 6 pasos desde el Inicio,
%% entonces no tiene sentido seguir evaluando cualquier camino que implique llegar a la celda (3, 4)
%% desde Inicio en más de 6 pasos.
%% Notar que dos ejecuciones de camino3/4 con los mismos argumentos deben dar los mismos resultados.
%% En este ejercicio se permiten el uso de predicados: dynamic/1, asserta/1, assertz/1 y retractall/1.

:- dynamic camino3lookup/5.

camino3(Inicio, Fin, Tablero, Camino) :- camino3SinVisitadas(Inicio, Fin, Tablero, Camino, [Inicio]).

camino3SinVisitadas(Inicio, Inicio, Tablero, [Inicio], _) :- !, libre(Inicio, Tablero).

camino3SinVisitadas(Inicio, Fin, Tablero, Camino, Visitadas1) :-
	camino3lookup(Inicio, Fin, Tablero, Caminos, Visitadas2),
	mismos_elementos(Visitadas1, Visitadas2),
	!,
	member(Camino, Caminos).

% Esto es en esencia el algoritmo de Dijkstra.
camino3SinVisitadas(Inicio, Fin, Tablero, Camino, Visitadas) :-
	bagof(Cam, (
	  vecinoLibre(Inicio, Tablero, Sig),
          not(member(Sig, Visitadas)),
	  camino3SinVisitadas(Sig, Fin, Tablero, RestoCam, [Sig | Visitadas]),
	  Cam = [Inicio | RestoCam]
	), Caminos),
	minimos(Caminos, CaminosMinimos),
	assert(camino3lookup(Inicio, Fin, Tablero, CaminosMinimos, Visitadas)),
	member(Camino, CaminosMinimos).

% minimos(+ListaDeListas, -ListasMasCortas)
minimos(ListaDeListas, ListasMasCortas) :-
	findall(L, (
	  member(L, ListaDeListas),
	  length(L, N),
	  forall(member(Other, ListaDeListas), (length(Other, M), N =< M))
	), ListasMasCortas).

:- minimos([[1,2], [10], [1,2,3], [5]], L), mismos_elementos(L, [[10], [5]]).



%%%%camino4 intenta ser camino3 pero con otra idea%%%%%%

:- dynamic minimaDistancia/2.

%DISCLAIMER: el resultado depende del orden en el que se evaluan los vecinos.
camino4(Inicio, Fin, Tablero, Camino) :-
    libre(Inicio, Tablero),
    retractall(minimaDistancia(X,Y)),
    %al principio todos tienen distancia como 0 (pensar como el infinito de dijkstra).
    assert(minimaDistancia(X,0)),
	caminoSinVisitadas4(Inicio, Fin, Tablero, Camino, [Inicio]).

% caminoSinVisitadas4(+Inicio, +Fin, +Tablero, -Camino, +Visitadas)
caminoSinVisitadas4(Pos, Pos, Tablero, [Pos], Visitadas):- libre(Pos, Tablero).
caminoSinVisitadas4(Inicio, Fin, Tablero,[Inicio|RestoCamino], Visitadas) :-
	posicionValida(Inicio, Tablero),
    posicionValida(Fin, Tablero),
	vecinoLibre(Inicio, Tablero, SiguientePaso),
	not(member(SiguientePaso, Visitadas)),
	length(Visitadas, L),
	minimaDistanciaASiguiente(SiguientePaso, L),
	%agrego al principio de la base de conocimiento porque me gustaria que se evalue primero.
	asserta(minimaDistancia(SiguientePaso, L)),
	caminoSinVisitadas4(SiguientePaso, Fin, Tablero, RestoCamino, [SiguientePaso|Visitadas]).

%aca no calcule ninguna distancia para la posicion
minimaDistanciaASiguiente(Posicion, L):- minimaDistancia(Posicion, 0).
%aca tengo por lo menos una distancia. me fijo si la que tengo ahora es "mejor".
minimaDistanciaASiguiente(Posicion, L):- minimaDistancia(Posicion, D),!, L < D.






%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tableros simultáneos %%
%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 9
% caminoDual(+Inicio, +Fin, +Tablero1, +Tablero2, -Camino)
% Será verdadero cuando Camino sea un camino desde Inicio
% hasta Fin pasando al mismo tiempo sólo por celdas
% transitables de ambos tableros.
% Nota: Es posible una implementación que resuelva en forma
% inmediata casos en los que trivialmente no existe camino
% dual posible.

% Usa generate and test.
caminoDual(Inicio, Fin, Tablero1, Tablero2, Camino) :-
	camino2(Inicio, Fin, Tablero1, Camino), % Genera un camino en Tablero1.
	camino2(Inicio, Fin, Tablero2, Camino). % Chequea que es válido en Tablero2.


%% Predicados útiles para el desarrollo

%% Visualización de un tablero y un camino sobre el mismo.
% dibujarTableroConCamino(+Tablero, +Camino).
dibujarTableroConCamino(Tablero, Camino) :-
	tablero(_, Columnas, Tablero),
	write('┌'),
	forall(between(1, Columnas, I), write('───')),
	write('──┐'),
	nl,
	forall(nth0(NFila, Tablero, Fila), dibujarFila(NFila, Fila, Camino)),
	write('└'),
	forall(between(1, Columnas, I), write('───')),
	write('──┘'),
        nl.

% dibujarFila(+Nfila, +Fila, +Camino)
dibujarFila(NFila, Fila, Camino) :-
	write('│'),
	forall(
	  nth0(NCol, Fila, X),
	  (
	    (
	      nth0(Step, Camino, pos(NFila, NCol)), atom_concat('  ', Step, Yaux), sub_atom(Yaux, _, 3, 0, Y);
	      var(X), Y = '   ';
	      Y = ' ▒▒'
	    ),
	    write(Y)
	  )
	),
	write('  │'),
	nl.


%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tableros de ejemplo %%
%%%%%%%%%%%%%%%%%%%%%%%%%

% tablero(+Nombre, -T).

% ┌─────────────────┐
% │                 │
% │    ▒▒ ▒▒        │
% │                 │
% │                 │
% │                 │
% └─────────────────┘
tablero(ej5x5, T) :-
	tablero(5, 5, T),
	ocupar(pos(1, 1), T),
	ocupar(pos(1, 2), T).

% ┌─────────────────┐
% │                 │
% │          ▒▒     │
% │       ▒▒        │
% │                 │
% │                 │
% └─────────────────┘
tablero(ej5x5_2, T) :-
	tablero(5, 5, T),
	ocupar(pos(1, 3), T),
	ocupar(pos(2, 2), T).

tablero(libre20, T) :-
	tablero(20, 20, T).

% ┌──────────┐
% │ ▒▒       │
% │    ▒▒    │
% │       ▒▒ │
% └──────────┘
tablero(ej3x3diagonal, T) :-
	tablero(3, 3, T),
	ocupar(pos(0, 0), T),
	ocupar(pos(1, 1), T),
	ocupar(pos(2, 2), T).

% ┌──────────┐
% │ ▒▒ ▒▒ ▒▒ │
% │ ▒▒ ▒▒ ▒▒ │
% └──────────┘
tablero(ocupado2x3, T) :-
	tablero(2, 3, T),
	ocupar(pos(0, 0), T),
	ocupar(pos(0, 1), T),
	ocupar(pos(0, 2), T),
	ocupar(pos(1, 0), T),
	ocupar(pos(1, 1), T),
	ocupar(pos(1, 2), T).

% ┌─────────────────┐
% │    ▒▒           │
% │    ▒▒    ▒▒     │
% │          ▒▒     │
% │          ▒▒     │
% └─────────────────┘
tablero(ej4x5conObstaculos, T) :-
	tablero(4, 5, T),
	ocupar(pos(0, 1), T),
	ocupar(pos(1, 1), T),
	ocupar(pos(1, 3), T),
	ocupar(pos(2, 3), T),
	ocupar(pos(3, 3), T).

% ┌───────┐
% │       │
% │       │
% └───────┘
tablero(ej2x2, T) :- tablero(2,2,T).
