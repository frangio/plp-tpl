%%%%%%%%%%%%%%%%%%%%%%%%
%% Tablero
%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 1
%% tablero(+Filas,+Columnas,-Tablero) instancia una estructura de tablero en blanco
%% de Filas x Columnas, con todas las celdas libres.

% tablero(+Filas, +Columnas, -Tablero)
tablero(Filas, Columnas, Tablero) :-
	length(Tablero, Filas),
        maplist(flip(length, Columnas), Tablero).

% flip(+R, ?X, ?Y)
flip(R, X, Y) :- call(R, Y, X).

%% Ejercicio 2
%% ocupar(+Pos, ?Tablero) será verdadero cuando la posición indicada esté ocupada.
ocupar(pos(F, C), Tablero) :- posicion(pos(F, C), Tablero, ocupada).

%% posicion(+Pos, +Tablero, ?Contenido)
posicion(pos(F, C), Tablero, Contenido) :-
	nth0(F, Tablero, Fila),
	nth0(C, Fila, Contenido).

%% Tableros de ejemplo
tablero(ej5x5, T) :-
	tablero(5, 5, T),
	ocupar(pos(1, 1), T),
	ocupar(pos(1, 2), T).

tablero(libre20, T) :-
	tablero(20, 20, T).

tablero(ej3x3diagonal, T) :-
	tablero(3, 3, T),
	ocupar(pos(0, 0), T),
	ocupar(pos(1, 1), T),
	ocupar(pos(2, 2), T).

tablero(ocupado2x3, T) :-
	tablero(2, 3, T),
	ocupar(pos(0, 0), T),
	ocupar(pos(0, 1), T),
	ocupar(pos(0, 2), T),
	ocupar(pos(1, 0), T),
	ocupar(pos(1, 1), T),
	ocupar(pos(1, 2), T).

%% _ X _ _ _ 
%% _ X _ X _ 
%% _ _ _ X _ 
%% _ _ _ X _ 
tablero(ej4x5conObstaculos, T) :-
	tablero(4, 5, T),
	ocupar(pos(0, 1), T),
	ocupar(pos(1, 1), T),
	ocupar(pos(1, 3), T),
	ocupar(pos(2, 3), T),
	ocupar(pos(3, 3), T).


%% Ejercicio 3
%% vecino(+Pos, +Tablero, -PosVecino) será verdadero cuando PosVecino sea
%% un átomo de la forma pos(F', C') y pos(F', C') sea una celda contigua a
%% pos(F, C), donde Pos=pos(F, C). Las celdas contiguas puede ser a lo sumo cuatro
%% dado que el robot se moverá en forma ortogonal.
%% Usamos generate&test
vecino(pos(F, C), Tablero, pos(FV, CV)) :-
	posibleVecino(pos(F, C), pos(FV, CV)),
	posicionValida(pos(FV, CV), Tablero).

%% posibleVecino(+Pos,-Pos')
%% Devuelve en Pos' uno de los cuatro posibles vecinos de Pos,
%% moviendose ortogonalmente.
posibleVecino(pos(F, C), pos(FV, CV)) :- FV is F - 1, CV is C.
posibleVecino(pos(F, C), pos(FV, CV)) :- FV is F + 1, CV is C.
posibleVecino(pos(F, C), pos(FV, CV)) :- FV is F, CV is C - 1.
posibleVecino(pos(F, C), pos(FV, CV)) :- FV is F, CV is C + 1.

%% posicionValida(+Pos, +Tablero)
posicionValida(pos(F, C), Tablero) :-
	tamanio(Tablero, NumeroF, NumeroC),
	F >= 0, F < NumeroF,
	C >= 0, C < NumeroC.

%% tamanio(+Tablero, -F, -C)
%% A pesar de que podriamos usar tablero(F, C, T) para obtener
%% F y C (se puede dado a cómo esta implementada),
%% su nomenclatura de instanciacion permite que se cambie
%% ese comportamiento y ya no nos sirva.
tamanio(Tablero, F, C) :-
	length(Tablero, F),
	nth0(0, Tablero, Fila),
	length(Fila, C).

%% Ejercicio 4
%% vecinoLibre(+Pos, +Tablero, -PosVecino) idem vecino/3 pero además PosVecino
%% debe ser una celda transitable (no ocupada) en el Tablero
vecinoLibre(Pos, Tablero, PosVecino) :-
	vecino(Pos, Tablero, PosVecino),
	libre(PosVecino, Tablero).

%% libre(+Pos, +Tablero) 
libre(Pos, Tablero) :-
	posicion(Pos, Tablero, Celda),
	var(Celda).


%%%%%%%%%%%%%%%%%%%%%%%%
%% Definicion de caminos
%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 5
%% camino(+Inicio, +Fin, +Tablero, -Camino) será verdadero cuando Camino sea una lista
%% [pos(F1, C1), pos(F2, C2),..., pos(Fn, Cn)] que denoten un camino desde Inicio
%% hasta Fin pasando solo por celdas transitables.
%% Además se espera que Camino no contenga ciclos.
%% Notar que la cantidad de caminos es finita y por ende se tiene que poder recorrer
%% todas las alternativas eventualmente.
%% Consejo: Utilizar una lista auxiliar con las posiciones visitadas
camino(Inicio, Fin, Tablero, Camino) :-
	caminoValido(Inicio, Fin, Tablero, Camino, []).

caminoValido(Pos, Pos, Tablero,[Pos], _) :- libre(Pos, Tablero).
caminoValido(Inicio, Fin, Tablero,[Inicio|RestoCamino], Visitadas) :-
	not( Inicio = Fin ),
	posicionValida(Inicio, Tablero), posicionValida(Fin, Tablero),
	vecinoLibre(Inicio, Tablero, PosLibre),
	not(member(PosLibre, Visitadas)),
	caminoValido(PosLibre, Fin, Tablero, RestoCamino, [PosLibre|Visitadas]).


%% Ejercicio 6
%% cantidadDeCaminos(+Inicio, +Fin, +Tablero, ?N) que indique la cantidad de caminos
%% posibles sin ciclos entre Inicio y Fin.
cantidadDeCaminos(Inicio, Fin, Tablero, N) :- 
	bagof(Camino, camino(Inicio, Fin, Tablero, Camino), Bag),
	length(Bag, N).

%% Ejercicio 7
%% camino2(+Inicio, +Fin, +Tablero, -Camino) ídem camino/4 pero se espera una heurística
%% que mejore las soluciones iniciales.
%% No se espera que la primera solución sea necesariamente la mejor.
%% Una solución es mejor mientras menos pasos se deba dar para llegar a
%% destino (distancia Manhattan). Por lo tanto, el predicado deberá devolver de a uno,
%% todos los caminos pero en orden creciente de longitud.
camino2(_, _, _, _).

%% Ejercicio 8
%% camino3(+Inicio, +Fin, +Tablero, -Camino) ídem camino2/4 pero se espera que
%% se reduzca drásticamente el espacio de búsqueda.
%% En el proceso de generar los potenciales caminos, se pueden ir sacando algunas conclusiones.
%% Por ejemplo, si se está en la celda (3, 4) y se dieron ya 6 pasos desde el Inicio,
%% entonces no tiene sentido seguir evaluando cualquier camino que implique llegar a la celda (3, 4)
%% desde Inicio en más de 6 pasos.
%% Notar que dos ejecuciones de camino3/4 con los mismos argumentos deben dar los mismos resultados.
%% En este ejercicio se permiten el uso de predicados: dynamic/1, asserta/1, assertz/1 y retractall/1.
camino3(_, _, _, _).

%%%%%%%%%%%%%%%%%%%%%%%%
%% Tableros simultáneos
%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 9
%% caminoDual(+Inicio, +Fin, +Tablero1, +Tablero2, -Camino) será verdadero
%% cuando Camino sea un camino desde Inicio hasta Fin pasando al mismo tiempo
%% sólo por celdas transitables de ambos tableros.
%% Nota: Es posible una implementación que resuelva en forma inmediata casos en los que trivialmente no existe camino dual posible.
caminoDual(_, _, _, _, _).
