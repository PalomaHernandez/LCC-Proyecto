:- module(proylcc, 
	[  
		flick/5
	]).

cantCol(14).
cantFilas(14).

:-use_module(library(lists)).
:- dynamic visitados/1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% visitar(+Lista).
% 
% Visita todos los elementos X que estan contenidos en Lista, es decir, realiza
% una inserción del predicado visitados con el elemento X.

visitar([]).
visitar([X|Xs]):- assert(visitados(X)), 
    			  visitar(Xs).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% replace(+X, +XIndex, +Y, +Xs, -XsY)
%
% XsY es el resultado de reemplazar la ocurrencia de X en la posición XIndex de Xs por Y.

replace(X, 0, Y, [X|Xs], [Y|Xs]).

replace(X, XIndex, Y, [Xi|Xs], [Xi|XsY]):-
    XIndex > 0,
    XIndexS is XIndex - 1,
    replace(X, XIndexS, Y, Xs, XsY).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% getColor(+Grid, +Lista, -Color)
%
% Grid es la grilla actual.
% Lista es una lista de dos elementos que representa una coordenada de la grilla Grid.
% Color es el color de la coordenada representada por la lista Lista en la grilla Grid.

getColor(Grid, [X,Y], Color):- 
	nth0(X, Grid, R),
	nth0(Y, R, Color).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% adyacentes(Lista, ListaAdy)    
%
% Lista es una lista de dos elementos que representa una coordenada de la grilla dada.
% ListaAdy es el resultado de calcular los adyacentes de la coordenada dada en la grilla dada.

adyacentes([X,Y], ListaAdy):-
	 XS is X+1,
	 XR is X-1,
     YS is Y+1,
     YR is Y-1,
     ListaAdy= [[XS, Y],[XR,Y],[X,YS],[X,YR]].
	 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
%   
% setColor(+Grid, +Lista, +Color, -NewGrid)
%
% Grid es la grilla actual.
% Lista es la lista de posiciones de la grilla a la que se le debe cambiar el color.
% Color es el color que se le debe asignar a las posiciones pasadas en la lista.
% NewGrid es el resultado de cambiar el color de las posiciones de la grilla Grid que están dentro de la lista Lista. 

setColor(Grid, [], _C, Grid).

setColor(Grid, [[X,Y]|Ls], Color, NewGrid):-
	nth0(X, Grid, R),
	nth0(Y, R, C),
	replace(C , Y, Color, R, NewR),
	replace(R, X, NewR, Grid, NewGridA),
	setColor(NewGridA,Ls,Color,NewGrid).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% buscarAdyacentesC(+Grid, +AdyacentesC, +Color, -Lf)    
%
% Grid es la grilla actual.
% AdyacentesC es la lista de adyacentesC* actual.
% Color es el color de los elementos de la lista AdyacentesC.
% Lf es el resultado de buscar los adyacentesC* de cada elemento de la lista AdyacentesC. 

buscarAdyacentesC(_,[],_ , []).

buscarAdyacentesC(Grid,[[I,J]|L],Color, Lf):- 
    adyacentes([I,J], La),
    findall( [Y,X], (member([Y,X], La), getColor(Grid,[Y,X],Color), not(visitados([Y,X]))), Ls),
    visitar(La),
    buscarAdyacentesC(Grid,Ls, Color, Lm),
    append(Ls,Lm,Lh),
    buscarAdyacentesC(Grid,L, Color, Lt),
    append(Lh,Lt,Lf).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% flick(+Grid, +Color, +AdyacentesC, -FGrid, -NewAdyacentesC)
%
% Grid es la grilla actual.
% Color es el color seleccionado por el usuario.
% AdyacentesC es la lista de adyacentesC* actual.
% FGrid es el resultado de hacer 'flick' de la grilla Grid con el color Color.
% NewAdyacentesC es la lista actualizada de adyacentesC*.
% Retorna false si Color coincide con el color del primer elemento de la lista adyacentesC (celda origen).
              

flick(Grid, Color,AdyacentesC, FGrid, NewAdyacentesC):-
    AdyacentesC = [A|_Ad],
    getColor(Grid, A, C),
    Color \= C,
    setColor(Grid, AdyacentesC, Color, FGrid),
    visitar(AdyacentesC),
    buscarAdyacentesC(FGrid,AdyacentesC, Color, La),
    retractall(visitados(_)),
    append(AdyacentesC,La, NewAdyacentesC).



remove(_, [], []).
remove(C, [C|Xs], Xs).
remove(C, [X|Xs], [X|Zs]):- X \= C, remove(C, Xs, Zs).

listaSinColor(Color, Colors):-
    remove(Color, [r,v,p,g,b,y], Colors).

crearHijos([], _,[]).
crearHijos(Hijos, Prof, [H1|ArbolesHijo]):-
    ProfAux is Prof - 1,
    Hijos = [H|Hs],
    crearArbol(H, ProfAux, H1),
    crearHijos(Hs, Prof, ArbolesHijo).

crearArbol(Rotulo, 0, t(Rotulo,[])).
crearArbol(Rotulo, Prof, Arbol):-
    Prof > 0,
    listaSinColor(Rotulo, Hijos),
    crearHijos(Hijos,Prof,ArbolesHijo),
    Arbol= t(Rotulo, ArbolesHijo).


caminoHoja(t(R,[]), [R]):-!.
caminoHoja(t(R, Hijos), [R | Camino]):-
    member(Hijo, Hijos),
    caminoHoja(Hijo, Camino).
caminoHojaS(t(_R, Hijos), Camino):-
    member(Hijo, Hijos),
    caminoHoja(Hijo, Camino).


caminosPosibles(Color, Prof, Caminos):-
    crearArbol(Color, Prof, Arbol),
    findall(Camino, caminoHojaS(Arbol, Camino), Caminos).

% CB: Completamos el camino (lista vacia)
simularCamino(_Grid, AdyacentesC, [], [], CantAdyacentes):- 
    % !,
    length(AdyacentesC, CantAdyacentes).

% CB: Ganamos el juego con una profundidad menor a la indicada
simularCamino(Grid, AdyacentesC, [C|_Cs] , [C], CantAdyacentes):- 
    flick(Grid, C, AdyacentesC, _FGrid, FAdyacentesC),
    cantCol(X),
    cantFilas(Y),
    length(FAdyacentesC, CantAux),
    CantAux is X*Y,
    length(FAdyacentesC, CantAdyacentes).

% CR: Quedan hojas(colores) por recorrer y no se termino el juego
simularCamino(Grid, AdyacentesC, [C|Cs] , [C|CaminoRecorrido], CantAdyacentes):-
    flick(Grid, C, AdyacentesC, FGrid, FAdyacentesC),
    simularCamino(FGrid, FAdyacentesC, Cs, CaminoRecorrido, CantAdyacentes).


% CB: No hay camino para recorrer
simularTodos(_Grid, _AdyacentesC, [], []).

simularTodos(Grid, AdyacentesC, [Camino|CaminosRest], [Solucion| SolucionesR]):-
    simularCamino(Grid, AdyacentesC, Camino, CaminoRecorrido, CantAdyacentes),
    length(CaminoRecorrido, Long),
    Solucion=[CaminoRecorrido,Long,CantAdyacentes],
    simularTodos(Grid, AdyacentesC, CaminosRest, SolucionesR).

% LCaminos= Lista de todos los caminos posibles
% LSoluciones= Lista de listas [[Camino, CantAdy] | Ls]
help(Grid, AdyacentesC, N, Solucion, CantAdy):-
    AdyacentesC=[A|_],
    getColor(Grid, A, Color),
    caminosPosibles(Color, N, LCaminos),
    simularTodos(Grid, AdyacentesC, LCaminos, LSoluciones),
    !,
    sort(2, @>=, LSoluciones, LFSolucionesAux),
    nth0(0, LFSolucionesAux, Mov),
    nth0(2, Mov, Ady),
    findall(X, (member(X, LFSolucionesAux),nth0(2,X,Y), Y =:= Ady), LFSoluciones), 
    sort(2, @=<, LFSoluciones, LFSoluciones2),
    LFSoluciones2=[MejorCamino|_Otros],
    MejorCamino=[Solucion,_Longitud,CantAdy].


