:- module(proylcc, 
	[  
		flick/5
	]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
 % adyCStar(Origin, +Grid, -Res)
 % Calcula el conjunto de celdas adyacentesC* de la celda Origin en la grilla Grid
 % siguiendo una estrategia de propagación o expansión.
 %%

adyCStar(Origin, Grid, Res) :-
    adyCStarSpread(Origin, [], Grid, Res).

%
 % adyCStarSpread(+Pend, +Vis, +Grid, -Res)
 % Pend: por "pendientes", inicialmente es la lista [Origin], y en general es 
 % el conjunto de celdas adyacentesC* a Origin que aún no fueron consideradas.
 % Vis: por "visitados", inicialmente [], son las celdas adyacentesC* a la Origen 
 % que ya fueron consideradas.
 % Grid: idem adyCStar
 % Res: idem adyCStar
 % En cada paso se selecciona una celda de las pendientes, se pasa a visitados, y
 % se agregan a pendientes todas aquellas adyacentes a la celda, del mismo color, que no estén
 % ya ni en pendientes ni visitados.
 %%

adyCStarSpread([], Vis, _Grid, Vis).

adyCStarSpread(Pend, Vis, Grid, Res):-
    Pend = [P|Ps],
    findall(A, 
	        (
    	        adyC(P, Grid, A),
        	    not(member(A, Pend)),
            	not(member(A, Vis))
	        ), 
            AdyCP),
    append(Ps, AdyCP, NPend),
    append(Vis, [P], NVis),
    adyCStarSpread(NPend, NVis, Grid, Res).

%%
 % adyC(+P, +Grid, -A)
 %%

adyC(P, Grid, A):-
    ady(P, Grid, A),
    color(Grid, P, C),
    color(Grid, A, C).

%% 
 % ady(+P, +Grid, -A)
 %

ady([X, Y], Grid, [X1, Y]):-
    length(Grid, L),
    X < L - 1,
    X1 is X + 1.

ady([X, Y], _Grid, [X1, Y]):-
    X > 0,
    X1 is X - 1.

ady([X, Y], Grid, [X, Y1]):-
    Grid = [F|_],
    length(F, L),
    Y < L - 1,
    Y1 is Y + 1.

ady([X, Y], _Grid, [X, Y1]):-
    Y > 0,
    Y1 is Y - 1.


 % 
 % color(Grid, P, C)
 %%

color(Grid, [X,Y], C):-
    nth0(X, Grid, F),
    nth0(Y, F, C).    

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
    color(Grid, A, C),
    Color \= C,
    setColor(Grid, AdyacentesC, Color, FGrid), !,
    adyCStar(AdyacentesC, FGrid, NewAdyacentesC).
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% remove(+C, +List1, -List2)
%
% C es el elemento a eliminar de la lista.
% List1 es la lista que contiene el elemento a eliminar .
% List2 es el resultado de eliminar el elemento C de la lista List1.

remove(_, [], []).
remove(C, [C|Xs], Xs).
remove(C, [X|Xs], [X|Zs]):- X \= C, remove(C, Xs, Zs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  listaSinColor(+Color, -Colors)
%
% Color es el color a eliminar de la lista de colores.
% Colors es el resultado de eliminar el color Color de la lista de colores .

listaSinColor(Color, Colors):-
    remove(Color, [r,v,p,g,b,y], Colors).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  help(+Grid, +AdyacentesC,  +N, -Solucion, -CantAdyacentes
%
% Grid es la grilla actual.
% AdyacentesC es la lista de adyacentesC* actual.
% N es la profundidad ingresada por el usuario.
% Solucion es una lista con la mejor secuencia de movidas con profundidad N de la ayuda % optimal.
% CantAdyacentes es la cantidad de celdas que se capturan al realizar flick con la   
% secuencia de movidas de la lista Solucion.

help(Grid, AdyacentesC, N, Solucion, CantAdyacentes):-
    N > 0,
    helpOne(Grid,AdyacentesC,N, [], Resultado),
    caminosPosibles(Resultado, CaminosPosibles), !,
    sort(2, @>=, CaminosPosibles, SolucionesAux),
    nth0(0, SolucionesAux, Mov),
    nth0(1, Mov, Ady),
    findall(X, (member(X, SolucionesAux),nth0(1,X,Y), Y== Ady), LFSolucionesAux),
    sort(2, @=<, LFSolucionesAux, LFSoluciones),
    LFSoluciones=[MejorCamino|_Otros],
    MejorCamino=[Solucion,CantAdyacentes].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  caminosPosibles(+Lista1, -Lista2)
%
% Lista1 es la lista de listas que contienen una grilla, una lista de movimientos y una lista de % adyacentesC.
% Lista2 es una lista que se forma a partir de la lista de movimientos y la lista de  
% adyacentesC deLista1

caminosPosibles([],[]):-!.
caminosPosibles([L|Ls],[[Movimientos,CantAdy]|Res]):-
    L= [_Grid, Movimientos, Ady],
    length(Ady,CantAdy),
    caminosPosibles(Ls, Res).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  helpOne(+Grid, +AdyacentesC, +N, +Secuencia, -Resultado)
%
% Grid es una grilla.
% AdyacentesC es la lista de celdas capturadas en la grilla Grid.
% N es la profundidad en la que se encuentra la secuencia de jugadas.
% Secuencia es una lista de colores.
% Resultado lista de listas que contiene todas las posibles jugadas incluyendo: grilla, 
% secuencia de colores y cantidad de celdas capturadas.

helpOne(_Grid, _AdyacentesC, 0, _Secuencia, _Resultado):-!.
helpOne(Grid, AdyacentesC, N, Secuencia, Resultado):-
    N1 is N -1,
    AdyacentesC=[A|_],
    color(Grid, A, Color),
    listaSinColor(Color, Colors),
    caminoPorNivel(Grid, Colors, AdyacentesC, Secuencia, R),
    helpAll(R, N1,Resul),
    append(Resul,R,Resultado).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  helpAll(+R, +N, -Resul)
%
% R es una lista de listas que contiene las posibles jugadas incluyendo: grilla, secuencia de % colores y cantidad de celdas capturadas.
% N es la profundidad en la que se encuentra la secuencia de jugadas.
% Resul lista de listas que contiene las posibles jugadas que se encuentran a partir de R.


helpAll([],_,_):-!.
helpAll(R, N,Resul):-
    R=[L|Ls],
    L=[NewGrid, NewSecuencia, NewAdyacentesC],
    helpOne(NewGrid, NewAdyacentesC, N,  NewSecuencia, Res),
    helpAll( Ls, N,Re),
    append(Re,Res,Resul).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  caminoPorNivel(+Grid, +Colors, +AdyacentesC, +Secuencia, -Resultado)
%
% Grid es una grilla.
% Colors es una lista de colores.
% AdyacentesC es la lista de celdas capturadas en la grilla Grid.
% Secuencia es una lista de colores.
% Resultado es una lista de listas, donde cada una de ellas almacena la cantidad de celdas % capturadas, grilla luego de hacer flick de un color, la secuencia de colores actualizada y la % nueva cantidad de celdas capturadas

caminoPorNivel(Grid, Colors, AdyacentesC, Secuencia, Resultado):-
    findall([NewGrid, NewSecuencia, NewAdyacentesC], (member(C, Colors), flick(Grid,
    C,AdyacentesC, NewGrid, NewAdyacentesC),
    length(NewAdyacentesC,Long1),length(AdyacentesC,Long2),
    Long1 > Long2,
    append(Secuencia, [C], NewSecuencia)),
    Resultado), !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  helpGreedy(+Grid, +AdyacentesC,  +N, -Solucion, -CantAdyacentes
%
% Grid es la grilla actual.
% AdyacentesC es la lista de adyacentesC* actual.
% N es la profundidad ingresada por el usuario.
% Solucion es una lista con la mejor secuencia de movidas con profundidad N de la ayuda % greedy.
% CantAdyacentes es la cantidad de celdas que se capturan al realizar flick con la   
% secuencia de movidas de la lista Solucion.


helpGreedy(Grid, AdyacentesC, N, Solucion, CantAdyacentes):-
    N > 0,
    greedyOne(Grid,AdyacentesC,N,[], Resultado),
    Resultado = [CantAdyacentes, _NewGrid, Solucion, _NewAdyacentesC].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  greedyOne(+Grid, +AdyacentesC, +N, +Secuencia, -Resultado)
%
% Grid es una grilla.
% AdyacentesC es la lista de celdas capturadas en la grilla Grid.
% N es la profundidad en la que se encuentra la secuencia de jugadas.
% Secuencia es una lista de colores.
% Resultado lista de listas que contiene la mejor jugada incluyendo su grilla resultante, 
% la secuencia de colores y la cantidad de celdas que se capturan.

greedyOne(Grid, AdyacentesC, N, Secuencia, Resultado):-
    N1 is N -1,
    AdyacentesC=[A|_],
    color(Grid, A, Color),
    listaSinColor(Color, Colors),
    caminoPorNivelAlternativo(Grid, Colors, AdyacentesC, Secuencia, R),
    sort(0, @>=, R, RAux),
    nth0(0, RAux, Mov),
    greedyAll(Mov, N1,Resultado), !.
   

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  greedyAll(+R, +N, -Resul)
%
% R es una lista de listas que contiene la mejor jugada del nivel de profundidad en el que se % encuentra incluyendo su grilla, la secuencia de colores y la cantidad de celdas 
% capturadas.
% N es la profundidad en la que se encuentra la secuencia de jugadas.
% Resul lista de listas que contiene la mejor jugada que se encuentra a partir de R.


greedyAll(R, 0, R):-!.
greedyAll(R, N,Resul):-
    R=[ _LongitudAdy, NewGrid, NewSecuencia, NewAdyacentesC],
    greedyOne(NewGrid, NewAdyacentesC, N,  NewSecuencia, Resul).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  caminoPorNivel(+Grid, +Colors, +AdyacentesC, +Secuencia, -Resultado)
%
% Grid es una grilla.
% Colors es una lista de colores.
% AdyacentesC es la lista de celdas capturadas en la grilla Grid.
% Secuencia es una lista de colores.
% Resultado es una lista de listas, donde cada una de ellas almacena la cantidad de celdas % capturadas, la longitud de la lista de celdas capturadas, la grilla luego de hacer flick de un % color, la secuencia de colores actualizada y la nueva cantidad de celdas capturadas

caminoPorNivelAlternativo(Grid, Colors, AdyacentesC, Secuencia, Resultado):-
    findall([Long1,NewGrid, NewSecuencia, NewAdyacentesC], (member(C, Colors), flick(Grid,
    C,AdyacentesC, NewGrid, NewAdyacentesC),
    length(NewAdyacentesC,Long1),length(AdyacentesC,Long2),
    Long1 > Long2,
    append(Secuencia, [C], NewSecuencia)),
    Resultado), !.

