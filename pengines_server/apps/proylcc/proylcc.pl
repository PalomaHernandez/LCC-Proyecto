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
    
remove(_, [], []).
remove(C, [C|Xs], Xs).
remove(C, [X|Xs], [X|Zs]):- X \= C, remove(C, Xs, Zs).

listaSinColor(Color, Colors):-
    remove(Color, [r,v,p,g,b,y], Colors).

help(Grid, AdyacentesC, N, Solucion, CantAdyacentes):-
	predicadoOne(Grid,AdyacentesC,N, [], Resultado),
    caminosPosibles(Resultado, CaminosPosibles), !,
    sort(2, @>=, CaminosPosibles, LFSolucionesAux),
    nth0(0, LFSolucionesAux, Mov),
    nth0(1, Mov, Ady),
    findall(X, (member(X, LFSolucionesAux),nth0(1,X,Y), Y== Ady), LFSoluciones),
	sort(2, @=<, LFSoluciones, LFSoluciones2),
    LFSoluciones2=[MejorCamino|_Otros],
    MejorCamino=[Solucion,CantAdyacentes].


caminosPosibles([],[]):-!.
caminosPosibles([L|Ls],[[Movimientos,CantAdy]|Res]):-
	L= [_Grid, Movimientos, Ady],
	length(Ady,CantAdy),
	caminosPosibles(Ls, Res).


predicadoOne(_Grid, _AdyacentesC, 0, _Secuencia, _Resultado):-!.
predicadoOne(Grid, AdyacentesC, N, Secuencia, Resultado):-
    N1 is N -1,
    AdyacentesC=[A|_],
    color(Grid, A, Color),
    listaSinColor(Color, Colors),
    caminoPorNivel(Grid, Colors, AdyacentesC, Secuencia, R),
    predicadoAll(R, N1,Resul),
    append(Resul,R,Resultado).

predicadoAll([],_,_):-!.
predicadoAll(R, N,Resul):-
    R=[L|Ls],
    L=[NewGrid, NewSecuencia, NewAdyacentesC],
    predicadoOne(NewGrid, NewAdyacentesC, N,  NewSecuencia, Res),
    predicadoAll( Ls, N,Re),
    append(Re,Res,Resul).


caminoPorNivel(Grid, Colors, AdyacentesC, Secuencia, Resultado):-
    findall([NewGrid, NewSecuencia, NewAdyacentesC], (member(C, Colors), flick(Grid, 
    C,AdyacentesC, NewGrid, NewAdyacentesC), 
    length(NewAdyacentesC,Long1),length(AdyacentesC,Long2),
    Long1 > Long2,
	append(Secuencia, [C], NewSecuencia)),
    Resultado), !.

