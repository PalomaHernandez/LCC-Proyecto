:- module(proylcc, 
	[  
		flick/5
	]).

:-use_module(library(lists)).
:- dynamic visitados/1.

visitar([]).
visitar([X|Xs]):- assert(visitados(X)), 
    			visitar(Xs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% flick(+Grid, +Color, -FGrid)
%
% FGrid es el resultado de hacer 'flick' de la grilla Grid con el color Color.
% Retorna false si Color coincide con el color de la celda superior izquierda de la grilla. 
% Para cada celda noVisitada ver si sus celdas adyacentes son del mismo color

replace(X, 0, Y, [X|Xs], [Y|Xs]).

replace(X, XIndex, Y, [Xi|Xs], [Xi|XsY]):-
    XIndex > 0,
    XIndexS is XIndex - 1,
    replace(X, XIndexS, Y, Xs, XsY).

getColor(Grid, [X,Y], Color):- 
	nth0(X, Grid, R),
	nth0(Y, R, Color).

adyacentes([X,Y], ListaAdy):-
	 XS is X+1,
	 XR is X-1,
     YS is Y+1,
     YR is Y-1,
     ListaAdy= [[XS, Y],[XR,Y],[X,YS],[X,YR]].
	 

setColor(Grid, [], _C, Grid).
setColor(Grid, [[X,Y]|Ls], C, NewGridA):-
	nth0(X, Grid, R),
	nth0(Y, R, Color),
	replace(Color , Y, C, R, NewR),
	replace(R, X, NewR, Grid, NewGrid),
	setColor(NewGrid,Ls,C,NewGridA).

buscarAdyacentesC(_,[],_ , []).
buscarAdyacentesC(Grid,[[I,J]|L],Color, Lf):- 
                adyacentes([I,J], La),
                findall( [Y,X], (member([Y,X], La), getColor(Grid,[Y,X],Color), not(visitados([Y,X]))), Ls),
                visitar(La),
                buscarAdyacentesC(Grid,Ls, Color, Lm),
                append(Ls,Lm,Lh),
                buscarAdyacentesC(Grid,L, Color, Lt),
                append(Lh,Lt,Lf).

flick(Grid, Color,AdyacentesC, FGrid, NewAdyacentesC):-
    AdyacentesC = [A|_Ad],
    getColor(Grid, A, C),
    Color \= C,
    setColor(Grid, AdyacentesC, Color, FGrid),
    visitar(AdyacentesC),
    buscarAdyacentesC(FGrid,AdyacentesC, Color, La),
    retractall(visitados(_)),
    append(AdyacentesC,La, NewAdyacentesC).