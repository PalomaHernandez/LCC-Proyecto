:- module(proylcc, 
	[  
		flick/5
	]).

:-use_module(library(lists)).

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

buscarAdyacentesC(_,[],_, _, []).
buscarAdyacentesC(Grid,[[I,J]|L],Color, Visitados, Lf):- 
                adyacentes([I,J], La),
               findall( [Y,X], (member([Y,X], La), getColor(Grid,[Y,X],Color), not(member([Y,X],Visitados))), Ls),
               append(Visitados, La, V), 
                buscarAdyacentesC(Grid,Ls, Color, V, Lm),
                append(Ls,Lm,Lh),
                buscarAdyacentesC(Grid,L, Color, V, Lt),
                append(Lh,Lt,Lf).

flick(Grid, Color,AdyacentesC, FGrid, NewAdyacentesC):-
    AdyacentesC = [A|_Ad],
    getColor(Grid, A, C),
    Color \= C,
    setColor(Grid, AdyacentesC, Color, FGrid),
    buscarAdyacentesC(FGrid,AdyacentesC, Color, AdyacentesC, La),
    append(AdyacentesC,La, NewAdyacentesCF),
    sort(NewAdyacentesCF,NewAdyacentesC).