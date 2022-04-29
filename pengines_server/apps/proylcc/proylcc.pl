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

<<<<<<< HEAD

=======
>>>>>>> d919c43b269b3a798b9eb173670377591cd42ce9
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
<<<<<<< HEAD


buscarAdyacentes([], []).
buscarAdyacentes([[I,J]], Lf):- adyacentes([I,J], Lf).
buscarAdyacentes([[X,Y]|Ls], NewGridA):-
    adyacentes([X,Y], La),
    buscarAdyacentes(Ls, Lb), 
    append(La,Lb,NewGridA).

flick(Grid, Color,AdyacentesC, FGrid, NewAdyacentesC):-
AdyacentesC = [A|_Ad],
	getColor(Grid, A, C),
	Color \= C,
    setColor(Grid, AdyacentesC, Color, FGrid),
    buscarAdyacentes(AdyacentesC, La),
	findall( [I,J], (member([I,J], La), getColor(Grid,[I,J],Color)),FAdyacentesC),
    append(AdyacentesC,FAdyacentesC, NewAdyacentesC).
=======
>>>>>>> d919c43b269b3a798b9eb173670377591cd42ce9


flick(Grid, Color, AdyacentesC, FGrid, FAdyacentesC):-
	AdyacentesC = [A|_Ad],
	getColor(Grid, A, C),
	Color \= C,
	setColor(Grid, AdyacentesC, Color, FGrid),
	findall( [I,J], (member([I,J], adyacentes([I,J],AdyacentesC)), getColor(Grid,[I,J],Color)),FAdyacentesC).