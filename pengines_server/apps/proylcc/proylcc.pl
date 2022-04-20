:- module(proylcc, 
	[  
		flick/3
	]).

:-use_module(library(lists)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% flick(+Grid, +Color, -FGrid)
%
% FGrid es el resultado de hacer 'flick' de la grilla Grid con el color Color.
% Retorna false si Color coincide con el color de la celda superior izquierda de la grilla. 
% Para cada celda noVisitada ver si sus celdas adyacentes son del mismo color

flick(Grid, Color, adyacentesC, noVisitadas, FGrid):-
	Grid = [F|Fs],
	F = [X|Xs],
	Color \= X,
	FGrid = [[Color|Xs]|Fs],
	findall(Pos, (member(Pos,ListaAdy), Color=:=getColor(Grid,Pos,Color)),adyacentesC).

replace(X, 0, Y, [X|Xs], [Y|Xs]).

replace(X, XIndex, Y, [Xi|Xs], [Xi|XsY]):-
    XIndex > 0,
    XIndexS is XIndex - 1,
    replace(X, XIndexS, Y, Xs, XsY).

getColor(Grid, [X,Y], Color):- 
	nth0(X, Grid, R),
	nth0(Y, R, Color).

setColor(Grid, [], _C, Grid).
setColor(Grid, [X,Y], C, NewGrid):-
	nth0(X, Grid, R),
	nth0(Y, R, Color),
	replace(Color , Y, C, R, NewR),
	replace(R, X, NewR, Grid, NewGrid).
	setColor

adyacentes([X,Y], ListaAdy):-
	 X > 0, 
	 Y > 0, 
	 X < 15, 
	 Y < 15, 
	 XS is X+1,
	 XR is X-1,
     YS is Y+1,
     YR is Y-1,
     ListaAdy= [[XS, Y],[XR,Y],[X,YS],[X,YR]].
	 
	 


