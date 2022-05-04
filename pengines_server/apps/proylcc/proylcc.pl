:- module(proylcc, 
	[  
		flick/5
	]).

:-use_module(library(lists)).
:- dynamic visitados/1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
%
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
% adyacentes(Lista, ListaAdy)    
%
% Lista es una lista de dos elementos que representa una coordenada de una grilla.
% ListaAdy es el resultado de calcular los adyacentes de esa coordenada en una grilla.

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
% Color es el color que se le debe asignar a las posiciones pasadas en la lista
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
% AdyacentesC es la lista de adyacentes actual.
% Color es el color de los adyacentes de la lista AdyacentesC.
% Lf es el resultado de buscar los adyacentesC de la lista AdyacentesC 

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
% AdyacentesC es la lista de adyacentes actual.
% FGrid es el resultado de hacer 'flick' de la grilla Grid con el color Color.
% NewAdyacentesC es la lista actualizada de adyacentes.
% Retorna false si Color coincide con el color del primer elemento de la lista de adyacentes (celda origen).
              

flick(Grid, Color,AdyacentesC, FGrid, NewAdyacentesC):-
    AdyacentesC = [A|_Ad],
    getColor(Grid, A, C),
    Color \= C,
    setColor(Grid, AdyacentesC, Color, FGrid),
    visitar(AdyacentesC),
    buscarAdyacentesC(FGrid,AdyacentesC, Color, La),
    retractall(visitados(_)),
    append(AdyacentesC,La, NewAdyacentesC).