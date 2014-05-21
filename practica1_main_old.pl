%
% Álvaro Sanz Sanz
% Pedro Palazón Martínez de Alegría

% Cargamos las reglas iniciales dadas por el profesor.

:- consult('./practica1.pl').
:- use_module(library(clpfd)).

% Cargamos los Sudokus ejemplo
:- consult('./ejemplos.pl').

easyTest(X):- ejemploSudokuFacil(X,L),
		time(resolverSudoku(L)).

hardTest(X):- ejemploSudokuDificil(X,L),
		time(resolverSudoku(L)).



resolverSudoku(L) :-
      nl,write('***************** Sudoku Original *****************'),
	  representarSUDOKU(L),
      nl,write('*************** Sudoku posibilidades **************'),		
	  generarPosibilidadesSUDOKU(L,SIn),
	  representarSUDOKU(SIn),
      simplificarSudoku(SIn,[]).


simplificarSudoku(Sudoku,Sudoku_posibilidades_mod):-
	representarSUDOKU(Sudoku),nl,
	generarPosibilidadesSUDOKU(Sudoku,Sudoku_posibilidades),
	representarSUDOKU(Sudoku_posibilidades),nl,
	regla0(Sudoku_posibilidades,L),
	regla1(L,L1),
	regla2(L,L2),
	regla3(L2,Sudoku_posibilidades_mod),
	representarSUDOKU(Sudoku_posibilidades_mod).




%caso base de la recursion
simplificar_sudoku(SIn,SAnt):-
          SIn = SAnt,
          nl,write('**************** Sudoku Solucion ****************'),		
		  representarSUDOKU(SIn).

simplificar_sudoku(SIn,SAnt):-
          SIn \= SAnt,
          %nl,write('*************** Sudoku Tras Regla 0 **************'),		
		  regla0(SIn,B),
		  %representarSUDOKU(B),
          %nl,write('*************** Sudoku Tras Regla 1 **************'),		
		  regla1(B,C),
		  %representarSUDOKU(C),
          %nl,write('*************** Sudoku Tras Regla 2 **************'),		
		  regla2(C,D), 
		  %representarSUDOKU(D), nl,
          %nl,write('*************** Sudoku Tras Regla 3 **************'),		
		  regla3(D,E),
		  %representarSUDOKU(E), nl,
          simplificar_sudoku(E,SIn).



% #######################################################################################	
% Métodos para imprimir el formato de sudoku como lineas y columnas.
% CORRECTA: Comprobada Álvaro.
% #######################################################################################
imprimirElementoAux(X,0):- nl, write(X), write('|').
imprimirElementoAux(X,_):- write(X), write('|').
imprimirElemento(X,Y):- 
	T is mod(X, 9),
	imprimirElementoAux(Y,T).
% ---------------------------------------------------------------------------------------

% #######################################################################################
% Método para representar un Sudoku, da formato 9x9 en base al sudoku/lista dada en 
%	Sudoku.
% CORRECTA: Comprobada Álvaro.
% #######################################################################################
representarSUDOKU(Sudoku):- representarSUDOKU(Sudoku,0).

representarSUDOKU([_], 1).

representarSUDOKU([],_).

representarSUDOKU([N|Sudoku], Posicion):-
	imprimirElemento(Posicion,N),
	NuevaPosicion is Posicion + 1,
	representarSUDOKU(Sudoku, NuevaPosicion).
% ---------------------------------------------------------------------------------------

% #######################################################################################
% Predicado que genera a partir de una lista de entrada, una nueva lista con las 
% posibilidades de los sitios vacios del sudoku.
% Funcionalidad:
%	Recorre todos los elementos de la lista Sudoku.
%	Generar la lista de posibles para cada posición vacía ('.').
%	La incorporo a la lista SudokuPosibilidades
%
% SALIDA:debe ser la matriz de sudoku con las posibilidades de cada posición de las 9x9
%	, con las diferentes posibilidades.
% CORRECTA: Comprobada Álvaro.
% #######################################################################################
generarPosibilidadesSUDOKU(Sudoku, SudokuPosibilidades):-
	generarPosibilidadesAux(Sudoku, [], SudokuPosibilidades, 1).

generarPosibilidadesAux(_, X, SudokuPosiblidades, 82):-
	reverse(X, SudokuPosiblidades).

generarPosibilidadesAux(Sudoku, Entrada, SudokuPosiblidades, Posicion):-
	nth1(Posicion,Sudoku,X),
	integer(X),
	Posicion1 is Posicion + 1,
	generarPosibilidadesAux(Sudoku, [X|Entrada], SudokuPosiblidades, Posicion1).

generarPosibilidadesAux(Sudoku,Entrada,SudokuPosiblidades,Posicion):-
	posibilidadesParaLugar(Sudoku, Posicion, L1),
	Posicion1 is Posicion + 1,
	generarPosibilidadesAux(Sudoku,[L1|Entrada],SudokuPosiblidades, Posicion1).

posibilidadesParaLugar(Sudoku, Posicion, Candidatos):-
	conflictivos(Posicion, LConflictivos),
	generarListaLugares(Sudoku, LConflictivos, ValoresConflictivos),
	generarListaPosibles(ValoresConflictivos, Candidatos).
% ---------------------------------------------------------------------------------------

% #######################################################################################
% Predicado que genera la lista de posibles a partir de una lista de conflictivos.
% CORRECTA: Comprobada Pedro.
% #######################################################################################
generarListaPosibles(ListaConflictivos, ListaCandidatos):-
	generarListaPosiblesAux(ListaConflictivos, [], ListaCandidatos).

generarListaPosiblesAux([N|ListaConflictivos], ListaEntrada, ListaCandidatos):-
	union([N], ListaEntrada, X),
	generarListaPosiblesAux(ListaConflictivos, X, ListaCandidatos).

generarListaPosiblesAux([],ListaEntrada, ListaCandidatos):- 
	subtract([1,2,3,4,5,6,7,8,9],ListaEntrada,ListaCandidatos).

% ---------------------------------------------------------------------------------------

% #######################################################################################
% Predicado que extrae de una lista L, el valor de todas las posiciones indicadas por la 
% lista L1, dando como resultado la lista L2.
% CORRECTA: Hecha en clase
% #######################################################################################
% Info PROLOG: Si da warning singleton de la variable solo substituye L por _
%ej.: generar-lista-lugares([a,b,c,d], [3,1], X).
%ej.: generar-lista-lugares([a,.,c,d], [3,1], X).
generarListaLugares(L,L1,L2):-
	generarListaLugaresAux(L, L1, [], L2).

generarListaLugaresAux(L,[N|L1], Z, L2):-
	nth1(N,L,X),
	generarListaLugaresAux(L, L1, [X|Z], L2).

generarListaLugaresAux(_,[],Z,L2):- 
	reverse(Z,L2).
% ---------------------------------------------------------------------------------------

% #######################################################################################
% REGLA 0:
% Si hay un lugar donde solo cabe un número, lo escribimos en el lugar correspondiente y lo 
% eliminamos de los lugares en los que aparezca de los que son conflictivos.
% CORRECTA: Comprobado Pedro.
% Resuelve los Sudokus Faciles (1-4) y Simplica los dificiles (5-7)
% #######################################################################################
% determinar si es lista o numero.
% si número, lo dejamos como está.
% si es lista, 
% 	[Si la lista solo contiene 1 numero, eliminar número de los lugares conflictivos.] 
%	[Si la lista contiene más de uno, lo dejamos].

%regla inicial
regla0(SudokuIn,SudokuRegla0):-	
	append([],SudokuIn,L1),
	append([],SudokuIn,L2),
	append([],[],L3),	
	regla0_aux(L1,L2,L3,LOut,1),
	append([],LOut,SudokuRegla0).

% caso base de la regla auxiliar
regla0_aux([],L2,L3,LOut,Pos):- 
	reverse(L3,LOut).

% caso recursivo es un elemento
regla0_aux([X|L1],L2,L3,LOut,Pos):-
	integer(X),
	Pos1 is Pos + 1,
    regla0_aux(L1,L2,[X|L3],LOut,Pos1).

% caso recursivo es una lista mayor a 1
regla0_aux([X|L1],L2,L3,LOut,Pos):- 
	is_list(X),
	length(X,Tam),
	Tam > 1,
	Pos1 is Pos + 1,
	regla0_aux(L1,L2,[X|L3],LOut,Pos1).

%caso backtracking es una lista de un elemento
%modificamos el sudoku y volvemos a analizar desde el principio
regla0_aux([X|_],L2,L3,LOut,Pos):-   
	is_list(X),
	proper_length(X,1),
	append([],[],L4),
	aplicaregla0(X,L2,L4,L5,Pos,1),
    regla0_aux(L5,L5,[],LOut,1).

%caso base de la aplicacion de regla
aplicaregla0(_,_,L4,L5,_,82):- 
    reverse(L4,L5).

%caso recursivo pertence a los conflictivos y es una lista
aplicaregla0([X|L],L2,L4,L5,Pos1,Pos2):- 
    conflictivos(Pos1, Conflictivos),
    member(Pos2, Conflictivos),
    nth1(Pos2,L2,Y),
    is_list(Y),
	delete(Y,X,LAux),
	Pos3 is Pos2 + 1,
    aplicaregla0([X|L],L2,[LAux|L4],L5,Pos1,Pos3).

%caso recursivo es la posicion pivote, hay que convertir en elemento    
aplicaregla0([X|L],L2,L4,L5,Pos1,Pos2):-
	Pos1 =:= Pos2,
	Pos3 is Pos2 + 1,
	aplicaregla0([X|L],L2,[X|L4],L5,Pos1,Pos3).

%caso recursivo cuando no es una posicion conflictiva
aplicaregla0([X|L],L2,L4,L5,Pos1,Pos2):-
    nth1(Pos2,L2,Y),
	Pos3 is Pos2 + 1,
    aplicaregla0([X|L],L2,[Y|L4],L5,Pos1,Pos3).
% ---------------------------------------------------------------------------------------

% #######################################################################################
% REGLA 1:
% Si hay un número que aparece en una sola de las listas que aparecen en una fila, 
%  columna o cuadro, cambiamos la lista por el número y borramos el número del resto de 
%  listas de la fila, columna o cuadro.
% #######################################################################################
% regla1Test(1).
% Predicado de la regla 1.
regla1(Sudoku, Sudoku_regla1):-
	%nl,write('aplica fila'),
	aplicaregla1_fila(Sudoku, SudokuTemp),
	%nl,representarSUDOKU(SudokuTemp),
	regla1(SudokuTemp, Sudoku_regla1).
regla1(Sudoku, Sudoku_regla1):-
	%nl,write('aplica columna'),
	aplicaregla1_columna(Sudoku, SudokuTemp),
	%nl,representarSUDOKU(SudokuTemp),
	regla1(SudokuTemp, Sudoku_regla1).
regla1(Sudoku, Sudoku_regla1):-
	%nl,write('aplica cuadro'),
	aplicaregla1_cuadro(Sudoku, SudokuTemp),
	%nl,representarSUDOKU(SudokuTemp),
	regla1(SudokuTemp, Sudoku_regla1).
regla1(Sudoku, Sudoku_regla1):-
	append([],Sudoku, Sudoku_regla1).

aplicaregla1_fila(Sudoku, SudokuTemp):-
	fila(FCC),
	generarListaLugares(Sudoku, FCC, ListaLugares),
	unifica(ListaLugares, E),
	mirar(E, ListaLugares, 1, N),
	locate_position(ListaLugares, FCC, N, IList),
	%nl,write('fil Elemento unico'), write(N),write('rehacer sudoku'),write(IList),
	redo_sudoku(fi,Sudoku, N, IList, SudokuTemp).
aplicaregla1_columna(Sudoku, SudokuTemp):-
	columna(FCC),
	generarListaLugares(Sudoku, FCC, ListaLugares),
	unifica(ListaLugares, E),
	mirar(E, ListaLugares, 1, N),
	locate_position(ListaLugares, FCC, N, IList),
	%nl,write('col Elemento unico'), write(N),write('rehacer sudoku'),write(IList),
	redo_sudoku(co,Sudoku, N, IList, SudokuTemp).
aplicaregla1_cuadro(Sudoku, SudokuTemp):-
	cuadro(FCC),
	generarListaLugares(Sudoku, FCC, ListaLugares),
	unifica(ListaLugares, E),
	mirar(E, ListaLugares, 1, N),
	locate_position(ListaLugares, FCC, N, IList),
	%nl,write('cua Elemento unico'),write(N),write('rehacer sudoku'),write(IList),
	redo_sudoku(cu,Sudoku, N, IList, SudokuTemp).

mirar(E, ListaLugares, Veces, R):-
	validos(V),
	count(V, E, N),
	N =:= Veces,
	\+member(V, ListaLugares),
	R is V.

% Compruebo si E ya esta seleccionado.


% Eliminamos el Elemento E, encontrado en Posicion de la fila o columna o cuadro.
redo_sudoku(fi,Sudoku, E, Posicion, Sudoku_nuevo):-
	%columna(X), member(Posicion, X),
	%cuadro(Y), member(Posicion, Y),
	%append(X,Y,Ilist),
	conflictivos(Posicion,Ilist),
	redo_sudoku_aux(Sudoku, E, Posicion, 1, Ilist, [], Sudoku_nuevo).

redo_sudoku(co,Sudoku, E, Posicion, Sudoku_nuevo):-
	%fila(X), member(Posicion, X),
	%cuadro(Y), member(Posicion, Y),
	%append(X,Y,Ilist),
	conflictivos(Posicion,Ilist),
	redo_sudoku_aux(Sudoku, E, Posicion, 1, Ilist, [], Sudoku_nuevo).

redo_sudoku(cu,Sudoku, E, Posicion, Sudoku_nuevo):-
	%fila(X), member(Posicion, X),
	%columna(Y), member(Posicion, Y),
	%append(X,Y,Ilist),
	conflictivos(Posicion,Ilist),
	redo_sudoku_aux(Sudoku, E, Posicion, 1, Ilist, [], Sudoku_nuevo).

redo_sudoku_aux([], E, Posicion, P, Ilist, Sudoku_nuevo, SudokuFinal):-
	reverse(Sudoku_nuevo, SudokuFinal).
redo_sudoku_aux([S|Sudoku], E, Posicion, P, Ilist, Sudoku_nuevo, SudokuFinal):-
	P =:= Posicion,
	P1 is P + 1,
	redo_sudoku_aux(Sudoku, E, Posicion, P1, Ilist, [E|Sudoku_nuevo], SudokuFinal).
redo_sudoku_aux([S|Sudoku], E, Posicion, P, Ilist, Sudoku_nuevo, SudokuFinal):-
	member(P, Ilist),
	P1 is P + 1,
	eliminar_valor(S, E, [], L1),
	redo_sudoku_aux(Sudoku, E, Posicion, P1, Ilist, [L1|Sudoku_nuevo], SudokuFinal).
redo_sudoku_aux([S|Sudoku], E, Posicion, P, Ilist, Sudoku_nuevo, SudokuFinal):-
	is_list(S),
	P1 is P + 1,
	redo_sudoku_aux(Sudoku, E, Posicion, P1, Ilist, [S|Sudoku_nuevo], SudokuFinal).
redo_sudoku_aux([S|Sudoku], E, Posicion, P, Ilist, Sudoku_nuevo, SudokuFinal):-
	integer(S),
	P1 is P + 1,
	redo_sudoku_aux(Sudoku, E, Posicion, P1, Ilist, [S|Sudoku_nuevo], SudokuFinal).



eliminar_valor([], E, L1, Vals):-
%	length(L1, X),
	%X > 1,
	reverse(L1, Vals).
%eliminar_valor([], E, L1, Vals):-
%	length(L1, X),
%	X =:= 1,
%	nth1(1,L1,Vals).
eliminar_valor([I|S], E, L1, Vals):-
	I =:= E,
	eliminar_valor(S, E, L1, Vals).
eliminar_valor([I|S], E, L1, Vals):-
	eliminar_valor(S, E, [I|L1], Vals).



locate_position(Lista, FCC, Elem, Posicion):-
	locate_position_aux(Lista, Elem, 1, Pos),
	nth1(Pos, FCC, Posicion).

locate_position_aux([], Elem, X, Posicion).
locate_position_aux([L|Lista], Elem, X, Posicion):-
	is_list(L),
	locate_position_aux1(L, Elem, X, Posicion),
	Posicion1 is X + 1,
	locate_position_aux(Lista, Elem, Posicion1, Posicion).

locate_position_aux([L|Lista], Elem, X, Posicion):-
	integer(L),
	Posicion1 is X + 1,
	locate_position_aux(Lista, Elem, Posicion1, Posicion).

locate_position_aux1([], Elem, X, Posicion).
locate_position_aux1([L|Lista], Elem, X, Posicion):-
	integer(L),
	L =:= Elem,
	Posicion is X.
locate_position_aux1([L|Lista], Elem, X, Posicion):-
	integer(L),
	locate_position_aux1(Lista, Elem, X, Posicion).

%fila(X),member(5,X),cuadro(Y),member(5,Y)

count(_, [], 0) :- !. /* lista vacia, caso base. */
count(X, [X|T], N) :- /* Si X esta en la cabecera de la lista*/
    count(X, T, N2), /* cuenta desde la cola (dejando como N2) */
    N is N2 + 1.     /* y N es N2 + 1  */
count(X, [Y|T], N) :- 
    X \= Y,          /* if X is not in the head */
    count(X, T, N).  /* just count the rest */


replace([],_,_,[]).
replace(L1, I, Nv, L2):-
	replace_aux(L1, I, Nv, 1, [], L2).
replace_aux([], _, _, _, Temp, L2):-
	reverse(Temp, L2).
replace_aux([A|L1], I, Nv, Pos, Temp, L2):-
	I =:= Pos,
	Pos1 is Pos + 1,
	replace_aux(L1, I, Nv, Pos1, [Nv|Temp], L2).
replace_aux([A|L1], I, Nv, Pos, Temp, L2):-
	Pos1 is Pos + 1,
	replace_aux(L1, I, Nv, Pos1, [A|Temp], L2).


%	unificar_listas(ListaLugares,[],L).
	
%(x, lista, natural)
% Elemento X que aparece N veces.
unifica(Lista, X):-
	unifica_aux(Lista, [], X).

unifica_aux([], L1, T):-
	reverse(L1,T).
unifica_aux([L|Lista], L1, T):-
	is_list(L),
	unifica_aux1(L, L1, Aux),
	unifica_aux(Lista, Aux, T).
unifica_aux([L|Lista], L1, T):-
	integer(L),
	unifica_aux(Lista, L1, T).

unifica_aux1([E|L1], Lista, T):-
	integer(E),
	unifica_aux1(L1, [E|Lista], T).
unifica_aux1([], L, T):-
	append([],L,T).


validos(1).
validos(2).
validos(3).
validos(4).
validos(5).
validos(6).
validos(7).
validos(8).
validos(9).

% ---------------------------------------------------------------------------------------

% #######################################################################################
% REGLA 2:
% Si dos números aparecen solos en dos lugares distintos de una fila, columna o cuadro, 
%  los borramos del resto de lugares de la fila, columna o cuadro correspondiente.
% #######################################################################################
% ---------------------------------------------------------------------------------------
regla2(SudokuIn,SudokuOut):-
    regla2_auxRecur(SudokuIn,[],SOut),
    append([],SOut,SudokuOut).            

regla2_auxRecur(SudokuIn,SudokuAnt,SudokuOut):-
  SudokuIn \= SudokuAnt,
  append([],SudokuIn,LAux0),
  regla2_aux(LAux0,[],LAux1,1,columna),
  regla2_aux(LAux1,[],LAux2,1,fila),
  regla2_aux(LAux2,[],LAux3,1,cuadro),
  regla2_auxRecur(LAux3,SudokuIn,SudokuOut).

regla2_auxRecur(SudokuIn,SudokuAnt,SudokuOut):-
  SudokuIn = SudokuAnt,
  append([],SudokuIn,SudokuOut).            

% caso base de la recursion
regla2_aux(L1,L2,LOut,82,Tipo):-
   reverse(L2,LOut). 

% caso recursivo con backtracking
regla2_aux(L1,L2,LOut,Pos,Tipo):-
	nth1(Pos,L1,X),   %este es mi pivote
	is_list(X),
	proper_length(X,2),
	elemento(Pos,Confli,Tipo),
	append([],[],LAux),
    valores(Confli,L1,LAux,L3), 
    nroveces(L3,X,0,Nro),
    Nro=:=2,
    reemplaza(Confli,L1,LAux,L4,X,N),
    N >= 1,    
    regla2_aux(L4,[],LOut,1,Tipo).	

% caso recursivo sin backtracking. No existen cambios
regla2_aux(L1,L2,LOut,Pos,Tipo):-
	nth1(Pos,L1,X),   %este es mi pivote
	is_list(X),
	proper_length(X,2),
	elemento(Pos,Confli,Tipo),
    append([],[],LAux),    
    valores(Confli,L1,LAux,L3),
    nroveces(L3,X,0,Nro),
    Nro =:= 2,
    reemplaza(Confli,L1,LAux,L4,X,N),
    N =:= 0,
    Pos1 is Pos +1 ,
    regla2_aux(L1,[X|L2],LOut,Pos1,Tipo).	

% caso recursivo 
regla2_aux(L1,L2,LOut,Pos,Tipo):-
	nth1(Pos,L1,X),
	Pos1 is Pos + 1,
    regla2_aux(L1,[X|L2],LOut,Pos1,Tipo).

valores([],L1,LAux,L3):-
	reverse(LAux,L3).

valores([Pos|Confli],L1,LAux,L3):-
	nth1(Pos,L1,Y),
	valores(Confli,L1,[Y|LAux],L3).

nroveces([],X,NroAux,Nro):-
	Nro is NroAux.

nroveces([Z|L3],X,NroAux,Nro):-
 	Z = X,
 	NroAux1 is NroAux +1,
 	nroveces(L3,X,NroAux1,Nro).

nroveces([Z|L3],X,NroAux,Nro):-
	nroveces(L3,X,NroAux,Nro).

reemplaza(Confli,L1,LAux,L4,X,N):-
    NAux is 0,
	reemplaza_aux(Confli,L1,LAux,L4,X,1,NAux,N).

%caso base de recursividad
reemplaza_aux(Confli,L1,LAux,L4,X,82,NAux,N):-
	reverse(LAux,L4),
	N is NAux. 

reemplaza_aux(Fi,L1,LAux,L4,X,82,N):-
	reverse(LAux,L4). 

reemplaza_aux(Confli,L1,LAux,L4,X,Pos,NAux,N):-
	member(Pos,Confli),
	nth1(Pos,L1,Y),
	is_list(Y),
	Y = X,
	Pos1 is Pos + 1,
	reemplaza_aux(Confli,L1,[X|LAux],L4,X,Pos1,NAux,N).

%reemplaza_aux(Confli,L1,LAux,L4,X,Pos,NAux,N):-
%	member(Pos,Confli),
%	nth1(Pos,L1,Y),
%	is_list(Y),
%	Y \= X,
%	subtract(Y,X,Z),	
%	proper_length(Z,1),
%	nth1(1,Z,ZElem),
%	Pos1 is Pos + 1,
%	NAux1 is NAux + 1,
%	reemplaza_aux(Confli,L1,[ZElem|LAux],L4,X,Pos1,NAux1,N).

reemplaza_aux(Confli,L1,LAux,L4,X,Pos,NAux,N):-
	member(Pos,Confli),
	nth1(Pos,L1,Y),
	is_list(Y),
	Y \= X,
	subtract(Y,X,Z),
	Y \= Z,	
	Pos1 is Pos + 1,
	NAux1 is NAux + 1,
	reemplaza_aux(Confli,L1,[Z|LAux],L4,X,Pos1,NAux1,N).

reemplaza_aux(Confli,L1,LAux,L4,X,Pos,NAux,N):-
	member(Pos,Confli),
	nth1(Pos,L1,Y),
	is_list(Y),
	Y \= X,
	subtract(Y,X,Z),
	Y = Z,	
	Pos1 is Pos + 1,
	reemplaza_aux(Confli,L1,[Z|LAux],L4,X,Pos1,NAux,N).

reemplaza_aux(Confli,L1,LAux,L4,X,Pos,NAux,N):-
	member(Pos,Confli),
	nth1(Pos,L1,Y),
	integer(Y),
	Pos1 is Pos + 1,
	reemplaza_aux(Confli,L1,[Y|LAux],L4,X,Pos1,NAux,N).

reemplaza_aux(Fi,L1,LAux,L4,X,Pos,NAux,N):-
	nth1(Pos,L1,Y),
	Pos1 is Pos + 1,
	reemplaza_aux(Fi,L1,[Y|LAux],L4,X,Pos1,NAux,N).

elementos([fila,columna,cuadro]).

elemento(X,Y,Relacion):-
       elementos(L),
       member(Relacion,L),
       Z=..[Relacion,X,Y],
       call(Z).

columna(Posicion,Lista):-
	columna(Lista),member(Posicion,Lista).

fila(Posicion,Lista):-
	fila(Lista),member(Posicion,Lista).

cuadro(Posicion,Lista):-
	cuadro(Lista),member(Posicion,Lista).

% #######################################################################################
% REGLA 3:
% Si en tres lugares de una fila, columna o cuadro sólo aparecen tres números distintos, 
%  borramos los números de las restantes listas de la fila, columna o cuadro.
% #######################################################################################
regla3(SudokuIn,SudokuOut):-
    regla3_auxRecur(SudokuIn,[],SOut),
    append([],SOut,SudokuOut).            

regla3_auxRecur(SudokuIn,SudokuAnt,SudokuOut):-
  SudokuIn \= SudokuAnt,
  append([],SudokuIn,LAux0),
  regla3_aux(LAux0,[],LAux1,1,columna),
  regla3_aux(LAux1,[],LAux2,1,fila),
  regla3_aux(LAux2,[],LAux3,1,cuadro),
  regla3_auxRecur(LAux3,SudokuIn,SudokuOut).

regla3_auxRecur(SudokuIn,SudokuAnt,SudokuOut):-
  SudokuIn = SudokuAnt,
  append([],SudokuIn,SudokuOut).            

% caso base de la recursion
regla3_aux(L1,L2,LOut,82,Tipo):-
   reverse(L2,LOut). 

% caso recursivo con backtracking
regla3_aux(L1,L2,LOut,Pos,Tipo):-
	nth1(Pos,L1,X),   %este es mi pivote
	is_list(X),
	proper_length(X,3),
	elemento(Pos,Confli,Tipo),
	append([],[],LAux),
    valores(Confli,L1,LAux,L3), 
    nroveces(L3,X,0,Nro),
    Nro=:=3,
    reemplaza(Confli,L1,LAux,L4,X,N),
    N >= 1,    
    regla3_aux(L4,[],LOut,1,Tipo).	

% caso recursivo sin backtracking. No existen cambios
regla3_aux(L1,L2,LOut,Pos,Tipo):-
	nth1(Pos,L1,X),   %este es mi pivote
	is_list(X),
	proper_length(X,3),
	elemento(Pos,Confli,Tipo),
    append([],[],LAux),    
    valores(Confli,L1,LAux,L3),
    nroveces(L3,X,0,Nro),
    Nro =:= 3,
    reemplaza(Confli,L1,LAux,L4,X,N),
    N =:= 0,
    Pos1 is Pos +1 ,
    regla3_aux(L1,[X|L2],LOut,Pos1,Tipo).	

% caso recursivo 
regla3_aux(L1,L2,LOut,Pos,Tipo):-
	nth1(Pos,L1,X),
	Pos1 is Pos + 1,
    regla3_aux(L1,[X|L2],LOut,Pos1,Tipo).

% ---------------------------------------------------------------------------------------
