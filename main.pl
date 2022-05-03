/* TDA cardsSet: [Id, Author, Date, Text, Votes, Status, Labels]).
 * 
 * Por defecto: 
 * 	Una nueva pregunta se agrega con el estado "abierta" (estado="abierta")
 * 	Una nueva pregunta se agrega con 0 votos (votos=0)
 * Meta primaria:
 * 	cardsSet(Elements, NumElementos, MaxC, Seed, CS).
 * Metas secundarias: 
 * 	addQuestion (CurrentQuestions, Author, Date, Text, Labels, UpdatedQuestions).  
 *  validateQuestionId(CurrentQuestions, QuestionId).
 * 
*/

% Predicados:
% caso base:

%cardsSet(Elements, NumElementos, MaxC, Seed, CS).


myRandom(Xn, Xn1):-
	AX is 1103515245 * Xn,
	AXC is AX + 12345,
	Xn1 is (AXC mod 2147483647).


cardsSet(_,0,_,_,[]).
%cardsSet(Elements, NumElementos, MaxC, Seed, [Card|RestCard]):-
%	not(NumElementos = 0),
	

generaListaRandoms(0, _, _, []).
generaListaRandoms(Cont, Xi, Hasta, [PrimerNumero|LR2]):-
	not(Cont = 0),
	myRandom(Xi, Xi1),
	ContMenos1 is Cont -1,
	generaListaRandoms(ContMenos1, Xi1, Hasta, LR2),
	PrimerNumero is Xi1 mod Hasta.





% Calcula longuitud de la lista
long([],0).
long([_|Y],S):-long(Y,T), S is T + 1.


generarFirstCard(_,0,_,[]):-!.
generarFirstCard(ListaElement,CantElement,N,[U|Carta]):-
	not(CantElement = 0),
	CantElementMenos1 is CantElement -1,
	elementoN(ListaElement,N,U),
	M is N + 1,
	generarFirstCard(ListaElement,CantElementMenos1,M,Carta),
	!.


generarNCard(_,0,_,_,_,[]).
generarNCard(ListaElement,CantElement,I,J,K,[U|Carta]):-
	CantElement = I,
	CantElementMenos1 is CantElement -1,
	elementoN(ListaElement,K,U),
	generarNCard(ListaElement,CantElementMenos1,I,J,K,Carta),
	!.
generarNCard(ListaElement,CantElement,I,J,K,[U|Carta]):-
	not(CantElement = 0),
	not(CantElement = I),
	CantElementMenos1 is CantElement -1,
	M is K +1,
	L is I -1,
	H is L * J + M,
	elementoN(ListaElement,H,U),
	generarNCard(ListaElement,CantElementMenos1,I,J,M,Carta).


generarNCartas(_,0,_,_,_,[]).
generarNCartas(ListaElement,CantCartas,I,J,K,[Card|Cartas]):-
    not(CantCartas = 0),
    generarNCard(ListaElement,I,I,J,K,Card),
	CantCartasMenos1 is CantCartas -1,
	M is J +1,
    generarNCartas(ListaElement,CantCartasMenos1,I,M,K,Cartas),
    !.




elementoN([X|_],1,X).
elementoN([_|Y],Cont,U):-
	not(Cont = 1),
	ContMenos1 is Cont -1,
	elementoN(Y,ContMenos1,U),
	!.

% Carta enesima
%cardsSetNthCard([],N,[]):-!.
%cardsSetNthCard[X|N],1,X):-!.
%cardsSetNthCard([X|R],N,S):-
	%M is N - 1,
	%cardsSetNthCard(R,M,S).
