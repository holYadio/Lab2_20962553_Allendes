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


generarN2Card(_,0,_,_,_,_,[]).
generarN2Card(ListaElement,CantElement,O,I,J,K,[U|Carta]):-
	CantElement = O,
	CantElementMenos1 is CantElement -1,
	T is I +1,
	elementoN(ListaElement,T,U),
	generarN2Card(ListaElement,CantElementMenos1,O,I,J,K,Carta),
	!.
generarN2Card(ListaElement,CantElement,O,I,J,K,[U|Carta]):-
	not(CantElement = 0),
	not(CantElement = O),
	CantElementMenos1 is CantElement -1,
	M is K +1,
	L is I -1,
	H is ((O - 1) + 2 + ((O - 1) * (K - 1)) + (((L * (K - 1)) + J - 1) mod (O - 1))),
	elementoN(ListaElement,H,U),
	generarN2Card(ListaElement,CantElementMenos1,O,I,J,M,Carta).


generarNCartas(_,0,_,_,_,[]).
generarNCartas(ListaElement,CantCartas,I,J,K,[Card|Cartas]):-
    not(CantCartas = 0),
    generarNCard(ListaElement,I,I,J,K,Card),
	CantCartasMenos1 is CantCartas -1,
	M is J +1,
    generarNCartas(ListaElement,CantCartasMenos1,I,M,K,Cartas),
    !.

generarN2Cartas(_,0,_,_,_,_,[]).
generarN2Cartas(ListaElement,CantCartas,O,I,J,K,[Card|Cartas]):-
    not(CantCartas = 0),
    generarN2Card(ListaElement,O,O,I,J,K,Card),
	CantCartasMenos1 is CantCartas -1,
	M is J +1,
    generarN2Cartas(ListaElement,CantCartasMenos1,O,I,M,K,Cartas),
    !.


join( [], Lista, Lista ).
join( [CabezaL1|RestoL1], Lista2, [CabezaL1|ListaResultado] ) :-
	join( RestoL1, Lista2, ListaResultado ).


generarN3Cartas(_,0,_,_,_,_,_,[]).
generarN3Cartas(ListaElement,CantCartas,G,O,I,J,K,[Cards|Cartas]):-
    not(CantCartas = 0),
    generarN2Cartas(ListaElement,G,O,I,J,K,Cards),
	CantCartasMenos1 is CantCartas -1,
	M is I +1,
    generarN3Cartas(ListaElement,CantCartasMenos1,G,O,M,J,K,Cartas),
    !.


mazoNCartas(ListaElement,CantElement,[FisrtCard|Cards]):-
	CantCartas is CantElement -1,
	generarFirstCard(ListaElement,CantElement,1,FisrtCard),
	generarNCartas(ListaElement,CantCartas,CantElement,1,1,Cards).


mazoN2Cartas(ListaElement,CantElement,[NCards|Cards]):-
	CantCartas is CantElement -1,
	mazoNCartas(ListaElement,CantElement,NCards),
	generarN3Cartas(ListaElement,CantCartas,CantCartas,CantElement,1,1,1,Cards).


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
