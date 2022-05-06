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
% set_prolog_flag(answer_write_options,[max_depth(0)]).
%cardsSet(Elements, NumElementos, MaxC, Seed, CS).
/*

	Documentacion.

*/


cardsSet(Elements, NumE, MaxC, _, CS):-
	not(NumE = 0),
	not(var(MaxC)),
	mazoCartas(Elements,NumE,Mazo),
	acotarMazo(Mazo,MaxC,CS),
	!.
cardsSet(Elements, NumE, MaxC, _, CS):-
	not(NumE = 0),
	var(MaxC),
	mazoCartas(Elements,NumE,CS),
	length(CS, MaxC),
	!.


%----------------------- Funciones para construir el CardsSet -----------------------%

generarFirstCard(_,0,_,[]):-!.
generarFirstCard(ListaElement,CantElement,K,[U|Carta]):-
	not(CantElement = 0),
	CantElementMenos1 is CantElement -1,
	elementoN(ListaElement,K,U),
	M is K + 1,
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


generarN3Cartas(_,0,_,_,_,_,_,[]).
generarN3Cartas(ListaElement,CantCartas,G,O,I,J,K,X):-
	not(CantCartas = 0),
	CantCartasMenos1 is CantCartas -1,
	generarN2Cartas(ListaElement,G,O,I,J,K,Cards),
	append(Cards,Cartas,X),
	M is I +1,
	generarN3Cartas(ListaElement,CantCartasMenos1,G,O,M,J,K,Cartas),
	!.


mazoNCartas(ListaElement,CantElement,[FisrtCard|Cards]):-
	CantCartas is CantElement -1,
	generarFirstCard(ListaElement,CantElement,1,FisrtCard),
	generarNCartas(ListaElement,CantCartas,CantElement,1,1,Cards).


mazoCartas(ListaElement,CantElement,Cartas):-
	CantCartas is CantElement -1,
	mazoNCartas(ListaElement,CantElement,CardsN),
	generarN3Cartas(ListaElement,CantCartas,CantCartas,CantElement,1,1,1,OtherCards),
	append(CardsN,OtherCards,Cartas).


cardSetDeleteCard([_|X], 0, X).
cardSetDeleteCard([X|T1], I, [X|T2]):-
	I2 is I - 1,
	cardSetDeleteCard(T1, I2, T2),
	!.


elementoN([X|_],1,X).
elementoN([_|Y],Cont,U):-
	not(Cont = 1),
	ContMenos1 is Cont -1,
	elementoN(Y,ContMenos1,U),
	!.


acotarMazo(_,0,[]).
acotarMazo(Mazo,CantCartas,[Card|MazoAcotado]):-
	not(CantCartas = 0),
	CantCartasMenos1 is CantCartas -1,
	cardSetDeleteCard(Mazo,0,NewMazo),
	elementoN(Mazo,1,Card),
	acotarMazo(NewMazo,CantCartasMenos1,MazoAcotado),
	!.
%------------------------------------------------------------------------------------%
% cardsSetIsDobble

/*

	Documentacion.

*/

cardsSetIsDobble(CS):-
	length(CS,A),
	noRepetido2(CS,A,1).
	

noRepetido(_,_,1,_,A):-
	A =< 1,
	!.
noRepetido(CS,Card1,CantCartas,I,A):-
	not(CantCartas = 1),
	A =< 1,
	M is I + 1,
	elementoN(CS,I,Card2),
	intersection(Card1, Card2, InterCardMazo),
	length(InterCardMazo,A),
	CantCartasMenos1 is CantCartas -1,
	noRepetido(CS,Card1, CantCartasMenos1, M, A),
	!.

noRepetido2(_,1,A):-
	not(A =< 1),
	false.
noRepetido2(_,1,A):-
	A =< 1.
noRepetido2(CS,CantCartas,A):-
	not(CantCartas = 1),
	A =< 1,
	CantCartasMenos1 is CantCartas -1,
	elementoN(CS,1,Card),
	cardSetDeleteCard(CS,0,NewCS),
	noRepetido(NewCS,Card,CantCartasMenos1,1,A),
	noRepetido2(NewCS, CantCartasMenos1, A),
	!.


%------------------------------------------------------------------------------------%
% Carta enesima

/*

	Documentacion.

*/

cardsSetNthCard([],_,[]):-!.
cardsSetNthCard([X|_],0,X):-!.
cardsSetNthCard([_|R],N,S):-
	M is N - 1,
	cardsSetNthCard(R,M,S),
	!.




%------------------------------------------------------------------------------------%
% cardsSetFindTotalCards

/*

	Documentacion.

*/
cardsSetFindTotalCards(Card,TC):-
	length(Card,A),
	TC is (((A - 1) * (A - 1)) + (A - 1) + 1).


%------------------------------------------------------------------------------------%
% cardsSetToString

/*

	Documentacion.

*/
/*

cardsSetToString([],_):-
	!.
cardsSetToString([Card|Resto],String):-
	string_concat()

	Documentacion.

*/

stringCarta(Card,I,String4):-
	atomic_list_concat(Card, " ", StringCard),
	number_string(I, StringI),
	string_concat('Carta ',StringI,String1),
	string_concat(String1,': ',String2),
	string_concat(String2,StringCard,String3),
	string_concat(String3,'\n',String4).

stringCS(_,_,'',0):-!.
stringCS(CS,I,StringFinal,CantCartas):-
	not(CantCartas = 0),
	M is I + 1,
	CantCartasMenos1 is CantCartas - 1,
	elementoN(CS,1,Card),
	cardSetDeleteCard(CS,0,NewCS),
	stringCarta(Card,I,StringCard),
	string_concat(StringCard,StringResto,StringFinal),
	stringCS(NewCS, M, StringResto,CantCartasMenos1),
	!.