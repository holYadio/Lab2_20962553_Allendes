		% -          TDA card             - %
/* Representacion:
 Lista de n elementos
*/

% Constructor de la primera carta: Contruye la primera carta
% Dom: ListaElementos X CantidadElementos X Contador X Carta generada.
generarFirstCard(_,0,_,[]):-!.
generarFirstCard(ListaElement,CantElement,K,[U|Carta]):-
	not(CantElement = 0),
	CantElementMenos1 is CantElement -1,
	elementoN(ListaElement,K,U),
	M is K + 1,
	generarFirstCard(ListaElement,CantElementMenos1,M,Carta),
	!.


% Constructor de las siguientes n-1 cartas: Contruye las siguientes N-1 cartas.
% Dom: ListaElementos X CantidadElementos X Contador1 X Contador2 X Contador3 X Carta generada.
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


% Constructor del resto de cartas: Constructor que sirve para el resto de cartas
% Dom: ListaElementos X CantidadElementos X CantidadElementos2 X Contador1 X Contador2 X Contador3 X Carta generada.
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


        % -          TDA cardsSet             - %
/* Representacion:
 Lista de Lista de elementos, Numero de Elementos, Maximo de Cartas, Seed y CS 
 que es donde se guardara el cardsSet armado
*/

% Constructor: predicado que construye el set de cartas 
% Dom: ListaElementos X CantidadElementos X Contador X Carta generada.
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


        % -        Funciones Auxiliares        - %
% Predicado: Construye la lista con las siguientes n cartas.
% Dom: ListaElementos X CantidadElementos X Contador1 X Contador2 X Contador3 X Cartas generadas.
generarNCartas(_,0,_,_,_,[]).
generarNCartas(ListaElement,CantCartas,I,J,K,[Card|Cartas]):-
    not(CantCartas = 0),
    generarNCard(ListaElement,I,I,J,K,Card),
	CantCartasMenos1 is CantCartas -1,
	M is J +1,
    generarNCartas(ListaElement,CantCartasMenos1,I,M,K,Cartas),
    !.


% Predicado: Construye la lista con el resto de cartas por generar.
% Dom: ListaElementos X CantidadElementos X CantidadElementos2 X Contador1 X Contador2 X Contador3 X Cartas generadas.
generarN2Cartas(_,0,_,_,_,_,[]).
generarN2Cartas(ListaElement,CantCartas,O,I,J,K,[Card|Cartas]):-
    not(CantCartas = 0),
    generarN2Card(ListaElement,O,O,I,J,K,Card),
	CantCartasMenos1 is CantCartas -1,
	M is J +1,
    generarN2Cartas(ListaElement,CantCartasMenos1,O,I,M,K,Cartas),
    !.


% Predicado: Construye la lista con todo el resto de cartas por generar.
% Dom: ListaElementos X CantidadCartas X CantidadElementos X CantidadElementos2 X Contador1 X Contador2 X Contador3 X Cartas generadas.
generarN3Cartas(_,0,_,_,_,_,_,[]).
generarN3Cartas(ListaElement,CantCartas,G,O,I,J,K,X):-
	not(CantCartas = 0),
	CantCartasMenos1 is CantCartas -1,
	generarN2Cartas(ListaElement,G,O,I,J,K,Cards),
	append(Cards,Cartas,X),
	M is I +1,
	generarN3Cartas(ListaElement,CantCartasMenos1,G,O,M,J,K,Cartas),
	!.


% Predicado: Genera un mazo con la primera carta y las n cartas.
% Dom: ListaElementos X CantidadElementos X Cartas generadas.
mazoNCartas(ListaElement,CantElement,[FisrtCard|Cards]):-
	CantCartas is CantElement -1,
	generarFirstCard(ListaElement,CantElement,1,FisrtCard),
	generarNCartas(ListaElement,CantCartas,CantElement,1,1,Cards).


% Predicado: Genera un mazo con todas las cartas que se pueden generar.
% Dom: ListaElementos X CantidadElementos X ListaCartasgeneradas.
mazoCartas(ListaElement,CantElement,Cartas):-
	CantCartas is CantElement -1,
	mazoNCartas(ListaElement,CantElement,CardsN),
	generarN3Cartas(ListaElement,CantCartas,CantCartas,CantElement,1,1,1,OtherCards),
	append(CardsN,OtherCards,Cartas).


% Predicado: Elimina la carta en la posicion N.
% Dom: ListaCartasIn X Posicion X ListaCartasOut.
cardSetDeleteCard([_|X], 0, X).
cardSetDeleteCard([X|T1], I, [X|T2]):-
	I2 is I - 1,
	cardSetDeleteCard(T1, I2, T2),
	!.


% Predicado: Selecciona el elemento N en una lista.
% Dom: ListaCartasIn X Posicion X ListaCartasOut.
elementoN([],_,[]).
elementoN([X|_],1,X).
elementoN([_|Y],Cont,U):-
	not(Cont = 1),
	ContMenos1 is Cont -1,
	elementoN(Y,ContMenos1,U),
	!.


% Predicado: Acorta el cardsSet al largo que se establece.
% Dom: ListaCartasIn X CantidadCartas X ListaCartasOut.
acotarMazo(_,0,[]).
acotarMazo(Mazo,CantCartas,[Card|MazoAcotado]):-
	not(CantCartas = 0),
	CantCartasMenos1 is CantCartas -1,
	cardSetDeleteCard(Mazo,0,NewMazo),
	elementoN(Mazo,1,Card),
	acotarMazo(NewMazo,CantCartasMenos1,MazoAcotado),
	!.


% Predicado: Verifica si una carta no se repite en el mazo.
% Dom: CardsSet,Carta,CantidadCartas,Contador,Verificador.
noRepetido(_,_,1,_,A):-
	A =< 1,
	!.
noRepetido(CS,Card1,CantCartas,I,A):-
	not(CantCartas = 1),
	not(var(Card1)),
	A =< 1,
	M is I + 1,
	elementoN(CS,I,Card2),
	intersection(Card1, Card2, InterCardMazo),
	length(InterCardMazo,A),
	CantCartasMenos1 is CantCartas -1,
	noRepetido(CS,Card1, CantCartasMenos1, M, A),
	!.


% Predicado: Verifica que las cartas del mazo no se repite en el mazo.
% Dom: CardsSet,CantidadCartas,Verificador.
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


% Predicado: Construye la representacion de una carta como string.
% Dom: Card,Contador,Str.
stringCarta(Card,I,String4):-
	atomic_list_concat(Card, ' ', StringCard),
	number_string(I, StringI),
	string_concat('Carta ',StringI,String1),
	string_concat(String1,': ',String2),
	string_concat(String2,StringCard,String3),
	string_concat(String3,',\n',String4).


% Predicado: Construye la representacion de un cardsSet como string.
% Dom: CardsSet,Contador,Str, CantidadCartas.
stringCS([],_,'',_):-!.
stringCS(CS,I,StringFinal,CantCartas):-
	not(CantCartas = 0),
	M is I + 1,
	CantCartasMenos1 is CantCartas - 1,
	elementoN(CS,1,Card),
	cardSetDeleteCard(CS,0,NewCS),
	stringCarta(Card,I,StringCard),
	stringCS(NewCS, M, StringResto,CantCartasMenos1),
	string_concat(StringCard,StringResto,StringFinal),
	!.


		% -          TDA dobbleGame             - %
% Representacion:
% Lista de numPlayers(int), CS(cardsSet), Mode(str), seed(int), lista que tiene la como
% primer elemento el turno del jugador y despues el nombre de los usuarios, una lista 
% que tiene en orden lso mazos de los jugadores y por ultimo un mazo donde se encuentran
% las cartas en mesa.

%Constructor: Construye una partida de dobble
% Dom: numPlayers(int), CS(cardsSet), Mode(str), seed(int), DG(TDA).
dobbleGame(NumPlayers,CS,Mode,Seed,[NumPlayers,CS,Mode,Seed,[1,[]],[],[]]).

% Selectores:
getNumPlayers([NumPlayers|_],NumPlayers).
getCardsSet([_,CS,_,_,_,_,_],CS).
getMode([_,_,Mode,_,_,_,_],Mode).
getSeed([_,_,_,Seed,_,_,_],Seed).
getTurnPlayer([_,_,_,_,[TurnoJugador,_],_,_],TurnoJugador).
getListPlayer([_,_,_,_,[_,ListPlayer],_,_],ListPlayer).
getMazoPlayers([_,_,_,_,_,MazoPlayers,_],MazoPlayers).
getCartaEnMesa([_,_,_,_,_,_,CartasEnMesa],CartasEnMesa).


		% -        Funciones Auxiliares        - %
% Predicado: Contruye la representacion en string de los jugadores y sus mazos de cartas.
% Dom: dobbleGame,Contador,Str.
strPlayers(DG,I,Str):-
	getListPlayer(DG,ListPlayer),
	length(ListPlayer,A),
	not(I =< A),
	Str = '',
	!.
strPlayers(DG,I,Str):-
	number_string(I, StrI),
	getMazoPlayers(DG,MazoPlayers),
	elementoN(MazoPlayers,I,MUser),
	cardsSetToString(MUser,StrMUser),
	getListPlayer(DG,ListPlayer),
	elementoN(ListPlayer,I,User),
	length(ListPlayer,A),
	I =< A,
	M is I +1,
	string_concat('El jugador ',StrI,Str1),
	string_concat(Str1,': ',Str2),
	string_concat(Str2, User,Str3),
	string_concat(Str3, ' tiene el mazo: ',Str4),
	string_concat(Str4,StrMUser,Str5),
	string_concat(Str5,'\n',Str6),
	strPlayers(DG,M,Str7),
	string_concat(Str6,Str7,Str),
	!.


% ----------------------------MAIN ----------------------------%
% cardsSetIsDobble
% Predicado: Comprueba si el cardsSet es valido.
% Dom: CardsSet.
cardsSetIsDobble(CS):-
	length(CS,A),
	noRepetido2(CS,A,1).


% Carta enesima
% Predicado: Encuentra la carta que se encuentre en la posicion n-1.
% Dom: CardsSet,n,Card.
cardsSetNthCard([],_,[]):-!.
cardsSetNthCard([X|_],0,X):-!.
cardsSetNthCard([_|R],N,S):-
	M is N - 1,
	cardsSetNthCard(R,M,S),
	!.


% cardsSetFindTotalCards
% Predicado: Entrega el total de cartas que se puede entregar en base a una carta de muestra.
% Dom: Card, Numero Total de Cartas.
cardsSetFindTotalCards(Card,TC):-
	length(Card,A),
	TC is (((A - 1) * (A - 1)) + (A - 1) + 1),
	!.


% cardsSetToString
% Predicado: Crea una representacion de un cardsSet como string.
% Dom: CardsSet,Str.
cardsSetToString(CS,StrCS):-
	length(CS,A),
	stringCS(CS,1,StrCS,A),
	!.


% dobbleGameRegister
% Predicado: Registra un usuario en la partida de Dobble.
% Dom: User, dobbleGameIn, dobbleGameOut.
dobbleGameRegister(User,DGIn,DGOut):-
	getNumPlayers(DGIn,NPlayers),
	getCardsSet(DGIn,CS),
	getMode(DGIn,Mode),
	getTurnPlayer(DGIn,TP),
	getListPlayer(DGIn,ListPlayer),
	getSeed(DGIn,S),
	length(ListPlayer,A),
	(A+1) =< NPlayers,
	getMazoPlayers(DGIn,MazoPlayers),
	getCartaEnMesa(DGIn,CM),
	not(member(User, ListPlayer)),
	append(ListPlayer,[User],LP),
	append(MazoPlayers,[],MP),
	DGOut = [NPlayers,CS,Mode,S,[TP,LP],MP,CM],
	!.


% dobbleGameWhoseTurnIsIt
% Predicado: Funcion para identificar a que jugador le corresponde jugar.
% Dom: dobbleGame,User(str).
dobbleGameWhoseTurnIsIt(DG,User):-
	getTurnPlayer(DG,TP),
	getListPlayer(DG,ListPlayer),
	elementoN(ListPlayer,TP,User),
	!.


% dobbleGameToString
% Predicado: Crea una representacion del dobbleGame como String.
% Dom: dobbleGame,Str.
dobbleGameToString(DG,Str):-
	getNumPlayers(DG,NPlayers),
	number_string(NPlayers, StrNP),
	getCardsSet(DG,CS),
	cardsSetToString(CS,StrCS),
	getMode(DG,Mode),
	getTurnPlayer(DG,TP),
	number_string(TP, StrTP),
	getCartaEnMesa(DG,CM),
	cardsSetToString(CM,StrCM),
	dobbleGameWhoseTurnIsIt(DG,User),
	getSeed(DG,S),
	number_string(S,StrS),
	strPlayers(DG,1,StrPs),
	string_concat('El maximo de jugadores de la partida es ',StrNP,Str1),
	string_concat(Str1,'\n',Str2),
	string_concat(Str2,'El mazo de cartas es:\n',Str3),
	string_concat(Str3,StrCS,Str4),
	string_concat(Str4,'El modo de juego es: ',Str5),
	string_concat(Str5,Mode,Str6),
	string_concat(Str6,'\n',Str7),
	string_concat(Str7,'La Seed es : ',Str8),
	string_concat(Str8,StrS,Str9),
	string_concat(Str9,'\n',Str10),
	string_concat(Str10,'Es el turno del Jugador ',Str11),
	string_concat(Str11,StrTP,Str12),
	string_concat(Str12,' : ',Str13),
	string_concat(Str13,User,Str14),
	string_concat(Str14,'\n',Str15),
	string_concat(Str15,'Las carta en mesa son : ',Str16),
	string_concat(Str16,StrCM,Str17),
	string_concat(Str17,'\n',Str18),
	string_concat(Str18,StrPs,Str),
	!.


% ----------------------------Ejemplos----------------------------%
% cardsSet
% Se crea un mazo del largo maximo con 4 simbolos por carta.
% cardsSet([1,2,3,4,5,6,7,8,9,10,11,12,13],4,A,1,CS).
% Se crea un mazo del largo maximo con 2 simbolos por carta. 
% cardsSet([1,2,3,4,5,6,7,8,9,10,11,12,13],2,A,1,CS).
% Se crea un mazo de 3 cartas con 3 elementos por carta
% cardsSet([1,2,3,4,5,6,7,8,9,10,11,12,13],3,3,112312,CS).


% cardsSetIsDobble
% retorna el CS dado que si es uno valido.
% cardsSet([1,2,3,4,5,6,7,8,9,10,11,12,13],3,3,112312,CS),cardsSetIsDobble(CS).
% Retorna True.
% cardsSetIsDobble([[1, 2], [1, 3], [2, 3]]).
% Entrega False.
% cardsSetIsDobble([[1, 2], [1, 2], [2, 3]]).
% Entrega False.
% CS_MALO = [[a, b, c], [a, b, d], [a, c, e]], cardsSetIsDobble(CS_MALO).


% cardsSetNthCard
% Entrega el primer elemento en C1.
% cardsSet([1,2,3,4,5,6,7,8,9,10,11,12,13],4,A,1,CS),cardsSetNthCard(CS,0,C1).
% Obtiene la tercera carta en C3.
% cardsSet([1,2,3,4,5,6,7,8,9,10,11,12,13],2,A,1,CS),cardsSetNthCard(CS,2,C3).
% Obtiene la cuarta carta en C4.
% cardsSet([1,2,3,4,5,6,7,8,9,10,11,12,13],3,A,1,CS),cardsSetNthCard(CS,3,C4).


% cardsSetFindTotalCards
% Entrega el maximo posible de cartas para cartas con 4 elementos en TC.
% cardsSet([1,2,3,4,5,6,7,8,9,10,11,12,13],4,A,1,CS),cardsSetNthCard(CS,0,C1),cardsSetFindTotalCards(C1,TC).
% Entrega el maximo posible de cartas para cartas con 2 elementos en TC. 
% cardsSet([1,2,3,4,5,6,7,8,9,10,11,12,13],2,A,1,CS),cardsSetNthCard(CS,2,C3),cardsSetFindTotalCards(C3,TC).
% Entrega el maximo posible de cartas para cartas con 3 elementos en TC.
% cardsSet([1,2,3,4,5,6,7,8,9,10,11,12,13],3,A,1,CS),cardsSetNthCard(CS,3,C4),cardsSetFindTotalCards(C4,TC).


% cardsSetToString
% Representa el CS1 como str.
% cardsSet([1,2,3,4,5,6,7,8,9,10,11,12,13],4,A,1,CS1),cardsSetToString(CS1,Str).
% Representa el CS2 como str.
% cardsSet([1,2,3,4,5,6,7,8,9,10,11,12,13],2,A,1,CS2),cardsSetToString(CS2,Str).
% Representa el CS3 como str.
% cardsSet([1,2,3,4,5,6,7,8,9,10,11,12,13],3,A,1,CS3),cardsSetToString(CS3,Str).


% dobbleGame
% crea un Juego para un jugador con un modo generico.
% cardsSet([1,2,3,4,5,6,7,8,9,10,11,12,13],4,A,1,CS),dobbleGame(1,CS,'modoX',12313,DG1).
% crea un Juego para 2 jugadores con un modo generico.
% cardsSet([1,2,3,4,5,6,7,8,9,10,11,12,13],4,A,1,CS),dobbleGame(2,CS,'modoX',12313,DG2).
% crea un Juego para 3 jugadores con un modo generico.
% cardsSet([1,2,3,4,5,6,7,8,9,10,11,12,13],4,A,1,CS),dobbleGame(3,CS,'modoX',12313,DG3).


% dobbleGameRegister
% ERROR: Se intenta registrar mas jugadores de los que permite el juego.
% cardsSet([1,2,3,4,5,6,7,8,9,10,11,12,13],4,A,1,CS),dobbleGame(1,CS,'modoX',12313,DG1),dobbleGameRegister('a',DG1,DG2),dobbleGameRegister('b',DG2,DG3).
% Se registran 2 jugadores.
% cardsSet([1,2,3,4,5,6,7,8,9,10,11,12,13],4,A,1,CS),dobbleGame(2,CS,'modoX',12313,DG1),dobbleGameRegister('a',DG1,DG2),dobbleGameRegister('b',DG2,DG3).
% ERROR: Se intenta registrar 2 veces el mismo usuario.
% cardsSet([1,2,3,4,5,6,7,8,9,10,11,12,13],4,A,1,CS),dobbleGame(3,CS,'modoX',12313,DG1),dobbleGameRegister('a',DG1,DG2),dobbleGameRegister('a',DG2,DG3),dobbleGameRegister('b',DG3,DG4).


% dobbleGameWhoseTurnIsIt
% Le toca a 'User1'.
% cardsSet(['a','b','c','d','e','f','g','h','i','j','k','l','m'],4,A,1,CS),dobbleGame(3,CS,'stackMode',423,DG),dobbleGameRegister('User1',DG,DG2),dobbleGameRegister('User2',DG2,DG3),dobbleGameWhoseTurnIsIt(DG3,TP).
% Le toca a 'a'.
% cardsSet(['a','b','c','d','e','f','g','h','i','j','k','l','m'],4,A,1,CS),dobbleGame(3,CS,'stackMode',123,DG),dobbleGameRegister('a',DG,DG2),dobbleGameWhoseTurnIsIt(DG2,TP).
% Le toca a 'Pedro'.
% cardsSet([1,2,3,4,5,6,7,8,9,10,11,12,13],4,A,1,CS),dobbleGame(3,CS,'modoX',12313,DG1),dobbleGameRegister('Pedro',DG1,DG2),dobbleGameRegister('Juan',DG2,DG3),dobbleGameRegister('Diego',DG3,DG4),dobbleGameWhoseTurnIsIt(DG4,TP).


% dobbleGameToString
% Se crea el String del juego de 2 jugadores.
% cardsSet(['a','b','c','d','e','f','g','h','i','j','k','l','m'],4,A,1,CS),dobbleGame(3,CS,'stackMode',423,DG),dobbleGameRegister('a',DG,DG2),dobbleGameRegister('b',DG2,DG3),dobbleGameToString(DG3,Str).
% Se crea el String para el juego de un solo jugador.
% cardsSet(['a','b','c','d','e','f','g','h','i','j','k','l','m'],4,A,1,CS),dobbleGame(3,CS,'stackMode',123,DG),dobbleGameRegister('a',DG,DG2),dobbleGameToString(DG2,Str).
% Se crea el String del juego de 3 jugadores
% cardsSet([1,2,3,4,5,6,7,8,9,10,11,12,13],4,A,1,CS),dobbleGame(3,CS,'modoX',12313,DG1),dobbleGameRegister('a',DG1,DG2),dobbleGameRegister('b',DG2,DG3),dobbleGameRegister('c',DG3,DG4),dobbleGameToString(DG4,Str).


