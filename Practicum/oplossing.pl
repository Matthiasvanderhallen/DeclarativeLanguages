% Matthias van der Hallen
% Master computerwetenschappen
:- ensure_loaded(puzzels).
:- ensure_loaded(visualisatie).

% solve/2
solve(PuzzleId,Solution) :-
	puzzle(PuzzleId,Grid,Links),
	arukone(Grid,Links,Solution).

% arukone/3 - zelf te implementeren!
%
% Strategie: We kunnen elke startmarker en elke eindmarker zien
% als 2 aparte 'uiteinden'. Het is vervolgens vaak zo dat er voor
% een uiteinde maar één enkele neighbour gebruikt kan worden om de
% verbinding tussen deze 2 markers te maken. We kunnen deze marker
% toevoegen aan de 'connectie' vanuit de start of eindmarker door een
% bevat((Marker, Herkomst), pos(x,y)) term te maken. Dit betekent dan
% dat de connects term voor die marker zeker deze positie bevat. De
% Herkomst moet bijgehouden worden zodat het mogelijk is om vanuit beide
% markers apart te redeneren. Zo loopt er dus als het ware al een kleine
% connectie vanuit één van de markers naar een neighbour. Deze neighbour
% is dus eigenlijk het NIEUWE uiteinde. We kunnen vanuit dit nieuwe
% uiteinde steeds dezelfde redenering maken, tot we uiteindelijk voor
% alle uiteinden alle verplichte stappen gezet hebben. Wanneer twee
% uiteindes van dezelfde marker (Maar met een verschillende 'herkomst':
% begin/eindmarker) elkaars neighbour zijn, dan kunnen de 2 aparte
% segmenten samen komen, en is de volledige connectie gevonden. Er zijn
% nu GEEN UITEINDEN meer voor deze connectie.
%
% We kunnen dit voor alle connecties tegelijk doen. Soms zijn er voor
% alle bestaande uiteinden echter meerdere mogelijke 'volgende stappen'.
% In dit geval volstaat het om voor één van de uiteinden een
% willekeurige stap te zetten, en vervolgens opnieuw zo veel mogelijk
% verplichte stappen te zetten. Zo hoeft er pas wanneer het terug
% absoluut nodig is, terug een gok gemaakt te worden over welke stap er
% gezet moet worden. Wanneer er geen enkel uiteinden meer is, en alle
% velden gebruikt zijn, dan is de oplossing compleet.
%
% Deze methode vindt eerst alle Markers en roept
% allStartAndEndPosities(Links, [], Gebruikt) op om de
% lijst met bevat((Marker,Herkomst), pos(x,y) termen te maken voor de
% beginsituatie.
%
% Vervolgens roept het losOp op om de effectieve oplossing te berekenen.
% Dit geeft een lijst met bevat((Marker, Herkomst), pos(x,y)) termen
% terug. arukone/3 roept vervolgens de vanGebruiktNaarSolution methode
% op, om deze lijst van bevat termen om te zetten naar de vereiste
% vorm: een lijst van connects(Marker, [pos(x,y),...]) termen.
%
% Aan het einde gebeurt er een cut, aangezien er maar één enkele
% oplossing is, dus de eerst gevonden oplossing is de juiste en enige.
arukone(Grid,Links,Solution) :-
	alleMarkers(Links, Markers),
	allStartAndEndPosities(Links,[], Gebruikt),
	losOp(Grid,Gebruikt,NieuwGebruikt),
	vanGebruiktNaarSolution(Markers, NieuwGebruikt, Solution),
	!.

% Deze methode lost een gegeven puzzel op
% - Grid: een grid(xmax,ymax) term.
% - Setup: alle bevat termen die overeenkomen met de gegeven markers.
% - Solution: alle bevat termen die overeenkomen met de unieke
% oplossing.
%
% Deze methode vindt eerst alle ongebruikte vakjes, en roept vervolgens
% de 1ste keer iteratie(Grid, Uiteinden, Gebruikt, Ongebruikt, Solution)
% op. In deze call is Uiteinden gelijk aan Setup, omdat het alleen de
% opgave is. Deze zijn ook de enige reeds gebruikte vakjes, dus Gebruikt
% is ook gelijk aan Setup. Ongebruikt wordt berekend via
% vindOngebruikt(Grid,Gebruikt,Ongebruikt).
losOp(Grid, Setup, Solution):-
	vindOngebruikt(Grid,Setup,Ongebruikt),
	iteratie(Grid, Setup, Setup, Ongebruikt, Solution).

% iteratie(Grid,Uiteinden, Gebruikt, Ongebruikt, Solution).
%
% - Grid: de grid(xmax, ymax) term.
% - Uiteinden: Alle uiteinden die je kunt bereiken vanaf een
%   start of eindmarker.
% - Gebruikt: De lijst van bevat termen voor alle gebruikte vakjes.
% - Ongebruikt: De lijst van ongebruikte posities.
% - Solution: De uiteindelijke oplossing.
%
% Deze methode doet één iteratie van de oplossingsstrategie. Dit wilt
% zeggen dat alle verplichte stappen gemaakt worden via
% maakVerplichteStappen(Grid, Uiteinden, Gebruikt, Ongebruikt,
% NieuweUiteinden, NieuwGebruikt, NieuwOngebruikt, StapGemaakt).
%
% Dit levert een nieuwe lijst van uiteinden op, alsook een
% nieuwe versie van gebruikt en ongebruikt. StapGemaakt vertelt
% ons of er echt een stap is gemaakt of niet.
% Indien er werkelijk een stap gemaakt is, dan wordt er gewoon
% een nieuwe iteratie opgestart met de nieuwe lijst van uiteinden
% gebruikt en ongebruikt.
%
% Indien er géén nieuwe stap gemaakt is, dan moet er een willekeurige
% stap gezet worden, omdat geen elke stap met zekerheid gemaakt kan
% worden. Dit doen we door
% maakWillekeurigeStap(Grid, NieuweUiteinden, Gebruikt, Ongebruikt
% NieuweUiteinden2, NieuwGebruikt2, NieuwOngebruikt2). aan te roepen
% maakWillekeurigeStap wordt aangeroepen met de nieuwe uiteinden die
% maakVerplichteStappen heeft gemaakt, omdat zelfs als StapGemaakt
% true is, het nog steeds mogelijk is dat maakVerplichteStappen 2
% uiteinden met elkaar verbonden heeft en heeft verwijderd uit de lijst
% met uiteinden.
%
% We hebben een geldige oplossing als de lijst van uiteinden leeg is
% net zoals de lijst van ongebruikte posities. De solution is nu gewoon
% de lijst 'Gebruikt'.
iteratie(_,[],Gebruikt,[],Gebruikt).

% Doet een nieuwe iteratie(Verplichte stappen & eventueel 1 willekeurige
% stap) met de gegeven uiteinden, lijst van gebruikte en ongebruikte
% vakjes, en geeft de oplossing terug.
iteratie(Grid, Uiteinden, Gebruikt, Ongebruikt, Solution):-
	maakVerplichteStappen(Grid, Uiteinden, Gebruikt, Ongebruikt, NieuweUiteinden, NieuwGebruikt, NieuwOngebruikt, StapGemaakt),
	(StapGemaakt == true
	->	iteratie(Grid, NieuweUiteinden, NieuwGebruikt, NieuwOngebruikt, Solution)
	;	maakWillekeurigeStap(Grid, NieuweUiteinden, Gebruikt, Ongebruikt, NieuweUiteinden2, NieuwGebruikt2, NieuwOngebruikt2),
		iteratie(Grid, NieuweUiteinden2, NieuwGebruikt2, NieuwOngebruikt2, Solution)
	).

% maakWillekeurigeStap(Grid, Uiteinden, Gebruikt, Ongebruikt,
% nieuweUiteinden, NieuwGebruikt, NieuwOngebruikt).
%
% Maakt een willekeurige stap. Deze functie neemt de eerste
% bevat-term en zoekt alle neighbours die ook in Ongebruikt zitten.
% Als er zo buren zijn, dan kiest hij er willekeurig één en voegt deze
% als nieuw uiteinde toe aan NieuweUiteinden, en als vakje aan Gebruikt.
% Het verwijdert deze uit 'Ongebruikt'.
% Als er zo geen buren zijn, dan gaat hij verder naar de volgende bevat
% term. Als er geen meer over zijn, dan moet er gebacktrackt worden,
% omdat er eerder al een verkeerde willekeurige stap is gemaakt.
maakWillekeurigeStap(Grid, [bevat(Marker,Head)|Tail], Gebruikt, Ongebruikt, NieuweUiteinden, NieuwGebruikt, NieuwOngebruikt):-
	findall(Pos, (areConnected(Grid, Head, Pos), member(Pos, Ongebruikt)), Connections),
	length(Connections, Length),
	(Length > 0
	->	member(Pos2, Connections),
		delete(Ongebruikt, Pos2, NieuwOngebruikt),
		NieuwGebruikt = [bevat(Marker,Pos2)|Gebruikt],
		NieuweUiteinden = [bevat(Marker,Pos2)|Tail]
	;	maakWillekeurigeStap(Grid, Tail, Gebruikt, Ongebruikt, NieuweUiteinden, NieuwGebruikt, NieuwOngebruikt)
	).

% maakVerplichteStappen(Grid, Uiteinden, Gebruikt, Ongebruikt,
% NieuweUiteinden, NieuwGebruikt, NieuwOngebruikt, StapGemaakt)
%
% Soms is er voor een uiteinde maar één mogelijke stap waar de
% connection naar toe kan gaan. Deze methode zoekt voor de gegeven lijst
% van uiteinde zoveel mogelijk van deze verplichte stappen. StapGemaakt
% laat weten of er effectief een stap gemaakt is, zodat indien nodig
% er een willekeurige stap gemaakt kan worden door de
% oplossingsstrategie zodat er terug nieuwe verplichte stappen kunnen
% komen, en het algoritme geen lusjes blijft lopen waarin geen
% verplichte stappen gevonden kunnen worden.
%
% Deze methode wrapt de versie die een accumulator nodig heeft om
% de lijst van nieuwe uiteinden samen te stellen.
maakVerplichteStappen(Grid, Uiteinden, Gebruikt, Ongebruikt, NieuweUiteinden, NieuwGebruikt, NieuwOngebruikt, StapGemaakt):-
	maakVerplichteStappen(Grid, Uiteinden, Gebruikt, Ongebruikt, [], NieuweUiteinden, NieuwGebruikt, NieuwOngebruikt, StapGemaakt).

% Deze versie van maakVerplichteStappen stelt de base case voor
% waarin er geen ongeprocessde uiteinden meer over zijn. In dat geval
% wordt de accumulator terug gegeven als nieuwe lijst van uiteinden.
% Ook de nieuwe versie van gebruikt en ongebruikt worden door gegeven.
maakVerplichteStappen(_, [], Gebruikt, Ongebruikt, UiteindenAcc, UiteindenAcc, Gebruikt, Ongebruikt, _).

% Deze versie van maakVerplichteStappen kijkt of 2 uiteinden voor
% dezelfde marker maar met verschillende herkomst (Van de start marker
% of van de end marker) naast elkaar liggen. In dit geval komen de
% uiteinden samen en zijn de start en stopmarker feitelijk gelinkt. Dit
% betekent dat beide uiteinden verwijderd mogen worden uit de lijst van
% uiteinden.
%
% Nadat twee termen samen zijn gekomen, mag maakVerplichteStappen op de
% resterende uiteinden aangeroepen worden.
maakVerplichteStappen(Grid,[bevat((Linked, Herkomst), Head)|Tail], Gebruikt, Ongebruikt, UiteindenAcc, NieuweUiteinden, NieuwGebruikt, NieuwOngebruikt, StapGemaakt):-
	areConnected(Grid, Head, Pos),
	append(Tail, UiteindenAcc, Uiteinden),
	member(bevat((Linked,Herkomst2),Pos), Uiteinden), %Je kan alleen samen komen op 2 uiteinden.
	Herkomst2 \= Herkomst,
	delete(Tail,bevat((Linked,Herkomst2),Pos),TailN),
	delete(UiteindenAcc,bevat((Linked,Herkomst2),Pos),UiteindenAccN),
	maakVerplichteStappen(Grid, TailN, Gebruikt, Ongebruikt, UiteindenAccN, NieuweUiteinden, NieuwGebruikt, NieuwOngebruikt, StapGemaakt).

% Dit is de belangrijkste versie van maakVerplichteStappen. Deze gaat
% voor het eerste uiteinde eerst alle neighbours vinden die nog niet
% gebruikt zijn via areConnect(Grid,Head,Pos2) en
% \+member(bevat(_,Pos2, Gebruikt). (member(Pos2, Ongebruikt) zou ook
% werken ipv \+ member(bevat(_,Pos2,Gebruikt))).
%
% Wanneer de lijst van ongebruikte neighbours voor het eerste uiteinde
% exact één element bevat, dan wordt dit het nieuwe uiteinde en
% wordt vooraan Tail toegevoegd zodat het als eerstvolgende opnieuw
% gecheckt wordt voor verplichte stappen of uiteindes samenvoegen.
% en verwijderd uit Ongebruikt. Het wordt nog niet toegevoegd aan
% UiteindenAcc, aangezien de recursieve oproep vanaf dit uiteinde
% mogelijks nóg een stap kan doen. Vervolgens wordt StapGemaakt gelijk
% aan de true term en de recursieve oproep wordt gedaan op de resterende
% lijst van uiteinden.
%
% Wanneer er géén ongebruikte neighbours zijn, dan is er een fail, zodat
% er gebacktrackt kan worden. Dit dient als een optimalisatie, aangezien
% er in dit geval géén oplossing mogelijk is.
%
% Wanneer er meer dan één ongebruikte neighbours zijn, dan blijft het
% huidige uiteinde een uiteinde, en het wordt toegevoegd aan Uiteinden
% Accumulator. maakVerplichteStappen wordt recursief aangeroepen op de
% resterende uiteinden.
maakVerplichteStappen(Grid,[bevat(Marker,Head)|Tail], Gebruikt, Ongebruikt, UiteindenAcc, NieuweUiteinden, NieuwGebruikt, NieuwOngebruikt, StapGemaakt):-
	%Deze uitgecommented code is voor de interpretatie waarin er een unieke oplossing is die alle Markers verbindt, en
	%en deze unieke oplossing gebruikt alle vakjes. Dwz. in de interpretatie dat er geen oplossingen zijn voor de puzzel die alle
	%markers verbinden zonder alle vakjes ook te gebruiken. Wanneer men deze interpretatie aanhoudt dan MOETEN 2 uiteinden van dezelfde marker
	%namelijk altijd verbonden worden indien mogelijk.
	%
	%Marker = (Linked, Herkomst),
	%append(Tail, UiteindenAcc, Uiteinden),
	%findall(Pos, (areConnected(Grid,Head, Pos), member(bevat((Linked, Herkomst2), Pos), Uiteinden), Herkomst2 \= Herkomst), Check),
	%length(Check, 0),
	findall(Pos2, (areConnected(Grid,Head,Pos2), \+ member(bevat(_,Pos2),Gebruikt)), Connections),
	length(Connections,Length),
	(Length =:=1
	->	Connections = [Head2],
		GebruiktN = [bevat((Marker),Head2)|Gebruikt], %Voeg de nieuwe stap toe aan gebruikt met dezelfde marker
		TailN = [bevat((Marker),Head2)|Tail], %Maak het de volgende node die je processt.
		delete(Ongebruikt, Head2, OngebruiktN),
		UiteindenAccN = UiteindenAcc,
		StapGemaakt = true,
		maakVerplichteStappen(Grid, TailN, GebruiktN, OngebruiktN, UiteindenAccN, NieuweUiteinden, NieuwGebruikt, NieuwOngebruikt, _)
	;	(Length =:=0
		->	fail
		;	UiteindenAccN = [bevat((Marker),Head)|UiteindenAcc], %Als we geen stap hebben kunnen maken is dit een uiteinde
			maakVerplichteStappen(Grid, Tail, Gebruikt, Ongebruikt, UiteindenAccN, NieuweUiteinden, NieuwGebruikt, NieuwOngebruikt, StapGemaakt)
		)
	).

% allStartAndEndPosities(Links, Acc, Set)
% Maakt bevatTermen voor alle link(Marker, Start, Einde) termen
% door de lijst 1 voor 1 af te gaan een de nieuwe bevat termen
% in een accumulator bij te houden.
%
% - Links: de lijst van link termen.
% - Acc: De accumulator
% - Set: De set van bevat((Marker, herkomst), pos(x,y)) termen.
allStartAndEndPosities([], Acc, Acc).

allStartAndEndPosities([Head|Tail], Acc, Set):-
	Head = link(Linked, Start, Einde),
	allStartAndEndPosities(Tail, [bevat((Linked,start),Start),bevat((Linked,einde),Einde)|Acc], Set).

% Vindt alle pos(x,y) termen die nog niet in een bevat term van
% de gegeven lijst 'Gebruikt' voorkomen.
% - Grid: De gegeven grid, waaruit de XMax en YMax worden gehaald
% - Gebruikt: De lijst van bevat termen.
% - Ongebruikt: De lijst van posities die nog niet bezet zijn.
vindOngebruikt(grid(XMax, YMax), Gebruikt, Ongebruikt):-
	lijstTot(XMax,XCoords),
	lijstTot(YMax,YCoords),
	findall(Pos,(member(X,XCoords), member(Y,YCoords), Pos = pos(X,Y),\+ member(bevat((_),Pos), Gebruikt)),Ongebruikt).

% Bouwt een lijst op van 1 tot het gegeven getal
% - Getal: Het maximum van de op te bouwen lijst
% - Lijst: Een strikt stijgende lijst, van 1 tot Getal.
lijstTot(Getal, Lijst):-
	lijstTot(Getal, [1], Lijst).

lijstTot(Getal, [Laatste|Tail], Lijst):-
	(Laatste == Getal
	->	Lijst = [Laatste|Tail]
	;	Nieuw is Laatste+1,
		lijstTot(Getal, [Nieuw, Laatste|Tail], Lijst)
	).

% Deze methode vindt al de markers die in het probleem gebruikt worden.
% - Links zijn de links die gevormd moeten worden in het probleem,
% - Markers zijn de markers die gebruikt worden.
alleMarkers(Links, Markers):-
	alleMarkers(Links, [], Markers).

alleMarkers([], MarkerAcc, MarkerAcc).

alleMarkers([link(Marker, _, _)|Tail], MarkerAcc, Markers):-
	alleMarkers(Tail, [Marker|MarkerAcc], Markers).

% Zet de oplossing opgeslagen in Gebruikt, die bestaat uit een lijst van
% bevat((Marker,Herkomst), pos(x,y)) termen, om naar de vereiste vorm
% van Solution, namelijk [connects(Marker, [pos(x,y),...]). Het doet dit
% door het lijstje van Markers af te gaan en voor elke marker de hulp
% methode vanGebruiktNaarSolutionVoorMarker(Marker,Gebruikt,MarkerSol)
% op te roepen.
% - Markers: zijn de Markers gebruikt in het probleem
% - Gebruikt: is de verzameling van bevat((Marker,Herkomst), pos(x,y))
%	     termen
% - Solution: Is de verzameling van connects(Marker, [pos(x,y),..])
vanGebruiktNaarSolution(Markers, Gebruikt, Solution):-
	vanGebruiktNaarSolution(Markers, Gebruikt, [], Solution).

vanGebruiktNaarSolution([], _, SolutionAcc, SolutionAcc).

vanGebruiktNaarSolution([Marker|Tail], Gebruikt, SolutionAcc, Solution):-
	vanGebruiktNaarSolutionVoorMarker(Marker,Gebruikt,MarkerSolution),
	vanGebruiktNaarSolution(Tail, Gebruikt, [MarkerSolution|SolutionAcc], Solution).


% Zet de oplossing opgeslagen in Gebruikt, die bestaat uit een lijst van
% bevat((Marker,Herkomst), pos(x,y)) termen om naar de vereiste vorm
% voor één enkele marker, namelijk connects(Marker, [pos(x,y),...]).
% Het doet dit door de lijst Gebruikt af te lopen en elke keer dat Marker in de
% bevat term overeen komt met de gegeven Marker de positie aan
% MarkerSolution toe te voegen.
% - Marker: De Marker waarvoor de connects term opgebouwd moet worden
% - Gebruikt: De lijst van bevat termen.
% - MarkerSolution: de connects term voor de gegeven marker.
vanGebruiktNaarSolutionVoorMarker(Marker,Gebruikt, MarkerSolution):-
	vanGebruiktNaarSolutionVoorMarker(Marker, Gebruikt, [], Sol),
	MarkerSolution=connects(Marker, Sol).

vanGebruiktNaarSolutionVoorMarker(_, [], SolutionAcc, SolutionAcc).

vanGebruiktNaarSolutionVoorMarker(Marker, [bevat((Mark,_), _)|Tail], SolutionAcc, Solution):-
	Mark \= Marker,
	vanGebruiktNaarSolutionVoorMarker(Marker, Tail, SolutionAcc, Solution).

vanGebruiktNaarSolutionVoorMarker(Marker, [bevat((Marker,_), Head)|Tail], SolutionAcc, Solution):-
	vanGebruiktNaarSolutionVoorMarker(Marker, Tail, [Head|SolutionAcc], Solution).

% areConnected(Grid, Pos1, Pos2).
% Vindt alle pos termen die links, rechts, boven of onder de gegeven
% pos term ligt. Deze geeft alleen pos termen terug die ook effectief
% binnen de grid liggen.
%
% - Grid: de grid(xmax, ymax) term.
% - Pos1: de eerste pos(x,y) term.
% - Pos2: de tweede pos(x,y) term, die links, rechts, boven of onder de
%  pos term die correspondeert met Pos1 moet liggen.
areConnected(Grid, Pos1, Pos2):-
	Pos1 = pos(X1,Y1),
	Pos2 = pos(X2,Y1),
	X2 is X1-1,
	onGrid(Grid,Pos1),
	onGrid(Grid,Pos2).

areConnected(Grid, Pos1, Pos2):-
	Pos1 = pos(X1,Y1),
	Pos2 = pos(X2,Y1),
	X2 is X1+1,
	onGrid(Grid,Pos1),
	onGrid(Grid,Pos2).

areConnected(Grid, Pos1, Pos2):-
	Pos1 = pos(X1,Y1),
	Pos2 = pos(X1,Y2),
	Y2 is Y1-1,
	onGrid(Grid,Pos1),
	onGrid(Grid,Pos2).

areConnected(Grid, Pos1, Pos2):-
	Pos1 = pos(X1,Y1),
	Pos2 = pos(X1,Y2),
	Y2 is Y1+1,
	onGrid(Grid,Pos1),
	onGrid(Grid,Pos2).

% Checkt of een gegeven positie binnen de grid ligt.
onGrid(Grid,Pos) :-
	Pos = pos(X1,Y1),
	Grid = grid(MaxX,MaxY),
	X1>0,
	Y1>0,
	X1=<MaxX,
	Y1=<MaxY.
