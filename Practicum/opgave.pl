% Voornaam Naam
% Studierichting

% Bewaar dit bestand als oplossing.pl en implementeer het predicaat arukone/3.
% Documenteer de oplossingsstrategie van je oplossing op hoog niveau.
% Documenteer de heuristieken en optimalisaties die je toepast.
% Besteed voldoende aandacht aan de leesbaarheid en indentatie van je code.
% Splits lange predicaten in korte predicaten die elk een eigen taak vervullen.
% Documenteer de beoogde functionaliteit van de afzonderlijke predicaten.
% Documenteer eventuele gekende problemen en leg uit hoe je deze op kan lossen.
% Verwijder deze lijst met aandachtspunten alvorens je oplossing in te dienen.

:- ensure_loaded(puzzels).
:- ensure_loaded(visualisatie).

% solve/2
solve(PuzzleId,Solution) :-
    puzzle(PuzzleId,Grid,Links),
    arukone(Grid,Links,Solution).

% arukone/3 - zelf te implementeren!
arukone(Grid,Links,Solution) :-
	%predsort(sorteerLinks, Links, SortedLinks),
	allStartAndEndPos(Links,[], Gebruikt),
	SortedLinks = Links,
	vindLink(Grid, SortedLinks, Gebruikt, Solution).

%testStartEndPos(Set):-
%	Voorbeeld = [link(1, pos(2,1), pos(5,5)),
%		     link(2, pos(1,5), pos(3,2)),
%		     link(3, pos(1,3), pos(3,3)),
%		     link(4, pos(1,1), pos(2,3))],
%	allStartAndEndPos(Voorbeeld, Set).


allStartAndEndPos([], Acc, Acc).

allStartAndEndPos([Head|Tail], Acc, Set):-
	Head = link(_, Start, Einde),
	allStartAndEndPos(Tail, [Start,Einde|Acc], Set).

allStartAndEndPos2([], Acc, Acc).

allStartAndEndPos2([Head|Tail], Acc, Set):-
	Head = link(Linked, Start, Einde),
	allStartAndEndPos2(Tail, [bevat((Linked,start),Start),bevat((Linked,einde),Einde)|Acc], Set).

sorteerLinks(<, Link1, Link2):-
	Link1 = link(_, pos(XB1,YB1), pos(XE1,YE1)),
	Link2 = link(_, pos(XB2,YB2), pos(XE2,YE2)),
	abs(XB1-XE1)+abs(YB1-YE1) < abs(XB2-XE2)+abs(YB2-YE2).

sorteerLinks(>, Link1, Link2):-
	Link1 = link(_, pos(XB1,YB1), pos(XE1,YE1)),
	Link2 = link(_, pos(XB2,YB2), pos(XE2,YE2)),
	abs(XB1-XE1)+abs(YB1-YE1) >= abs(XB2-XE2)+abs(YB2-YE2).

sorteerVolgensLength(<, Lijst1, Lijst2):-
	length(Lijst1, Length1),
	length(Lijst2, Length2),
	Length1<Length2.

sorteerVolgensLength(>, Lijst1, Lijst2):-
	length(Lijst1, Length1),
	length(Lijst2, Length2),
	Length1>=Length2.

vindLink(Grid, Lijst, Gebruikt, Solution):-
	vindLink(Grid, Lijst, Gebruikt, [], Solution),
	!.

vindLink(Grid, [], _, Acc, Acc).

vindLink(Grid, [Link1|Tail], Gebruikt, Acc, Solution) :-
	Link1 = link(Linked, Begin, Einde),
	forbiddenNodes(Grid, Tail, Gebruikt, ForbiddenNodes),
	append(ForbiddenNodes, Gebruikt, NietTeGebruiken),
	findPathTussen(Grid, Begin, Einde, NietTeGebruiken,nul,[Begin], GekozenPad),
	%isPadTussen(Grid,Begin,Einde,NietTeGebruiken,GekozenPad),
	%findall(Pad, isPadTussen(Grid, Begin, Einde, NietTeGebruiken, Pad), Paden),
	%predsort(sorteerVolgensLength, Paden, SortedPaden),
	%isPadTussen(Grid, Begin, Einde, NietTeGebruiken, gekozenPad),
	%member(GekozenPad, SortedPaden),
	%\+ containsForbiddenNode(GekozenPad, ForbiddenNodes),
	append(GekozenPad, Gebruikt, NewGebruikt),
	%othersHavePath(Grid,Tail, NewGebruikt),
	NewAcc = [connects(Linked,GekozenPad)|Acc],
	vindLink(Grid,Tail,NewGebruikt,NewAcc, Solution).

othersHavePath(_,[],_).

othersHavePath(Grid,[Head|Tail], NewGebruikt):-
	Head = link(_, Begin, Einde),
	isPadTussen(Grid, Begin, Einde, NewGebruikt, _),
	othersHavePath(Grid, Tail, NewGebruikt).

containsForbiddenNode(GekozenPad, ForbiddenNodes):-
	member(X, GekozenPad),
	member(X, ForbiddenNodes).

forbiddenNodes(Grid, Links, Gebruikt, ForbiddenNodes):-
	allStartAndEndPos2(Links,[],Positions),
	forbiddenNodes(Grid, Positions, Gebruikt, [], ForbiddenNodes).

forbiddenNodes(_, [], _, Acc, Acc):-
	!.

forbiddenNodes(Grid, [bevat((Linked, Herkomst),Head)|Tail], Gebruikt, Acc, ForbiddenNodes):-
	append(Gebruikt, Acc, All),
	member(bevat((Linked,Herkomst2),Pos), All),
	Herkomst2 \= Herkomst,
	areConnected(Grid,Head,Pos),
	forbiddenNodes(Grid, Tail, Gebruikt, Acc, ForbiddenNodes).

forbiddenNodes(Grid, [bevat(Linked,Head)|Tail], Gebruikt, Acc, ForbiddenNodes):-	findall(Pos2, (areConnected(Grid,Head,Pos2), \+ member(bevat(_,Pos2), Gebruikt), \+ member(bevat(_,Pos2), Acc)), Connections),
	length(Connections, Length),
	(Length =:=1
	->  Connections = [Head2],
	    NewAcc = [bevat(Linked,Head2)|Acc],
	    NewTail = [bevat(Linked,Head2)|Tail]
	;   NewAcc = Acc,
	    NewTail = Tail),
	forbiddenNodes(Grid, NewTail, Gebruikt, NewAcc, ForbiddenNodes).


testforbiddenNodes(ForbiddenNodes):-
	Test = [
    link(1,pos(1,7),pos(7,6)),
    link(2,pos(2,6),pos(3,2)),
    link(3,pos(2,7),pos(6,5)),
    link(4,pos(4,4),pos(5,3)),
    link(5,pos(4,5),pos(7,7)),
    link(6,pos(5,5),pos(6,6))
],
	allStartAndEndPos2(Test,[], Gebruikt),
	forbiddenNodes(grid(7,7), Test, Gebruikt, ForbiddenNodes).

%forbiddenNodes(Grid, [Link|Tail],Gebruikt, Acc, ForbiddenNodes):-
%	Link = link(_, Begin, Einde),
	%findall(Pos2, (areConnected(Grid, Begin, Pos2), \+ member(Pos2, Gebruikt)), ConnectionsBeginning),
	%length(ConnectionsBeginning, Length1),
	%(Length1 =:= 1
	%->  append(ConnectionsBeginning, Acc, Forbidden1)
	%;   Forbidden1 = Acc),
	%findall(Pos2, (areConnected(Grid, Einde, Pos2), \+ member(Pos2, Gebruikt)), ConnectionsEinde),
	%length(ConnectionsEinde, Length2),
	%(Length2 =:= 1
	%->  append(ConnectionsEinde, Forbidden1, Forbidden2)
	%;   Forbidden2 = Forbidden1),
	%forbiddenNodes(Grid, Tail, Gebruikt, Forbidden2, ForbiddenNodes).

%forbiddenNodes(_, [], _, Acc, Acc).

findPathTussen(Grid, Last, Einde, _, _, Acc, [Einde|Acc]):-
	areConnected(Grid, Last, Einde).

findPathTussen(Grid, Last, Einde, Gebruikt, Dir, Acc, Pad):-
	areConnected(Grid, Last, Pos),
	\+ member(Pos, Gebruikt),
	\+ member(Pos, Acc),
	findPathTussen(Grid, Pos, Einde, Gebruikt, nul, [Pos|Acc], Pad).


%Deze methode
isPadTussen(Grid, Begin, Einde, Gebruikt, Pad):-
	isPadTussen(Grid,Begin, Einde, Gebruikt, [Begin], Pad).

isPadTussen(Grid, Begin, Einde,_, Acc,[Einde|Acc]) :-
	areConnected(Grid,Begin, Einde).

isPadTussen(Grid, Begin,Einde,Gebruikt, Acc,Pad):-
%	(areConnected(Grid, Begin, Einde)
%	->  Pad = [Einde|Acc]
	areConnected(Grid,Begin,Stap),
	\+ member(Stap, Acc),
	\+ member(Stap, Gebruikt),
	isPadTussen(Grid, Stap, Einde, Gebruikt, [Stap|Acc], Pad).

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

onGrid(Grid,Pos) :-
	Pos = pos(X1,Y1),
	Grid = grid(MaxX,MaxY),
	X1>0,
	Y1>0,
	X1=<MaxX,
	Y1=<MaxY.



