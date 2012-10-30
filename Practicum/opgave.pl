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
	predsort(sorteerLinks, Links, SortedLinks),
	allStartAndEndPos(Links, Gebruikt),
	vindLink(Grid, SortedLinks, Gebruikt, Solution).

%testStartEndPos(Set):-
%	Voorbeeld = [link(1, pos(2,1), pos(5,5)),
%		     link(2, pos(1,5), pos(3,2)),
%		     link(3, pos(1,3), pos(3,3)),
%		     link(4, pos(1,1), pos(2,3))],
%	allStartAndEndPos(Voorbeeld, Set).


allStartAndEndPos([Link], [Start, Einde]):-
	Link = link(_, Start, Einde).

allStartAndEndPos([Head|Tail], [Start,Einde|Set]):-
	Head = link(_, Start, Einde),
	allStartAndEndPos(Tail, Set).

sorteerLinks(<, Link1, Link2):-
	Link1 = link(_, pos(XB1,YB1), pos(XE1,YE1)),
	Link2 = link(_, pos(XB2,YB2), pos(XE2,YE2)),
	abs(XB1-XE1)+abs(YB1-YE1) < abs(XB2-XE2)+abs(YB2-YE2).

sorteerLinks(>, Link1, Link2):-
	Link1 = link(_, pos(XB1,YB1), pos(XE1,YE1)),
	Link2 = link(_, pos(XB2,YB2), pos(XE2,YE2)),
	abs(XB1-XE1)+abs(YB1-YE1) >= abs(XB2-XE2)+abs(YB2-YE2).

vindLink(Grid, [Link1|Tail], Gebruikt, Solution) :-
	Link1 = link(_, pos(X1,Y1), pos(X2,Y2)),
	findall(Pad, isPadTussen(Grid, pos(X1,Y1), pos(X2,Y2), Gebruikt, Pad), Solution).

%Deze methode
isPadTussen(Grid, Begin, Einde, Gebruikt, Pad):-
	isPadTussen(Grid,Begin, Einde, Gebruikt, [Begin], Pad).

%isPadTussen(Grid, Begin, Einde,Gebruikt, Acc,[Einde|Acc]) :-
	%areConnected(Grid,Begin, Einde).

isPadTussen(Grid, Begin,Einde,Gebruikt, Acc,Pad):-
	(areConnected(Grid, Begin, Einde)
	->  Pad = [Einde|Acc]
	;   (areConnected(Grid,Begin,Stap),
	    \+ member(Stap, Acc),
	    \+ member(Stap, Gebruikt),
	    isPadTussen(Grid, Stap, Einde,Gebruikt, [Stap|Acc], Pad))
	).

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



