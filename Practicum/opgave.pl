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

pos(1,1).
pos(1,2).
pos(1,3).
pos(1,4).
pos(2,1).
pos(2,2).
pos(2,3).
pos(2,4).
pos(3,1).
pos(3,2).
pos(3,3).
pos(3,4).
pos(4,1).
pos(4,2).
pos(4,3).
pos(4,4).

% solve/2
solve(PuzzleId,Solution) :-
    puzzle(PuzzleId,Grid,Links),
    arukone(Grid,Links,Solution).

% arukone/3 - zelf te implementeren!
arukone(Grid,Links,Solution) :-
	predsort(sorteerLinks, Links, SortedLinks),
	vindLink(Grid, SortedRemainingLinks, Solution).

sorteerLinks(<, Link1, Link2):-
	Link1 = link(_, pos(XB1,YB1), pos(XE1,YE1)),
	Link2 = link(_, pos(XB2,YB2), pos(XE2,YE2)),
	abs(XB1-XE1)+abs(YB1-YE1) < abs(XB2-XE2)+abs(YB2-YE2).

sorteerLinks(>, Link1, Link2):-
	Link1 = link(_, pos(XB1,YB1), pos(XE1,YE1)),
	Link2 = link(_, pos(XB2,YB2), pos(XE2,YE2)),
	abs(XB1-XE1)+abs(YB1-YE1) >= abs(XB2-XE2)+abs(YB2-YE2).

vindLink(Grid, [Link1|Tail], Solution) :-
	Link1 = link(_, Begin, Einde).
%	vindPadenTussen(Begin, Einde)

vindPadenTussen(Begin, Einde).

isPadTussen(Grid, Begin, Einde,_,_) :-
	areConnected(Grid,Begin, Einde).

isPadTussen(Grid, Begin,Einde,Acc,[Stap|Tail]):-
        areConnected(Grid,Begin,Stap),
	\+ member(Stap, Acc),
	isPadTussen(Stap, Einde, [Stap|Acc], Tail).

areConnected(Grid, Pos1, Pos2):-
	Pos1 = pos(X1,Y1),
	Pos2 = pos(X2,Y2),
	onGrid(Grid,Pos1),
	onGrid(Grid,Pos2),
	(X1 == X2
	-> abs(Y1-Y2)=:=1
	;   (Y1==Y2
	    ->	abs(X1-X2)=:=1
	    ;	fail)).

onGrid(Grid,Pos) :-
	Pos = pos(X1,Y1),
	Grid = grid(MaxX,MaxY),
	X1>0,
	Y1>0,
	X1=<MaxX,
	Y1=<MaxY.



