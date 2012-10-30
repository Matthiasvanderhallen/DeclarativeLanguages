% voorstelling: List-Reverse (Fifo queue = List ++ reverse(Reverse))
% nieuwe elementen worden vooraan aan List toegevoegd
% elementen worden vooraan Reverse verwijderd
% als Reverse leeg is, wordt List omgekeerd tot een nieuwe Reverse

push(L-R,E,[E|L]-R).
pop(L-[E|R],E,L-R).
pop(L-[],E,Tuple) :-
	L \= [],
	reverse(L,R),
	pop([]-R,E,Tuple).

% de kost van elke afzonderlijke reverse operatie is proportioneel tot het
% aantal nog niet omgekeerde elementen en dus de totale kost van ALLE reverse
% operaties is proportioneel tot het aantal push operaties.
% ==> de amortized kost van 1 pop operatie is constant