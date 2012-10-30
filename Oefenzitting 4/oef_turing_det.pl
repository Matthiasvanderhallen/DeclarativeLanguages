% beweeg/3: Beweegt de leeskop in een bepaalde richting. 
beweeg(links,tape([],Teken,Rs),tape([],#,[Teken|Rs])).
beweeg(links,tape([L|Ls],Teken,Rs),tape(Ls,L,[Teken|Rs])).
beweeg(rechts,tape(Ls,Teken,[]),tape([Teken|Ls],#,[])).
beweeg(rechts,tape(Ls,Teken,[R|Rs]),tape([Teken|Ls],R,Rs)).
beweeg(wacht,Tape,Tape).

% lees/2: Leest het huidige teken onder de leeskop.
lees(tape(_,Teken,_),Teken).

% schrijf/3: Schrijft een teken onder de leeskop.
schrijf(Teken,tape(L,_,R),tape(L,Teken,R)).


% turing/2: Start de turing machine op een gegeven tape.
turing(Tape,NTape) :-
	start(Start),
	turing(Start,Tape,NTape).

turing(Toestand,Tape,Tape):-
	stop(Toestand).

turing(Toestand,Tape,NTape) :- 
	lees(Tape,Teken),
	regel(Toestand,Teken,NToestand,NTeken,Actie),
	schrijf(NTeken,Tape,Tape1),
	beweeg(Actie,Tape1,Tape2),
	turing(NToestand,Tape2,NTape).


% Een turing machine die een gegeven getal n in unaire voorstelling (1 staat voor 0, 11 staat voor 1, 111 staat voor 2, enz.) afbeeldt op n mod 2.

regel(qs,1,q0,#,rechts).
regel(q0,1,q1,#,rechts).
regel(q0,#,q2,0,wacht).
regel(q1,1,q0,#,rechts).
regel(q1,#,q2,1,wacht).

start(qs).

stop(q2).


% Voorbeelden:
%
% 0 mod 2 = 0
%?- turing(tape([],1,[]),Res).
%Res = tape([#], 0, []) .
%
% 1 mod 2 = 1
%?- turing(tape([],1,[1]),Res).
%Res = tape([#, #], 1, []) .
%
% 2 mod 2 = 0
%?- turing(tape([],1,[1,1]),Res).
%Res = tape([#, #, #], 0, []) .
