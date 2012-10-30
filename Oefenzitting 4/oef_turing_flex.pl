%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Flexibele deterministische Turing-machine (met optimalisaties)
%   - Tape, Programma en begin- en eindtoestand worden via argumenten
%     doorgegeven. 
%   - Lege plaatsen aan de linker- en rechterkant van de tekens op de tape
%     worden automatisch verwijderd.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% beweeg/3: beweegt de leeskop in een bepaalde richting.
beweeg(wacht,Tape,Tape).
beweeg(links,tape(Links,Teken,Rechts),tape(NLinks,NTeken,NRechts)) :-
    beweeg2(Teken,Links,Rechts,NTeken,NLinks,NRechts).
beweeg(rechts,tape(Links,Teken,Rechts),tape(NLinks,NTeken,NRechts)) :-
    beweeg2(Teken,Rechts,Links,NTeken,NRechts,NLinks).

% beweeg/2: voegt Teken toe aan het begin van B en haalt het eerste teken 
%           uit A. Lege tekens worden weggelaten.
beweeg2(Teken,A,B,NTeken,NA,NB) :-
        ( A = [X|Xs] ->
            NTeken = X,
            NA = Xs
        ;
            NTeken = #,
            NA = A
        ),
        ( B = [_|_] ->
            NB = [Teken|B]
        ;
          Teken = # -> 
            NB = [] 
        ; 
            NB = [Teken]
        ).

% lees/2: Leest het huidige teken onder de leeskop
lees(tape(_,Teken,_),Teken).

% schrijf/3: Schrijft een teken onder de leeskop
schrijf(Teken,tape(L,_,R),tape(L,Teken,R)).


% turing/3: Start een turing machine met een gegeven programma en gegeven
%   begin- en eindtoestand op een gegeven band, en geeft de resultaatband
%   terug.
turing(Tape,Programma,Start,Stop,NTape) :-
    turing2(Start,Tape,Programma,Stop,NTape).

turing2(Toestand,Tape,Programma,Stop,NTape) :- 
    ( member(Toestand,Stop) ->
        NTape = Tape
    ;
        lees(Tape,Teken),
        member(regel(Toestand,Teken,NToestand,NTeken,Actie),Programma), !,
        schrijf(NTeken,Tape,Tape1),
        beweeg(Actie,Tape1,Tape2),
        turing2(NToestand,Tape2,Programma,Stop,NTape)
    ).

% member/2:
member(X,[X|_]).
member(X,[_|Ys]) :-
    member(X,Ys).

% Test de 'n mod 2' turing machine.
test_mod :-
    turing(tape([],1,[1,1,1,1,1]),      % Tape
           [regel(qs,1,q0,#,rechts),
            regel(q0,1,q1,#,rechts),
            regel(q0,#,q2,0,wacht),
            regel(q1,1,q0,#,rechts),
            regel(q1,#,q2,1,wacht)],  % Programma
           qs,                        % Begintoestand
           [q2],                      % Eindtoestanden
           NTape),                    % Nieuwe tape
    write(NTape).
