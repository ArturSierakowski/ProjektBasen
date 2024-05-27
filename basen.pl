:- dynamic rezerwacja/4.

% Ładowanie pliku z rezerwacjami
:- [rezerwacje].

% Ładowanie pliku GUI
:- [basen_gui].

% Spis funkcji głównych i pomocniczych:
% 1. Główne funkcje:
%    - dodaj_rezerwacje/4
%    - dostepnosc/3
%    - anuluj_rezerwacje/4
%    - wyswietl_rezerwacje/0
%    - wyszukaj_rezerwacje_uzytkownika/1
%    - wyszukaj_rezerwacje_daty/1
%    - wyszukaj_rezerwacje_toru/1
%    - edytuj_rezerwacje/5
%    - dodaj_rezerwacje_cykliczne/5
%    - raport_liczba_rezerwacji/0
%    - najczesciej_rezerwowane_godziny/0
% 2. Funkcje pomocnicze:
%    - parse_date/2
%    - format_date/2
%    - add_days/3
%    - add_weeks/3
%    - zapisz_rezerwacje/0
%    - raport_liczba_rezerwacji_tor/2
%    - clumped/2
%    - clump/4

% --------------------------  Funkcje pomocnicze --------------------------

% Pomocnicza funkcja do parsowania daty i weryfikacji poprawności
parse_date(Atom, date(Year, Month, Day)) :-
    atomic_list_concat([Y, M, D], '-', Atom),
    atom_number(Y, Year),
    atom_number(M, Month),
    atom_number(D, Day),
    between(1, 12, Month),
    days_in_month(Year, Month, MaxDay),
    between(1, MaxDay, Day).

% Funkcja do określania liczby dni w miesiącu, uwzględniająca lata przestępne
days_in_month(Year, 2, 29) :-
    Year mod 4 =:= 0,
    (Year mod 100 =\= 0 ; Year mod 400 =:= 0), !.
days_in_month(_, 2, 28).
days_in_month(_, Month, 30) :- member(Month, [4, 6, 9, 11]), !.
days_in_month(_, _, 31).

% Pomocnicza funkcja do formatowania daty
format_date(date(Year, Month, Day), FormattedDate) :-
    format(atom(FormattedDate), '~d-~|~`0t~d~2+-~|~`0t~d~2+', [Year, Month, Day]).

% Pomocnicza funkcja do dodawania dni do daty
add_days(date(Year, Month, Day), DaysToAdd, date(NewYear, NewMonth, NewDay)) :-
    date_time_stamp(date(Year, Month, Day, 0, 0, 0, 0, -, -), Timestamp),
    NewTimestamp is Timestamp + DaysToAdd * 86400,
    stamp_date_time(NewTimestamp, date(NewYear, NewMonth, NewDay, _, _, _, _, _, _), 'UTC').

% Dodaj tygodnie do daty
add_weeks(Date, 0, [Date]).
add_weeks(Date, N, [Date | Rest]) :-
    N > 0,
    add_days(Date, 7, NextDate),
    NextN is N - 1,
    add_weeks(NextDate, NextN, Rest).

% Zapis rezerwacji do pliku
zapisz_rezerwacje :-
    tell('rezerwacje.pl'),
    listing(rezerwacja/4),
    told.

% Liczba rezerwacji dla każdego toru
raport_liczba_rezerwacji_tor(Tor, Liczba) :-
    findall(Tor, rezerwacja(_, Tor, _, _), Lista),
    length(Lista, Liczba).

% Pomocnicza funkcja do liczenia wystąpień
clumped([], []).
clumped([X|Xs], [X-N|Ys]) :-
    clump(X, Xs, Xs1, 1, N),
    clumped(Xs1, Ys).

clump(_, [], [], N, N).
clump(X, [Y|Ys], [Y|Ys], N, N) :-
    X \= Y.
clump(X, [X|Xs], Ys, K, N) :-
    K1 is K + 1,
    clump(X, Xs, Ys, K1, N).

% -----------------------------  Główne funkcje -----------------------------

dodaj_rezerwacje(Uzytkownik, Tor, Data, Godzina) :-
    integer(Tor), between(1, 8, Tor),             % Walidacja toru
    parse_date(Data, _),                          % Walidacja daty
    integer(Godzina), between(8, 20, Godzina),    % Walidacja godziny
    \+ rezerwacja(_, Tor, Data, Godzina),         % Sprawdzanie, czy tor nie jest już zajęty
    assertz(rezerwacja(Uzytkownik, Tor, Data, Godzina)),
    zapisz_rezerwacje.

dostepnosc(Tor, Data, Godzina) :-
    \+ rezerwacja(_, Tor, Data, Godzina).

anuluj_rezerwacje(Uzytkownik, Tor, Data, Godzina) :-
    retract(rezerwacja(Uzytkownik, Tor, Data, Godzina)),
    zapisz_rezerwacje.

wyswietl_rezerwacje :-
    findall((Uzytkownik, Tor, Data, Godzina), rezerwacja(Uzytkownik, Tor, Data, Godzina), Rezerwacje),
    writeln(Rezerwacje).

wyszukaj_rezerwacje_uzytkownika(Uzytkownik) :-
    findall((Tor, Data, Godzina), rezerwacja(Uzytkownik, Tor, Data, Godzina), Rezerwacje),
    writeln(Rezerwacje).

wyszukaj_rezerwacje_daty(Data) :-
    parse_date(Data, _),                          % Walidacja daty
    findall((Uzytkownik, Tor, Godzina), rezerwacja(Uzytkownik, Tor, Data, Godzina), Rezerwacje),
    writeln(Rezerwacje).

wyszukaj_rezerwacje_toru(Tor) :-
    integer(Tor), between(1, 8, Tor),             % Walidacja toru
    findall((Uzytkownik, Data, Godzina), rezerwacja(Uzytkownik, Tor, Data, Godzina), Rezerwacje),
    writeln(Rezerwacje).


edytuj_rezerwacje(Uzytkownik, Index, NowyTor, NowaData, NowaGodzina) :-
    integer(NowyTor), between(1, 8, NowyTor),     % Walidacja toru
    parse_date(NowaData, _),                      % Walidacja daty
    integer(NowaGodzina), between(8, 20, NowaGodzina), % Walidacja godziny
    findall((Tor, Data, Godzina), rezerwacja(Uzytkownik, Tor, Data, Godzina), Rezerwacje),
    nth1(Index, Rezerwacje, (StaryTor, StaraData, StaraGodzina)),
    retract(rezerwacja(Uzytkownik, StaryTor, StaraData, StaraGodzina)),
    (   \+ rezerwacja(_, NowyTor, NowaData, NowaGodzina)
    ->  assertz(rezerwacja(Uzytkownik, NowyTor, NowaData, NowaGodzina)),
        zapisz_rezerwacje
    ;   assertz(rezerwacja(Uzytkownik, StaryTor, StaraData, StaraGodzina)),
        fail
    ).

% Dodaj rezerwacje cykliczne (co tydzień przez n tygodni)
dodaj_rezerwacje_cykliczne(Uzytkownik, Tor, Data, Godzina, Tygodnie) :-
    integer(Tor), between(1, 8, Tor),             % Walidacja toru
    parse_date(Data, ParsedDate),                 % Walidacja daty
    integer(Godzina), between(8, 20, Godzina),    % Walidacja godziny
    add_weeks(ParsedDate, Tygodnie, ListaDat),
    (   \+ (member(NewDate, ListaDat),
            format_date(NewDate, FormattedDate),
            rezerwacja(_, Tor, FormattedDate, Godzina))
    ->  forall(member(NewDate, ListaDat),
               (   format_date(NewDate, FormattedDate),
                   assertz(rezerwacja(Uzytkownik, Tor, FormattedDate, Godzina))
               )),
        zapisz_rezerwacje
    ;   fail
    ).

raport_liczba_rezerwacji :-
    forall(between(1, 8, Tor),
           (raport_liczba_rezerwacji_tor(Tor, Liczba),
            format('Tor ~w: ~w rezerwacji~n', [Tor, Liczba]))).

najczesciej_rezerwowane_godziny :-
    findall(Godzina, rezerwacja(_, _, _, Godzina), ListaGodzin),
    msort(ListaGodzin, ListaPosortowana),
    clumped(ListaPosortowana, GodzinyPosortowane),
    sort(2, @>=, GodzinyPosortowane, Godziny),
    writeln(Godziny).
