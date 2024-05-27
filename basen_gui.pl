:- use_module(library(pce)).
:- use_module(basen).

% GUI
:- pce_begin_class(basen_gui, dialog, "Basen Reservation GUI").

initialise(D) :->
    send_super(D, initialise, 'Basen Reservation System'),
    send(D, append, new(User, text_item(user))),
    send(D, append, new(Lane, int_item(lane))),
    send(D, append, new(Date, text_item(date))),
    send(D, append, new(Time, int_item(time))),
    send(D, append, new(Weeks, int_item(weeks))),
    send(D, append, button(add_single_reservation, message(D, add_single_reservation,
                                                           User?selection,
                                                           Lane?selection,
                                                           Date?selection,
                                                           Time?selection))),
    send(D, append, button(add_cyclic_reservation, message(D, add_cyclic_reservation,
                                                           User?selection,
                                                           Lane?selection,
                                                           Date?selection,
                                                           Time?selection,
                                                           Weeks?selection))),
    send(D, append, button(show_reservations, message(D, show_reservations))),
    send(D, open).

add_single_reservation(D, User:name, Lane:int, Date:name, Time:int) :->
    dodaj_rezerwacje(User, Lane, Date, Time),
    send(D, report, status, 'Single reservation added').

add_cyclic_reservation(D, User:name, Lane:int, Date:name, Time:int, Weeks:int) :->
    dodaj_rezerwacje_cykliczne(User, Lane, Date, Time, Weeks),
    send(D, report, status, 'Cyclic reservation added').

show_reservations(D) :->
    findall((User, Lane, Date, Time), rezerwacja(User, Lane, Date, Time), Reservations),
    writeln(Reservations),
    send(D, report, status, 'Reservations displayed in console').

:- pce_end_class(basen_gui).

start :-
    new(_D, basen_gui).
