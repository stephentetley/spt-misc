% herbrand.pl

level_control(
    name("WET WELL LEVEL LOOP"),
    serial_number("XSD014/8")
    ).

level_control(
    name("CSO LEVEL LOOP"),
    serial_number("XSD014/8") 
    ).

serial_number_of(Name,Snum) :-
    level_control(name(Name), serial_number(Snum)).

