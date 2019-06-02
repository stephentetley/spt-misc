/*
    osgb36.pl
    Copyright (c) Stephen Tetley 2019
    License: BSD 3 Clause
*/   

:- module(osgb36, 
            [ gridref_geometry/3
            , geometry_gridref/3
            ]).

:- use_module(library(pcre)).


gridref_geometry(GridRef, Easting, Northing) :- 
    writeln("gridref_geometry"),
    atom_string(GridRef, String),
    writeln("gridref_geometry"),
    re_matchsub("(?<letters>[A-Z]{2})(?<easting>\\d{5})(?<northing>\\d{5})"/i, String, Dict, []),
    writeln("gridref_geometry"),
    decode_alpha(Dict.letters, Easting1, Northing1),
    atom_number(Dict.easting, Easting2),
    atom_number(Dict.northing, Northing2),
    Easting is Easting1 + Easting2,
    Northing is Northing1 + Northing2.





% use name/2 to get char codes
% name('AB', Ans).

prefix_codes(Str, Major, Minor) :- 
    upcase_atom(Str, Upper),
    atom_chars(Upper, [Major, Minor]).
    

shifti(X,Y) :- 
    (   X > 8
    ->  Y is X - 1
    ;   Y is X
    ).


decode_major('S', 0.0, 0.0).
decode_major('T', 500000.0, 0.0).
decode_major('N', 0.0, 500000.0).
decode_major('O', 500000.0, 500000.0).
decode_major('H', 0.0, 1000000.0).
decode_major('J', 500000.0, 1000000.0).

decode_minor(CharAtom, Easting, Northing) :- 
    char_code(CharAtom, C1), 
    C2 is C1 - 65,
    shifti(C2, C3),
    divmod(C3, 5, Northing1, Easting1),
    Northing2 is 4 - Northing1,
    Easting is 100000.0 * Easting1,
    Northing is 100000.0 * Northing2.

decode_alpha(Str, Easting, Northing) :- 
    prefix_codes(Str, Major, Minor),
    decode_major(Major, EastingMajor, NorthingMajor),
    decode_minor(Minor, EastingMinor, NorthingMinor),
    Easting is EastingMajor + EastingMinor,
    Northing is NorthingMajor + NorthingMinor.




find_major(Easting, Northing, CharAtom) :- 
    (   Easting >= 0.0, Easting < 500000.0, Northing >= 0.0, Northing < 500000.0
    ->  CharAtom = 'S'
    ;   Easting >= 500000.0, Easting < 1000000.0, Northing >= 0.0, Northing < 500000.0 
    ->  CharAtom = 'T'
    ;   Easting >= 0.0, Easting < 500000.0, Northing >= 500000.0, Northing < 1000000.0 
    ->  CharAtom = 'N'
    ;   Easting >= 500000.0, Easting < 1000000.0, Northing >= 500000.0, Northing < 1000000.0 
    ->  CharAtom = 'O'
    ;   Easting >= 0.0, Easting < 500000.0, Northing >= 1000000.0, Northing < 1500000.0 
    ->  CharAtom = 'H'
    ;   Easting >= 500000.0, Easting < 1000000.0, Northing >= 1000000.0, Northing < 1500000.0 
    ->  CharAtom = 'J'
    ).

minor_grid(0, 0, 'V').
minor_grid(0, 1, 'Q').
minor_grid(0, 2, 'L').
minor_grid(0, 3, 'F').
minor_grid(0, 4, 'A').
minor_grid(1, 0, 'W').
minor_grid(1, 1, 'R').
minor_grid(1, 2, 'M').
minor_grid(1, 3, 'G').
minor_grid(1, 4, 'B').
minor_grid(2, 0, 'X').
minor_grid(2, 1, 'S').
minor_grid(2, 2, 'N').
minor_grid(2, 3, 'H').
minor_grid(2, 4, 'C').
minor_grid(3, 0, 'Y').
minor_grid(3, 1, 'T').
minor_grid(3, 2, 'O').
minor_grid(3, 3, 'J').
minor_grid(3, 4, 'D').
minor_grid(4, 0, 'Z').
minor_grid(4, 1, 'U').
minor_grid(4, 2, 'P').
minor_grid(4, 3, 'K').
minor_grid(4, 4, 'E').

find_minor(Easting, Northing, CharAtom) :- 
    ModE is floor(Easting) mod 500000,
    ModN is floor(Northing) mod 500000,
    DivE is floor(ModE / 100000.0),
    DivN is floor(ModN / 100000.0),
    minor_grid(DivE, DivN, CharAtom).

pad5(Num, String) :- 
    (   Num < 10
    ->  format(string(String), "0000~d", [Num])
    ;   Num < 100
    ->  format(string(String), "000~d", [Num])
    ;   Num < 1000
    ->  format(string(String), "00~d", [Num])
    ;   Num < 10000
    ->  format(string(String), "0~d", [Num])
    ;   format(string(String), "~d", [Num])    
    ).


geometry_gridref(Easting, Northing, GridRef) :- 
    find_major(Easting, Northing, Major),
    find_minor(Easting, Northing, Minor),
    pad5(floor(Easting) mod 100000, SmallE),
    pad5(floor(Northing) mod 100000, SmallN),
    format(string(String), "~a~a~w~w", [Major, Minor, SmallE, SmallN]),
    atom_string(GridRef, String).