% queries.pl

:- use_module(library(csv)).
:- use_module(library(apply)).

:- use_module(common).
:- use_module(facts/adb_outstations).


all_sites(Sites) :- 
    findall(X, adb_outstation(X, _, _), Sites).

local_outstations_(Site,Len) :- 
    findall(X, adb_outstation(Site, X, _), Locs),
    length(Locs, Len).

has_one_outstation(Site) :-
    local_outstations_(Site,X),
    X == 1.

has_multiple_outstations(Site) :-
    local_outstations_(Site,X),
    X > 1.

demo01 :- 
    csv_read_file("worklist.csv", [_| Rows]),
    maplist(writeln, Rows).

row_site_(row(_, Site),Site).
row_uid_(row(Uid,_), Uid).


op2_(Row) :-
    row_site_(Row, X), writeln(X).


demo02 :- 
    csv_read_file("worklist.csv", [_| Rows]),
    maplist(op2_, Rows). 



demo03 :- 
    csv_read_file("worklist.csv", [_| Rows]),
    maplist([Row] >> op2_(Row), Rows).


demo04 :- 
    csv_read_file("worklist.csv", [_| Rows]),
    maplist([Row] >> (row_site_(Row,X), writeln(X)), Rows).

% Fails at first failure
demo05 :- 
    csv_read_file("worklist.csv", [_| Rows]),
    maplist([Row] >> (row_site_(Row,X), adb_outstation(X,_,Y), writeln(Y)), Rows).

operation_(Row) :- 
    row_site_(Row,Site),
    ( has_one_outstation(Site) -> 
        ( adb_outstation(Site,_, Y)
        , writeln(Y))
    ; writeln("None.")
    ).

demo06 :- 
    csv_read_file("worklist.csv", [_| Rows]),
    maplist(operation_, Rows).   


outstation_row(Row0,Row1) :- 
    row_site_(Row0,Site),
    row_uid_(Row0,Uid),
    ( has_one_outstation(Site) -> 
        ( adb_outstation(Site,_, Y)
        , Row1 = row(Uid, Site, Y)
        )
    ; Row1 = row(Uid, Site, "Unknown")
    ).


main :- 
    csv_read_file("worklist.csv", [_| Rows]),
    maplist(outstation_row, Rows, OutputRows),
    Headers = row("SAI","Site Name", "Outstation"),
    output_csv("adb_results.csv", Headers, OutputRows).

