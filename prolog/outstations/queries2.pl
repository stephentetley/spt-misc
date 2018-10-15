% queries.pl

:- use_module(library(csv)).
:- use_module(library(apply)).

:- use_module(common).
:- use_module(facts/adb_outstations).
:- use_module(facts/adb_sites).
:- use_module(facts/rts_outstations).

os_count(Uid, Count) :-
    findall(X, rts_outstation(Uid,X,_), Xs),
    length(Xs, Count).

outstations(Uid, Xs) :-
    findall(X, rts_outstation(Uid,X,_), Xs).

row_site_(row(_, Site),Site).
row_uid_(row(Uid,_), Uid).

get_worklist(Rows) :- 
    csv_read_file("worklist.csv", [_| Rows]).  

demo01 :- 
    get_worklist(Rows),
    maplist([Row] >> (row_uid_(Row, Uid), os_count(Uid,X), writeln(X)), Rows).

translate(RowX, RowY) :-
    row_uid_(RowX, Uid),
    row_site_(RowX, Name),
    outstations(Uid, Xs),
    os_count(Uid, OsCount),
    atomics_to_string(Xs, " & ", OsNames),
    RowY = row(Uid, Name, OsCount, OsNames).

main :- 
    csv_read_file("worklist.csv", [_| Rows]),
    maplist(translate, Rows, OutputRows),
    Headers = row("SAI Number","Site Common Name", "Outstation Count", "Outstation Names"),
    output_csv("rts_results.csv", Headers, OutputRows).


is_sai_number(Uid) :-
    term_string(Uid, Text),
    sub_string(Text, _, 3, _, Prefix),
    Prefix == "SAI".

rts_bad_name_aux(Uid, Row) :-
    rts_outstation(Uid, OsName, RtsName),
    adb_site(Uid, AdbName, _),
    not(adb_outstation(RtsName,_, _)),
    is_sai_number(Uid),
    not(RtsName == AdbName),
    Row = row(OsName, Uid, RtsName, AdbName).

rts_bad_names :-
    findall(Uid, rts_outstation(Uid, _, _), Uids),
    convlist(rts_bad_name_aux, Uids, OutputRows),
    Headers = row("OS Name", "SAI Number", "RTS OD Name", "Proper Name"),
    output_csv("rts_bad_names.csv", Headers, OutputRows).

adb_bad_name_aux(Uid, Row) :- 
    rts_outstation(Uid, RtsOsName, _),
    adb_site(Uid, AdbSite, AdbOsName),
    not(RtsOsName == AdbOsName),
    Row = row(Uid, AdbSite, AdbOsName, RtsOsName).


adb_bad_names :- 
    findall(Uid, adb_site(Uid, _, _), Uids),
    convlist(adb_bad_name_aux, Uids, OutputRows),
    Headers = row("SAI Number", "Site Name", "ADB OS Name", "RTS OS Name"),
    output_csv("adb_bad_names.csv", Headers, OutputRows).







    