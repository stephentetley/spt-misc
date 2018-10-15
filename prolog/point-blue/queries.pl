% queries.pl

:- use_module(library(csv)).
:- use_module(library(apply)).

:- use_module(facts/hawkeyes).
:- use_module(facts/site_work).
:- use_module(facts/csos).

is_phase_two(Sai) :- 
    hawkeye_remaining(Sai, _, _).

is_phase_one(Sai) :- 
    not(hawkeye_remaining(Sai, _, _)).

phase(Sai,Phase) :-
    ( is_phase_two(Sai) -> Phase = 'PHASE TWO'; Phase = 'PHASE ONE').

find_sai(Site,Sai) :- 
    cso(Sai,Site).

all_sites(Sites) :- 
    findall(X, site_work(X,_), Sites).


goal(Sitename, Phase, Worktype) :- 
    cso(Sai, Sitename),
    phase(Sai, Phase), 
    site_work(Sitename, Worktype).

/* missing */

missing_name(Name) :- 
    site_work(Name, _),
    not(cso(_,Name)).

find_missing(Results) :- 
    findall(X, missing_name(X), Results).


/* Generating CVS */

csv_titles(Row) :- 
    Row = row("Sai Number", "Site Name", "Phase", "Commission or Revisit").



temp_rows(Rows) :- 
    Rows = [row('SAI00036099', "ALMONDBURY/CSO", 'COMMISSION')].

goal_row(Sitename, Row) :- 
    goal(Sitename, Phase, Worktype),
    cso(Sai,Sitename),
    Row = row(Sai, Sitename, Phase, Worktype).

as_csv_rows(Conv, Source, Rows) :-
    convlist([X,Row] >> call(Conv, X, Row), Source, Rows).

resultset1(Rows) :- 
    all_sites(Names),
    % convlist([Name,X] >> goal_row(Name, X), Names, Rows).
    as_csv_rows(goal_row, Names, Rows).



output_csv(File, Headers, Rows) :-
    setup_call_cleanup(
        open(File, write, Out),
            ( csv_write_stream(Out, [Headers], []),
              csv_write_stream(Out, Rows, [])
            ),
        close(Out)).

main01 :- 
    Titles = row("Sai Number", "Site Name", "Phase", "Commission or Revisit"),
    resultset1(Rows),   
    output_csv("output.csv", Titles, Rows).

resultset2(Rows) :- 
    find_missing(Names),
    convlist([Name,Row] >> (Row = row(Name)), Names, Rows).

main02 :- 
    Titles = row("Missing"),
    resultset2(Rows),   
    output_csv("missing.csv", Titles, Rows).

