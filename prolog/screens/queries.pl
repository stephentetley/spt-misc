% queries.pl

:- use_module(library(csv)).
:- use_module(library(apply)).

:- use_module(facts/fact_db).
:- use_module(classification).


% This needs cut, todo - work out why...
% (format/2 seems to write to terminal)
screen_aux(Type) :- 
    screen_category(Type, Cat),
    format("~w ~w\n", [Type, Cat]), 
    !.

demo01 :- 
    findall(X, (X = screen(_,_,_,_,_,_,_,_), X), List),
    maplist(screen_type, List, Types),
    list_to_ord_set(Types, Xs),
    maplist(screen_aux, Xs).


demo02 :- 
    findall(Type, screen(_, _, _, _, _, Type, _, _), List),
    list_to_ord_set(List, Xs),
    maplist(screen_aux, Xs).



site(Name) :- installation(_, Name, _, _).
site_uid(Uid) :- installation(Uid, _, _, _).

get_kids(Uid, Kids) :- installation(Uid, _, _, kids(Kids)).


pg_case(process_group(_,_)) :- true.

pg_name(process_group(Name,_), Name).

% Toplevel...
process_group_names(Uid, Gs) :- 
    get_kids(Uid,Kids),
    convlist([X,Y] >> pg_name(X,Y), Kids, Gs).



