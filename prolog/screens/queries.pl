% queries.pl

:- use_module(library(csv)).
:- use_module(library(apply)).

:- use_module(facts/fact_db).
:- use_module(classification).


% This needs cut, todo - work out why...
% (format/2 seems to write to terminal)
screen_aux(Type) :- 
    screen_category(Type, Cat),
    !,
    format("~w ~w\n", [Type, Cat]).

demo01 :- 
    findall(X, screen(_, _, _, _, _, X, _, _), List),
    list_to_ord_set(List, Xs),
    maplist(screen_aux, Xs).