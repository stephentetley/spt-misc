% queries.pl

:- use_module(facts/fact_db).
:- use_module(common).
:- use_module(classification).

span_below_empty(PLI) :- 
    sensor_measurements(PLI, Empty, Span),
    Span < Empty.


%% Remember varnames start with a Capital...
empty_distance(PLI, X) :-
    sensor_measurements(PLI, X,_).

demo01(Site, Xs) :- 
    findall(X, monitor_location(Site, _, X), Xs).

demo02 :- 
    findall(X, active_relay(_, _, X, _, _), List),
    list_to_ord_set(List, Xs),
    maplist(writeln, Xs).

demo02b :- 
    findall(X, fixed_relay(_, _, X), List),
    list_to_ord_set(List, Xs),
    maplist(writeln, Xs).

% Note - one error of configs is fixed relays (e.g. 'TRANSDUCER CABLE FAULT ALARM') 
% configured to have set points.

/*
demo03 :- 
    findall(X, fixed_relay(_, _, X), Xs),
    list_to_ord_set(Xs, Ls1),
    findall(Y, active_relay(_, _, Y, _, _), Ys),
    list_to_ord_set(Ys, Ls2),
*/
    





