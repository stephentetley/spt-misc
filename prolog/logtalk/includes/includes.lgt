/*
    includes.lgt
*/   

:- use_module(library(csv)).
:- use_module(library(lists)).

:- object(film_facts).

    :- include('database.pl').

    :- public([star/2]).
    
:- end_object.

:- object(common_lib).

    :- include('common.pl').

    :- public([
        output_csv/3,
        nth0_cell/3,
        nth1_cell/3
    ]).

:- end_object.    

