/* 
    objects.lgt
*/

%% rose_tree

:- object(rose_tree(_Label, _Kids)).
    
    :- public([
        label/1,
        kids/1
    ]).

    label(X) :-
        parameter(1,X).
    
    kids(Kids) :-
        parameter(2,Kids).


:- end_object.

:- object(test).

    :- public([
        demo/0
    ]).

    demo :- 
        X = rose_tree(1,[]),
        write(X).

:- end_object.        
