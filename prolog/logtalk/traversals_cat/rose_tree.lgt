/* 
    rose_tree
*/

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

:- object(rose_tree_traversals,
    implements(walkerp),
    imports(traversals)).


    all_rewrite(R1, Input, Ans) :- 
        Input = rose_tree(Label,Kids),
        ::all_rewrite_list(R1, Kids, Kids1),
        Ans = rose_tree(Label, Kids1).

:- end_object.    