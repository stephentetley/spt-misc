/* 
    bin_tree.lgt
*/

:- object(bin_tree(_Label, _Left, _Right)).
    
    :- public(label/1).
    label(X) :-
        parameter(1,X).


:- end_object.  


:- object(bin_tree_traversals,
    imports(traversals),
    implements(walkerp)).

    all_rewrite(R1, bin_tree(Label, Left, Right), Ans) :- 
        ::apply_rewrite(R1, Left, Left1),
        ::apply_rewrite(R1, Right, Right1),
        Ans = bin_tree(Label, Left1, Right1), 
        !.

    all_rewrite(_, empty, empty).

:- end_object.