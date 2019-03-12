/* 
    traversals.lgt
*/

:- object(traversals,
    extends(traversals_base)).

    :- public([
        all_rewrite/3
    ]).

    :- meta_predicate(all_rewrite(2,*,*)).

        
    all_rewrite(R1, Input, Ans) :- 
        Input = rose_tree(Label, Kids),
        ^^all_rewrite_list(R1, Kids, Kids1),
        Ans = rose_tree(Label, Kids1).

:- end_object.

