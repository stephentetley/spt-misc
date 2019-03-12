/* 
    traversals_base.lgt
*/

:- object(traversals_base).

    :- public([
        id_rewrite/2,
        fail_rewrite/2,
        fail_transform/3,
        apply_rewrite/3,
        apply_transform/4,
        all_rewrite_list/3,

        all_rewrite/3
    ]).

    :- meta_predicate(apply_rewrite(2,*,*)).
    :- meta_predicate(apply_transform(3,*,*,*)).
    :- meta_predicate(all_rewrite_list(2,*,*)).
    :- meta_predicate(all_rewrite(2,*,*)).

    id_rewrite(Ans, Ans).
    
    fail_rewrite(_ , _) :- false.

    fail_transform(_, _ , _) :- false.

    apply_rewrite(Goal1, Input, Ans) :-
        catch(call(Goal1, Input, Ans),
            _,
            false).

    apply_transform(Goal1, Input, Acc, Ans) :-
        catch(call(Goal1, Input, Acc, Ans),
            _,
            false).

    % 
    all_rewrite_list(Goal1, Input, Ans) :-
        all_rewrite_list_aux(Input, Goal1, [], Ans).

    all_rewrite_list_aux([], _, Acc, Ans) :-
        {reverse(Acc, Ans)}.
        

    all_rewrite_list_aux([X|Xs], Goal1, Acc, Ans) :-
        apply_rewrite(Goal1, X, A1),
        all_rewrite_list_aux(Xs, Goal1, [A1|Acc], Ans).

    all_rewrite(R1, Input, Ans) :- 
        Input = rose_tree(Label, Kids),
        all_rewrite_list(R1, Kids, Kids1),
        Ans = rose_tree(Label, Kids1).        

:- end_object.

