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
        all_transform_list/4,
        any_rewrite_list/3,
        any_transform_list/4,

        alltd_rewrite/3
    ]).

    :- meta_predicate(apply_rewrite(2,*,*)).
    :- meta_predicate(apply_transform(3,*,*,*)).
    :- meta_predicate(all_rewrite_list(2,*,*)).
    :- meta_predicate(all_transform_list(3,*,*,*)).
    :- meta_predicate(any_rewrite_list(2,*,*)).
    :- meta_predicate(any_transform_list(3,*,*,*)).
    :- meta_predicate(one_rewrite_list(2,*,*)).
    :- meta_predicate(one_transform_list(3,*,*,*)).

    :- meta_predicate(alltd_rewrite(2,*,*)).

    :- mode(apply_rewrite(+callable, +term, -term), zero_or_more).
    :- mode(apply_transform(+callable, +term, +any, -term), zero_or_more).
    :- mode(all_rewrite_list(+callable, +term, -term), zero_or_more).

    :- mode(alltd_rewrite(+callable,+term,-term), zero_or_more).

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

    % Transform all the elements in a list.
    all_transform_list(Goal1, Input, Acc, Ans) :-
        all_transform_list_aux(Input, Goal1, Acc, Ans).

    all_transform_list_aux([], _, Ans, Ans).
        
    all_transform_list_aux([X|Xs], Goal1, Acc, Ans) :-
        apply_transform(Goal1, X, Acc, Acc1),
        all_transform_list_aux(Xs, Goal1, Acc1, Ans).

    % Rewrite any elements in a list where the rewrite succeeds.
    any_rewrite_list(Goal1, Input, Ans) :-
        any_rewrite_list_aux(Input, Goal1, [], Ans).

    any_rewrite_list_aux([], _, Acc, Ans) :-
        {reverse(Acc, Ans)}.

    any_rewrite_list_aux([X|Xs], Goal1, Acc, Ans) :-
        (apply_rewrite(Goal1, X, X0) -> X1 = X0; X1 = X),
        any_rewrite_list_aux(Xs, Goal1, [X1|Acc], Ans).


    % Transform any elements in a list.
    any_transform_list(Goal1, Input, Acc, Ans) :-
        any_transform_list_aux(Input, Goal1, Acc, Ans).

    any_transform_list_aux([], _, Ans, Ans).

    any_transform_list_aux([X|Xs], Goal1, Acc, Ans) :-
        (apply_transform(Goal1, X, Acc, A0) -> Acc1 = A0; Acc1 = Acc),
        any_transform_list_aux(Xs, Goal1, Acc1, Ans).


    % Rewrite one element, fail if none succeed.
    one_rewrite_list(Goal1, Input, Ans) :-
        one_rewrite_list_aux(Input, Goal1, [], Ans).

    one_rewrite_list_aux([], _, _, _) :-
        false.

    one_rewrite_list_aux([X|Xs], Goal1, Acc, Ans) :-
        (apply_rewrite(Goal1, X, X1) -> 
            {(reverse(Acc, Front), append(Front,[X1|Xs],Ans))}
            ; one_rewrite_list_aux(Xs, Goal1, [X|Acc], Ans)).


    % Transform one elements in a list.
    one_transform_list(Goal1, Input, Acc, Ans) :-
        one_transform_list_aux(Input, Goal1, Acc, Ans).

    one_transform_list_aux([], _, _, _) :- false.

    one_transform_list_aux([X|Xs], Goal1, Acc, Ans) :-
        (apply_transform(Goal1, X, Acc, A0) -> 
            Ans = A0; 
            one_transform_list_aux(Xs, Goal1, Acc, Ans)).
    
    alltd_rewrite(R1, Input, Ans) :-
        apply_rewrite(R1, Input, A1),
        A1 = rose_tree(Label1, Kids1),
        all_rewrite_list(alltd_rewrite(R1), Kids1, Kids2),
        Ans = rose_tree(Label1, Kids2).

:- end_object.

