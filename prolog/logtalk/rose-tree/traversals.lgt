/* 
    traversals.lgt
*/

:- object(traversals,
    extends(traversals_base)).

    :- public([
        all_rewrite/3,
        any_rewrite/3, 
        one_rewrite/3,
        all_transform/4,
        any_transform/4,
        one_transform/4,
        alltd_rewrite/3,
        anytd_rewrite/3,
        onetd_rewrite/3
    ]).

    :- meta_predicate(all_rewrite(2,*,*)).
    :- meta_predicate(any_rewrite(2,*,*)).
    :- meta_predicate(one_rewrite(2,*,*)).
    :- meta_predicate(all_transform(3,*,*,*)).
    :- meta_predicate(any_transform(3,*,*,*)).
    :- meta_predicate(one_transform(3,*,*,*)).
    :- meta_predicate(alltd_rewrite(2,*,*)).
    :- meta_predicate(anytd_rewrite(2,*,*)).
    :- meta_predicate(onetd_rewrite(2,*,*)).

    :- mode(alltd_rewrite(+callable,+term,-term), zero_or_more).

    all_rewrite(R1, Input, Ans) :- 
        Input = rose_tree(Label, Kids),
        ^^all_rewrite_list(R1, Kids, Kids1),
        Ans = rose_tree(Label, Kids1).

    any_rewrite(R1, Input, Ans) :- 
        Input = rose_tree(Label,Kids),
        ^^any_rewrite_list(R1, Kids, Kids1),
        Ans = rose_tree(Label, Kids1).

    one_rewrite(R1, Input, Ans) :- 
        Input = rose_tree(Label,Kids),
        ^^one_rewrite_list(R1, Kids, Kids1),
        Ans = rose_tree(Label, Kids1).

    all_transform(T1, rose_tree(_,Kids), Acc, Ans) :- 
        ^^all_transform_list(T1, Kids, Acc, Ans).

    any_transform(T1, rose_tree(_,Kids), Acc, Ans) :- 
        ^^any_transform_list(T1, Kids, Acc, Ans).

    one_transform(T1, rose_tree(_,Kids), Acc, Ans) :- 
        ^^one_transform_list(T1, Kids, Acc, Ans).


    alltd_rewrite(R1, Input, Ans) :-
        writeln("1."),
        ^^apply_rewrite(R1, Input, A1),
        writeln("2.."),
        A1 = rose_tree(Label1, Kids1),
        writeln("3..."),
        writeln(Kids1),
        ^^all_rewrite_list(::alltd_rewrite(R1), Kids1, Kids2),
        writeln("4...."),
        Ans = rose_tree(Label1, Kids2).
        

    anytd_rewrite(Goal1, Tree, Ans) :-
        (^^apply_rewrite(Goal1, Tree, A0) -> Ans1 = A0; Ans1 = Tree),
        Ans1 = rose_tree(Label1, Kids1),
        ^^any_rewrite_list(anytd_rewrite(Goal1), Kids1, Kids2),
        Ans = rose_tree(Label1, Kids2).

    %% Check this matches the spec.
    onetd_rewrite(Goal1, Input, Ans) :-
        (^^apply_rewrite(Goal1, Input, A0) -> 
            Ans = A0
        ;   Input = rose_tree(Label1, Kids1),
            ^^one_rewrite_list(onetd_rewrite(Goal1), Kids1, Kids2),
            Ans = rose_tree(Label1, Kids2)).

:- end_object.

