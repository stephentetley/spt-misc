/*
    traversals.pl
    Copyright (c) Stephen Tetley 2019
    License: BSD 3 Clause
*/   

:- module(traversals, 
            [ rose_tree_rewrite/4
            , all_rewrite/3
            , any_rewrite/3
            , one_rewrite/3
            , rose_tree_transform/4
            , all_transform/4
            , any_transform/4
            , one_transform/4
            , alltd_rewrite/3
            , alltd_transform/4
            , allbu_rewrite/3
            , allbu_transform/4
            , anytd_rewrite/3
            , onetd_rewrite/3
            , onetd_transform/4
            ]).

:- use_module(structs).
:- use_module(traversals_base).

:- meta_predicate
    rose_tree_rewrite(2,3,+,-),
    all_rewrite(2,+,-),
    any_rewrite(2,+,-),
    one_rewrite(2,+,-),
    rose_tree_transform(3,+,+,-),
    all_transform(3,+,+,-),
    any_transform(3,+,+,-),
    one_transform(3,+,+,-),
    alltd_rewrite(2,+,-),
    anytd_rewrite(2,+,-),
    onetd_rewrite(2,+,-),
    alltd_transform(3,+,+,-),
    allbu_rewrite(2,+,-),
    allbu_transform(3,+,+,-), 
    onetd_transform(3,+,+,-).  


% So-called congruence combinators

rose_tree_rewrite(R1, Build, Input, Ans) :-
    is_rose_tree(Input),
    apply_rewrite(R1, Input, T1),
    rose_tree_label(T1,Label1),
    rose_tree_kids(T1,Kids),
    all_rewrite_list(R1, Kids, Kids1),
    call(Build, Label1, Kids1, Ans).

all_rewrite(R1, Input, Ans) :- 
    Input = rose_tree(Label,Kids),
    all_rewrite_list(R1, Kids, Kids1),
    call(cons_rose_tree, Label, Kids1, Ans).


any_rewrite(R1, Input, Ans) :- 
    Input = rose_tree(Label,Kids),
    any_rewrite_list(R1, Kids, Kids1),
    call(cons_rose_tree, Label, Kids1, Ans).


one_rewrite(R1, Input, Ans) :- 
    Input = rose_tree(Label,Kids),
    one_rewrite_list(R1, Kids, Kids1),
    call(cons_rose_tree, Label, Kids1, Ans).


rose_tree_transform(T1, Input, Acc, Ans) :-
    is_rose_tree(Input),
    apply_transform(T1, Input, Acc, Acc1),       % apply on root label
    rose_tree_kids(Input,Kids),
    all_transform_list(T1, Kids, Acc1, Ans).
    

all_transform(T1, Input, Acc, Ans) :- 
    Input = rose_tree(_,Kids),
    all_transform_list(T1, Kids, Acc, Ans).

any_transform(T1, Input, Acc, Ans) :- 
    Input = rose_tree(_,Kids),
    any_transform_list(T1, Kids, Acc, Ans).

one_transform(T1, Input, Acc, Ans) :- 
    Input = rose_tree(_,Kids),
    one_transform_list(T1, Kids, Acc, Ans).


alltd_rewrite(R1, Input, Ans) :-
    apply_rewrite(R1, Input, A1),
    A1 = rose_tree(Label1, Kids1),
    all_rewrite_list(alltd_rewrite(R1), Kids1, Kids2),
    cons_rose_tree(Label1, Kids2, Ans).
    


anytd_rewrite(Goal1, Tree, Ans) :-
    (apply_rewrite(Goal1, Tree, A0) -> Ans1 = A0; Ans1 = Tree),
    Ans1 = rose_tree(Label1, Kids1),
    any_rewrite_list(anytd_rewrite(Goal1), Kids1, Kids2),
    cons_rose_tree(Label1, Kids2, Ans).


onetd_rewrite(Goal1, Input, Ans) :-
    (apply_rewrite(Goal1, Input, A0) -> 
        Ans = A0
    ;   Input = rose_tree(Label1, Kids1),
        one_rewrite_list(onetd_rewrite(Goal1), Kids1, Kids2),
        cons_rose_tree(Label1, Kids2, Ans)).


alltd_transform(T1, Input, Acc, Ans) :-
    apply_transform(T1, Input, Acc, Acc1),
    Input = rose_tree(_, Kids1),
    all_transform_list(alltd_transform(T1), Kids1, Acc1, Ans).
    

allbu_rewrite(Goal1, Tree, Ans) :-
    A1 = rose_tree(Label1, Kids1),
    reverse(Kids1, Revlist),
    all_rewrite_list(alltd_rewrite(Goal1), Revlist, Kids2),
    apply_rewrite(Goal1, Tree, A1),
    cons_rose_tree(Label1, Kids2, Ans).    

allbu_transform(Goal1, Tree, Acc, Ans) :-
    Tree = rose_tree(_, Kids1),
    reverse(Kids1, Revlist),
    all_transform_list(allbu_transform(Goal1), Revlist, Acc, Acc1),
    apply_transform(Goal1, Tree, Acc1, Ans).
    
onetd_transform(T1, Input, Acc, Ans) :-
    (apply_transform(T1, Input, Acc, A0) -> 
        Ans = A0
    ;   Input = rose_tree(_, Kids1),
        one_transform_list(onetd_transform(T1), Kids1, Acc, Ans)).


