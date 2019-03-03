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
            , alltd_rewrite/3
            ]).

:- use_module(structs).
:- use_module(traversals_base).

:- meta_predicate
    rose_tree_rewrite(2,3,+,-),
    all_rewrite(2,+,-),
    any_rewrite(2,+,-),
    one_rewrite(2,+,-),
    alltd_rewrite(2,+,-).        


% So called congruence combinators

rose_tree_rewrite(R1, Build, Tree, Ans) :-
    is_rose_tree(Tree),
    apply_rewrite(R1, Tree, T1),
    rose_tree_label(T1,Label1),
    rose_tree_kids(T1,Kids),
    all_rewrite_list(R1, Kids, Kids1),
    call(Build, Label1, Kids1, Ans).

all_rewrite(Trafo, Tree, Ans) :- 
    rose_tree_rewrite(Trafo, cons_rose_tree, Tree, Ans).


any_rewrite(Trafo, Tree, Ans) :- 
    Tree = rose_tree(Label,Kids),
    any_rewrite_list(Trafo, Kids, Kids1),
    call(cons_rose_tree, Label, Kids1, Ans).


one_rewrite(Trafo, Tree, Ans) :- 
    Tree = rose_tree(Label,Kids),
    one_rewrite_list(Trafo, Kids, Kids1),
    call(cons_rose_tree, Label, Kids1, Ans).


alltd_rewrite(Goal1, Tree, Ans) :-
    apply_rewrite(Goal1, Tree, A1),
    A1 = rose_tree(Label1, Kids1),
    all_rewrite_list(alltd_rewrite(Goal1), Kids1, Kids2),
    cons_rose_tree(Label1, Kids2, Ans).
    
