/*
    demo.pl
    Copyright (c) Stephen Tetley 2019
    License: BSD 3 Clause
*/ 


:- use_module(structs).
:- use_module(traversals_base).
:- use_module(traversals).


tree1(rose_tree(1,[ rose_tree(2,[ rose_tree(4,[])
                                , rose_tree(5,[])])
                    , rose_tree(3,[])
                    ])).

tree2(rose_tree(1,[ rose_tree(2,[ rose_tree(4,[])
                                , rose_tree(5,[])])
                , rose_tree(-10,[])
                ])).

demoZ1 :-
    tree1(Body),
    is_rose_tree(Body).

demoZ2(Ans) :- 
    make_rose_tree([label(1),kids([])],Ans).

demoZ3(Ans) :- 
    cons_rose_tree(1,[],Ans).


id(Ans,Ans).
add1(A,Ans) :- 
    Ans is A + 1.

add1_rewrite(A, Ans) :- 
    A = rose_tree(X,Xs),
    (integer(X) -> Y is X+1, cons_rose_tree(Y, Xs, Ans)).


positive_rewrite(A, Ans) :- 
    A = rose_tree(X,Xs),
    (X < 0 -> Y is abs(X), cons_rose_tree(Y, Xs, Ans)).


demo01(Ans) :- 
    tree1(Body),
    rose_tree_rewrite(id, cons_rose_tree, Body, Ans).

demo02(Ans) :- 
    tree1(Body),
    rose_tree_rewrite(add1_rewrite, cons_rose_tree, Body, Ans).

demo03(Ans) :- 
    tree1(Body),
    any_rewrite(add1_rewrite, Body, Ans).


demo04(Ans) :- 
    tree1(Body),
    one_rewrite(add1_rewrite, Body, Ans).

demo04a(Ans) :- 
    tree1(Body),
    one_rewrite(positive_rewrite, Body, Ans).

demo04b(Ans) :- 
    tree2(Body),
    one_rewrite(positive_rewrite, Body, Ans).

demo05(Ans) :- 
    tree1(Body),
    alltd_rewrite(add1_rewrite, Body, Ans).