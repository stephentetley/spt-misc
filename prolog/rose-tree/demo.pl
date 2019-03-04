/*
    demo.pl
    Copyright (c) Stephen Tetley 2019
    License: BSD 3 Clause
*/ 


:- use_module(structs).
:- use_module(traversals_base).
:- use_module(traversals).

:- use_module(library(apply)).

% 
% prelude, what is the arg order of foldl?
% answer: x -> acc -> acc

accum(Int, AccString, Ans) :-
    format(string(S1), "+~d", Int),
    string_concat(AccString, S1, Ans).

test_foldl(Ans) :- 
    foldl(accum,[1,2,3,4,5],"[]", Ans).

%
% End prelude



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

% Note - Prolog's argument order for fold is `step`.
% step : x -> acc -> acc
sum_trafo(A, Acc, Ans) :- 
    A = rose_tree(X,_),
    integer(X), 
    integer(Acc),
    Ans is Acc + X.

demo06(Ans) :- 
    tree1(Body),
    alltd_transform(sum_trafo, Body, 0, Ans).

count_trafo(A, Acc, Ans) :- 
    format(string(Debug), "count_trafo: seen ~w", A),
    writeln(Debug),
    A = rose_tree(_,_),
    integer(Acc),
    Ans is Acc + 1.

% Note allbu is now traverses the kids list "backwards"
demo07(Ans) :- 
    tree1(Body),
    allbu_transform(count_trafo, Body, 0, Ans).

demo08(Ans) :- 
    tree1(Body),
    anytd_rewrite(add1_rewrite, Body, Ans).

demo09(Ans) :- 
    tree1(Body),
    onetd_rewrite(add1_rewrite, Body, Ans).

demo10(Ans) :- 
    tree1(Body),
    onetd_transform(count_trafo, Body, 0, Ans).
