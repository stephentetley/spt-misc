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

%% Slow
snoc(X, Acc, Ans) :- 
    append(Acc, [X], Ans).

/*
cons(X,Xs,Ans) :- 
    Ans = [X|Xs].
*/

cons(A,[],[A]) :- !.
cons(A,[H|T],[A,H|T]).


%% Discrepancy - should the "payload" (here the integer 1) appear in the list of children?
%% It would in syb-xpath, but things are generalized in KURE...
%% The answer actually depends on the "universe" and now it is defined 
%% KURE JFP Paper Section 4.3.2 Alternative Universes

all_children(Input,Ans) :-
    all_transform(snoc,Input,[],Ans).


demo11(Ans) :-
    tree1(Body),
    all_children(Body,Ans).

node_of_type(Type, Input) :-
    Input =.. [Type|_].

child_of_type_aux(Type, X, Acc, Ans) :-
    (node_of_type(Type,X) -> 
        append(Acc, X, Ans)
    ; Ans = Acc ).

child_of_type(Constructor, Input, Ans) :- 
    any_transform(child_of_type_aux(Constructor), Input, [], Ans).

demo12(Ans) :-
    tree1(Body),
    child_of_type(rose_tree, Body, Ans).

%% Digression - diff lists seem to upset the arity of our traversals...

%% diff lists
dappend(A-B, B-C, A-C).

dl([], L-L).
dl([H|T], [H|L1]-L2) :-
    dl(T, L1-L2).

undl(X-[],X).

demo_dl(Ans) :- 
    dl([1,2,3], Dxs),
    dl([4,5,6], Dys),
    dappend(Dxs, Dys, A1),
    undl(A1,Ans).

/* snocdl(A-B,):-
    dappend(A-B,) */

%% diff lists without pairs
dappend(A, B, B, C, A, C).

dl([], L, L).
dl([H|T], [H|L1], L2) :-
    dl(T, L1, L2).

dempty(X1, X2) :-
    dl([],X1,X2).

dsingleton(A, X1, X2) :-
    dl([A], X1, X2).

%% tail variable remains the same
dcons(A, [H|T], X2, Y1, X2) :-
    Y1 = [A,H|T].
    
dsnoc(A, X1, X2, Y1, Y2) :-
    dsingleton(A,B1,B2),
    dappend(X1,X2,B1,B2,Y1,Y2).





undlist(X,[],X).


demo_dl2(Ans) :- 
    dl([1,2,3], X1, X2),
    dl([4,5,6], Y1, Y2),
    dappend(X1, X2, Y1, Y2, A1, A2),
    undlist(A1, A2, Ans).






    

