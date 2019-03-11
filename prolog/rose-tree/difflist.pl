/*
    difflist.pl
    Copyright (c) Stephen Tetley 2019
    License: BSD 3 Clause
*/ 



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




