/* 
    regularexp.pl
*/


operators([ '?', '+', '*', '!', '_', '/', '(', ')', '|' ]).

symbol(X) :- 
    atomic(X),
    operators(Ops), 
    \+(member(X, Ops)),
    !.


regex(alt(X,Y)) -->
    term(X), ['|'], regex(Y).

regex(X) --> term(X).


term([H|T]) --> factor(H), term(T).
term([]) --> [].

factor(star(X)) --> base(X), ['*'].
factor(plus(X)) --> base(X), ['+'].
factor(opt(X)) --> base(X), ['?'].

factor(X) --> base(X).


base(group(X)) --> 
    ['('], regex(X), [')'].

base(symbol(X)) --> symbol(X).

symbol(S) -->
    [S], { symbol(S) }.


translate(Expr, Ans) :- 
    phrase(regex(Ans), Expr, []).


demo01(Ans) :- 
    translate(['(', a, '+', '|', b, '?', ')'], Ans), !. 





