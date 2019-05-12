/*
    identity.lgt 
*/
:- use_module(library(record)).



:- object(identity).


    :- public(run/2).
    run(id(X), X).

    :- public(mreturn/2).
    mreturn(X, id(X)).
        

    :- public(mbind/3).
    :- meta_predicate(mbind(*, 2, *)).
    :- mode(mbind(+id, +callable, -id), one).
    mbind(id(A), K, Ans) :- 
        call(K, A, Ans).
    
    :- public(fmap/3).
    :- meta_predicate(fmap(2, *, *)).
    :- mode(fmap(+callable, +id, -id), one).
    fmap(Fn, id(A), Ans) :-
        call(Fn, A, Temp),
        Ans = id(Temp).


    % TODO probably commands should be in a list

    :- public(return//1).
    return(Val), [Ans] --> [_], { identity::mreturn(Val, Ans) }.

    :- public(bind//2).
    :- meta_predicate(bind(*, 2)).
    bind(Ma, Fn), [Ans] --> [_], { identity::mbind(Ma, Fn, Ans) }.

    :- public(identity_do/2).
    identity_do(Expr, X) :- 
        phrase(Expr, [0], [id(X)]).

:- end_object. 

:- object(demo).
    
    :-uses(identity, [identity_do/2, return//1, bind//2]).

    :- public(test01/1).
    test01(Ans) :- 
        identity::mreturn(4, Action),
        identity::run(Action, Ans).    

    :- public(test02/1).
    test02(Ans) :- 
        identity::mreturn(4, Action0),
        identity::fmap([X,Y] >> (Y is X + 1), Action0, Action1),
        identity::run(Action1, Ans).    

    :- public(test03/1).
    test03(Ans) :- 
        identity::mreturn(4, M1),
        identity::mbind(M1, ([X,M2] >> (identity::mreturn(X, M2))), Action1),
        identity::run(Action1, Ans).  


    :- public(test04/1).
    test04(Ans) :- 
        identity_do(return(5), Ans).  

    :- public(test05/1).
    test05(Ans) :- 
        Fn = ([_,Ans1] >> (Ans1 = return("hello"))),
        identity_do(bind(return(2), Fn), Ans). 

    :- public(test05a/1).
    test05a(Ans) :- 
        identity::mreturn(4, M1),
        identity_do(bind(M1, ([_,M2] >> (identity::mreturn("hello", M2)))), Ans). 

    :- public(test05b/1).
    test05b(Ans) :- 
        identity::mreturn(4, M1),
        identity_do(bind(M1, ([_,Ans1] >> (Ans1 = return("hello")))), Ans). 

:- end_object.    