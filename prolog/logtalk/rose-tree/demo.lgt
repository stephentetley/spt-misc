%% terminal> & 'C:\Program Files\swipl\bin\swipl.exe'
%% ?- use_module(library(logtalk)).
%% ?- logtalk_load(loader).
%% ?- logtalk_load(demo).

:- object(demo).
    
    :- public([
        demoZ/0,
        demoZ2/1,
        demo01/1,
        demo02/1,
        demo03/1,
        demo04/1,
        demo05/1
    ]).

    demoZ :- 
        write("demo1").

    demoZ2(Xs) :- 
        { reverse([3,2,1], Xs) }.


    demo01(Ans) :- 
        X = rose_tree(1,[]),
        traversals_base::id_rewrite(X,Ans).

    add1_rewrite(Input, Ans) :- 
        Input = rose_tree(X,Xs),
        Y is X+1,
        Ans = rose_tree(Y, Xs).

        
    
    tree1(rose_tree(1,[ rose_tree(2,[ rose_tree(4,[])
                                , rose_tree(5,[])])
                    , rose_tree(3,[])
                    ])).

    demo02(Ans) :- 
        tree1(Body),
        traversals_base::apply_rewrite(add1_rewrite, Body, Ans).        

    :- public(demo02b/1).
    demo02b(Ans) :- 
        tree1(Body),
        traversals_base::apply_rewrite(traversals_base::id_rewrite, Body, Ans).  

    demo03(Ans) :- 
        tree1(Body),
        traversals_base::all_rewrite_list(add1_rewrite, [Body,Body], Ans). 

    demo04(Ans) :- 
        tree1(Body),
        traversals::all_rewrite(add1_rewrite, Body, Ans).

    demo05(Ans) :- 
        tree1(Body),
        traversals_base::alltd_rewrite(add1_rewrite, Body, Ans).

:- end_object.

