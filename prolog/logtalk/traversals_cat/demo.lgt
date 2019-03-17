/* 
    demo.lgt
*/

:- object(rose_tree_rewrites).

    :- public(add1/2).    
    :- mode(add1(+term, -term), zero_or_more).    
    add1(Input, Ans) :- 
        Input = rose_tree(X,Xs),
        Y is X+1,
        Ans = rose_tree(Y, Xs).
        
:- end_object.    


:- object(bin_tree_rewrites).

    :- public(add1/2).    
    :- mode(add1(+term, -term), zero_or_more).    
    add1(bin_tree(X, Left, Right), Ans) :- 
        Y is X+1,
        Ans = bin_tree(Y, Left, Right).

    add1(empty, empty).
        
:- end_object. 

:- object(demo).

    btree1(bin_tree(1
                   , bin_tree(2
                             , bin_tree(3, empty, empty)
                             , bin_tree(4, empty, empty))
                   , bin_tree(5,empty,empty))).

    rtree1(rose_tree(1, [ rose_tree(2,  [ rose_tree(3,[])
                                        , rose_tree(4,[])])
                        , rose_tree(5,[])
                    ])).    

    :-public(demo01/1).
    demo01(Ans) :- 
        rtree1(Input),
        Input::label(Ans).   

     :-public(demo02/1).
    demo02(Ans) :- 
        rtree1(Input),
        rose_tree_traversals::id_rewrite(Input, Ans).   

    :-public(demo03/1).
    demo03(Ans) :- 
        rtree1(Input),
        rose_tree_traversals::fail_rewrite(Input, Ans).  

    :-public(demo04/1).
    demo04(Ans) :- 
        rtree1(Input),
        rose_tree_traversals::apply_rewrite(rose_tree_traversals::id_rewrite, Input, Ans).  

    :-public(demo05/1).
    demo05(Ans) :- 
        btree1(Input),
        bin_tree_traversals::apply_rewrite(bin_tree_traversals::id_rewrite, Input, Ans).  


    :-public(demo06/1).
    demo06(Ans) :- 
        btree1(Input),
        bin_tree_traversals::apply_rewrite(bin_tree_rewrites::add1, Input, Ans). 


    :-public(demo07z/1).
    demo07z(Ans) :- 
        btree1(Input),
        bin_tree_traversals::all_rewrite(bin_tree_traversals::id_rewrite, Input, Ans), !. 

    :-public(demo07/1).
    demo07(Ans) :- 
        btree1(Input),
        bin_tree_traversals::all_rewrite(bin_tree_rewrites::add1, Input, Ans). 

    :-public(demo08/1).
    demo08(Ans) :- 
        btree1(Input),
        bin_tree_traversals::alltd_rewrite(bin_tree_rewrites::add1, Input, Ans). 

    :-public(demo09/1).
    demo09(Ans) :- 
        rtree1(Input),
        rose_tree_traversals::alltd_rewrite(rose_tree_rewrites::add1, Input, Ans). 

    :-public(demo10/1).
    demo10(Ans) :- 
        Input = rose_tree(1,[rose_tree(2,[])]),
        rose_tree_traversals::alltd_rewrite(rose_tree_rewrites::add1, Input, Ans). 

    :-public(demo11/1).
    demo11(Ans) :- 
        btree1(Input),
        bin_tree_traversals::alltd_rewrite(bin_tree_rewrites::add1, Input, Ans). 
    
    :-public(demo11z/1).
    demo11z(Ans) :- 
        Input = bin_tree(1, empty, empty),
        bin_tree_traversals::alltd_rewrite(bin_tree_rewrites::add1, Input, Ans). 
    
    :-public(demo11zz/1).
    demo11zz(Ans) :- 
        Input = bin_tree(1, bin_tree(2, empty, empty), empty),
        bin_tree_traversals::alltd_rewrite(bin_tree_rewrites::add1, Input, Ans). 


:- end_object.