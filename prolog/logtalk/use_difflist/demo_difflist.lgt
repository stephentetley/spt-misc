/* 
	demo_difflist.lgt 
*/

% ?- logtalk_load(library(types_loader)).

:- object(demo).

    :- public(test01/1).
    test01(Ans) :- 
        list::as_difflist([1,2,3,4],Ans).

    % difflist::empty(X) is a test for null, not the constructor for an empty difflist
    :- public(test02/1).
    test02(Ans) :- 
        difflist::empty(Ans).
            
    :- public(test03/1).
    test03(Ans) :- 
        list::as_difflist([],Ans).

    :- public(new_difflist/1).
    new_difflist(Ans) :- 
        list::as_difflist([],Ans).


    :- public(test04/1).
    test04(Ans) :- 
        ::new_difflist(L1),
        difflist::add(1,L1,L2),
        difflist::add(2,L2,L3),
        difflist::add(3,L3,L4),
        difflist::as_list(L4,Ans).
    
    % Note - the empty difflist is a pair of the same variable.

    :- public(test05/1).
    test05(Ans) :- 
        difflist::add(1,X-X,L2),
        difflist::add(2,L2,L3),
        difflist::add(3,L3,L4),
        difflist::add(4,L4,L5),
        difflist::add(5,L5,L6),
        difflist::as_list(L6,Ans).        


:- end_object.