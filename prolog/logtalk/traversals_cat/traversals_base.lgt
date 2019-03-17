/* 
    traversals_base.lgt
*/

:- protocol(walkerp).
    
    :- public(all_rewrite/3).
    :- mode(all_rewrite(+callable, +term, -term), zero_or_more).

:- end_protocol.

:- category(traversals).
    
    :- public([
        id_rewrite/2,
        fail_rewrite/2,
        apply_rewrite/3, 
        all_rewrite_list/3,
        alltd_rewrite/3
    ]).

    :- mode(id_rewrite(+term, -term), one).
    id_rewrite(Ans, Ans).

    :- mode(fail_rewrite(+term, -term), zero).
    fail_rewrite(_, _) :- false.        

    :- meta_predicate(apply_rewrite(2,*,*)).
    :- mode(apply_rewrite(+callable, +term, -term), zero_or_more).
    apply_rewrite(Goal1, Input, Ans) :-
        writeln("apply_rewrite"),
        writeln(Input),
        call(Goal1, Input, Ans).
/*         catch( call(Goal1, Input, Ans),
            _,
            false). */

    % all_rewrite_list
    :- meta_predicate(all_rewrite_list(2,*,*)).
    :- mode(all_rewrite_list(+callable, +term, -term), zero_or_more).
    all_rewrite_list(Goal1, Input, Ans) :-
        all_rewrite_list_aux(Input, Goal1, [], Ans).

    all_rewrite_list_aux([], _, Acc, Ans) :-
        {reverse(Acc, Ans)}.
        
    all_rewrite_list_aux([X|Xs], Goal1, Acc, Ans) :-
        ::apply_rewrite(Goal1, X, A1),
        all_rewrite_list_aux(Xs, Goal1, [A1|Acc], Ans).

    % all_rewrite_list
    :- meta_predicate(alltd_rewrite(2,*,*)).
    :- mode(alltd_rewrite(+callable, +term, -term), zero_or_more).
    alltd_rewrite(R1, Input, Ans) :-
        ::apply_rewrite(R1, Input, A1),
        ::all_rewrite(::alltd_rewrite(R1), A1, Ans).

:- end_category.


