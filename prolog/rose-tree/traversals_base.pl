/*
    traversals_base.pl
    Copyright (c) Stephen Tetley 2019
    License: BSD 3 Clause
*/   

:- module(traversals_base, 
            [ id_rewrite/2
            , fail_rewrite/2
            , fail_transform/3
            , apply_rewrite/3
            , apply_transform/4
            , sequence_rewrite/4
            , sequence_transform/5
            , all_rewrite_list/3
            , all_transform_list/4
            , any_rewrite_list/3
            , one_rewrite_list/3
            , const_transform/3
            ]).


:- meta_predicate
        apply_rewrite(2,+,-),
        apply_transform(3,+,+,-),
        sequence_rewrite(2,2,+,-),
        sequence_transform(3,3,+,+,-),
        all_rewrite_list(2,+,-), 
        all_transform_list(2,+,+,-), 
        any_rewrite_list(2,+,-),
        one_rewrite_list(2,+,-).

%! id_rewrite(Input, Ans)
% 
% Always succeeds.

id_rewrite(Ans, Ans).

%! fail_rewrite(Input, Ans)
% 
% Always fails.

fail_rewrite(_ , _) :- false.

%! fail_transform(Acc, Input, Ans)
% 
% Always fails.

fail_transform(_, _ , _) :- false.

%! apply_rewrite(Goal1, Input, Ans)
% 
% Apply a rewrite - if it fails, catch error and return false.

apply_rewrite(Goal1, Input, Ans) :-
    catch(call(Goal1, Input, Ans),
        _,
        false).


apply_transform(Goal1, Input, Acc, Ans) :-
    catch(call(Goal1, Input, Acc, Ans),
        _,
        false).

sequence_rewrite(Goal1, Goal2, Input, Ans) :-
    apply_rewrite(Goal1, Input, A1),
    apply_rewrite(Goal2, A1, Ans).


sequence_transform(Goal1, Goal2, Input, Acc, Ans) :-
    apply_transform(Goal1, Input, Acc, A1),
    apply_transform(Goal2, Input, A1, Ans).

% 
all_rewrite_list(Goal1, Input, Ans) :-
    all_rewrite_list_aux(Input, Goal1, [], Ans).

all_rewrite_list_aux([], _, Acc, Ans) :-
    reverse(Acc, Ans).
    

all_rewrite_list_aux([X|Xs], Goal1, Acc, Ans) :-
    apply_rewrite(Goal1, X, A1),
    all_rewrite_list_aux(Xs, Goal1, [A1|Acc], Ans).


% Transform all the elements in a list.
all_transform_list(Goal1, Input, Acc, Ans) :-
    all_transform_list_aux(Input, Goal1, Acc, Ans).

all_transform_list_aux([], _, Ans, Ans).
    

all_transform_list_aux([X|Xs], Goal1, Acc, Ans) :-
    apply_transform(Goal1, X, Acc, Acc1),
    all_transform_list_aux(Xs, Goal1, Acc1, Ans).

% Rewrite any elements in a list where the rewrite succeeds.
any_rewrite_list(Goal1, Input, Ans) :-
    any_rewrite_list_aux(Input, Goal1, [], Ans).

any_rewrite_list_aux([], _, Acc, Ans) :-
    reverse(Acc, Ans).


any_rewrite_list_aux([X|Xs], Goal1, Acc, Ans) :-
    (apply_rewrite(Goal1, X, X0) -> X1 = X0; X1 = X),
    any_rewrite_list_aux(Xs, Goal1, [X1|Acc], Ans).


% Rewrite one element, fail if none succeed.
one_rewrite_list(Goal1, Input, Ans) :-
    one_rewrite_list_aux(Input, Goal1, [], Ans).

one_rewrite_list_aux([], _, _, _) :-
    false.


one_rewrite_list_aux([X|Xs], Goal1, Acc, Ans) :-
    (apply_rewrite(Goal1, X, X1) -> 
        (reverse(Acc, Front), append(Front,[X1|Xs],Ans))
        ; one_rewrite_list_aux(Xs, Goal1, [X|Acc], Ans)).

%! const_transform(Value, Input, Ans)
% 

const_transform(Ans, _, Ans).    