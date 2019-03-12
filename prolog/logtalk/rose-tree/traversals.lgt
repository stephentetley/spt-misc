/* 
    traversals.lgt
*/

:- object(traversals).


    :- public([
        all_rewrite/3
    ]).

    :- meta_predicate(all_rewrite(2,*,*)).

    all_rewrite(R1, Input, Ans) :- 
        writeln("Start..."),
        % Input = rose_tree(Label, Kids),
        Input::kids(Kids),
        Input::label(Label),
        writeln(Kids),

        traversals_base::all_rewrite_list(R1, Kids, Kids1),
        write(Kids1),
        Kids1 = [],  %% temp
        Ans = rose_tree(Label, Kids1).
        


:- end_object.

