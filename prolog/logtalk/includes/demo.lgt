/* 
    demo.lgt
*/

:- object(test).

    %% This is properly exported by film_facts (include.lgt).
    :- public(demo01/2).
    demo01(X, Y) :- film_facts::star(X,Y).


    %% This is not exported by film_facts. What happens?
    %% ... runtime error
    :- public(demo02/2).
    demo02(X, Y) :- film_facts::director(X,Y).

    :- public(demo03/0).
    demo03 :- 
        common_lib::output_csv('test.csv', row("Name", "Job"), [row("Enrique Vila-Matas","Writer"), row("Henry", "Cleaner")]).

    :- public(demo04/1).
    demo04(Ans) :- 
        common_lib::nth1_cell(1, row("A",2,"C"), Ans).

:- end_object.	