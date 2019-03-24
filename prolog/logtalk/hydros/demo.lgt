/* 
	demo.lgt
*/

:- object(demo).

	:- public(get_facts/1).
	get_facts(Ans) :- 
		hydro_facts::hydros_db(Ans).

	:- public(demo1/1).
	demo1(Ans) :- 
		get_facts([H1|_]),
		H1::relays([R1|_]),
		R1::param(Ans).
		
	
:- end_object.	