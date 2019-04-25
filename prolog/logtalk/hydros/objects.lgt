/*
    objects.lgt
*/   

:- object(hydro(_Id, _Spill, _Empty, _Relays)).

	:- public([id/1, spill/1, empty_distance/1, relays/1]).

	id(Ans) :- 
		parameter(1, Ans).
	
	spill(Ans) :- 
		parameter(2, Ans).

	empty_distance(Ans) :- 
		parameter(3, Ans).
	
	relays(Ans) :- 
		parameter(4, Ans).


:- end_object.

:- protocol(relayp).

	:- public([ index/1, param/1 ]).

:- end_protocol.

:- object(relay(_Index, _Param),
	implements(relayp)).

	index(Ans) :- 
		parameter(1, Ans).

	param(Ans) :- 
		parameter(2, Ans).

:- end_object.

:- object(measure_relay(_Index, _Param, _Start, _Stop), 
	implements(relayp)).

	index(Ans) :- 
			parameter(1, Ans).
	
	param(Ans) :- 
		parameter(2, Ans).
	
	:- public(start/1).
	start(Ans) :- 
		parameter(3, Ans).

	:- public(stop/1).
	stop(Ans) :- 
		parameter(4, Ans).

:- end_object.
