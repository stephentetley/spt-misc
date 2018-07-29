% queries_rts.pl

:- use_module(facts/rts_picture_names).
:- use_module(facts/rts_asset_to_signal).
:- use_module(facts/rts_picture_facts).
:- use_module(facts/rts_pump_facts).
:- use_module(facts/rts_screen_facts).
:- use_module(utils).

/*
 * Comment
*/



picture_long_name(OS, Point, Name) :- 
    rts_picture(P, OS, Point),
    rts_picture_name(P,Name).

% picture_long_name( 'CHERRY_BURTON_STW', 'RBC_NO1_DRIVE_P', X).

all_pictures(Os, Pictures) :- 
    findall(X, rts_picture(X,Os,_), Xs),
    sort(Xs,Pictures).


all_picture_names(Xs) :-
    findall(Y, rts_picture_name(_,Y), Xs).



all_assets(Os, Xs) :-
    findall(Y, asset_to_signal(Os,Y,_,_), Xs_),
    sort(Xs_, Xs).


match_frp_(Xs) :- 
    length(Xs,3), 
    subset(['F','R','P'],Xs).
    

match_fr_(Xs) :- 
    length(Xs,2), 
    subset(['F','R'],Xs).
    
        
match_raf_(Xs) :- 
    length(Xs,3), 
    subset(['R','A','F'],Xs).
    
% Disjunction needs the cut! and the code below isn't working properly anyway
% hasStandardSignalsZ(Sigs) :-  
%     !,
%     isFRP(Sigs);
%     isFR(Sigs);
%     isRAF(Sigs).


% This is not so important anymore as the fact extractor is more "accurate".
% Use multiple rules rather than disjunction.
has_standard_signals_(Sigs) :- match_frp_(Sigs).
has_standard_signals_(Sigs) :- match_fr_(Sigs).
has_standard_signals_(Sigs) :- match_raf_(Sigs).


% isFRP(['F','P', 'R']).
% has_standard_signals_(['F','P', 'R']).



% Screens

all_pumps(Os, Xs) :- 
    findall(X, rts_pump(Os,X,_), Xs).


has_standard_pump_signals(Os, Point) :-   
    rts_pump(Os, Point, Sigs),
    has_standard_signals_(Sigs).

% has_standard_pump_signals('CHERRY_BURTON_STW', 'PRIMARY_SETTLE_TANK_3_PUMP').
% has_standard_pump_signals('CHERRY_BURTON_STW', 'WASH_PUMP_1').

% pumps with standard signals
pumps_with_standard_signals(Os, Xs) :- 
    all_pumps(Os, Names),
    include(has_standard_pump_signals(Os), Names, Xs).

% pumps_with_standard_signals('CHERRY_BURTON_STW', Xs).

% This may include elements that are not pumps, that have slipped 
% through fact extraction.
pumps_with_nonstandard_signals(Os, Xs) :- 
    all_pumps(Os, Names),
    exclude(has_standard_pump_signals(Os), Names, Xs).

% pumps_with_nonstandard_signals('CHERRY_BURTON_STW', Xs).

% Screens  
% This is not nondeterministic, which is an error.  
all_screens(Os, Screens) :- 
    findall(S, rts_screen(Os, S, _), Screens).

% all_screens('THORNTON_DALE_STW', Xs).

has_standard_screen_signals(Os, Point) :-   
    rts_screen(Os,Point,Sigs),
    has_standard_signals_(Sigs).

% pumps with standard signals
% Os must be ground.
screens_with_standard_signals(Os, Screens) :- 
    all_screens(Os, Names),
    include(has_standard_screen_signals(Os),Names,Screens).

% Os must be ground.
screens_with_nonstandard_signals(Os, Screens) :- 
    all_screens(Os, Names),
    exclude(has_standard_screen_signals(Os), Names, Screens).

% screens_with_nonstandard_signals('THORNTON_DALE_STW', Ys).

screen_points(Os, Screen, Points) :-
    findall(X, asset_to_signal(Os,Screen,X,_), Points).

% screen_points('THORNTON_DALE_STW', 'INLET_SCREEN_MACI_PUMP', Points).

get_picture_(Os, Point, Picture) :- 
    rts_picture(Picture, Os, Point).

screen_to_picture(Os, Screen, Pictures) :- 
    screen_points(Os, Screen, Points),
    convlist(get_picture_(Os), Points, Pictures).

% screen_to_picture('THORNTON_DALE_STW', 'INLET_SCREEN_MACI_PUMP', Picture).

% grouped_screens_(Os,X) :- 
%     all_screens(Os,ScreenNames),
%     findall(X, all_same(ScreenNames).

% grouped_screens_('THORNTON_DALE_STW', Xs).
% grouped_screens(Os,Groups) :- 
%     all_screens(Os,ScreenNames), 
%     singleton(ScreenNames,)
%     % rts_picture('CHERRY_BURTON_STW_0', 'CHERRY_BURTON_STW', 'TELEMETRY_DATA_0_TO_31').
