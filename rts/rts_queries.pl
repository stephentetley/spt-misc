/* 
Copyright (c) Stephen Tetley 2018
License: BSD 3 Clause
*/


:- use_module(facts/rts_mimic_names).
:- use_module(facts/rts_mimic_points).
:- use_module(facts/rts_asset_to_signal).
:- use_module(facts/rts_pump_facts).
:- use_module(facts/rts_screen_facts).
:- use_module(utils).

/*
 * Comment
*/



mimic_long_name(OS, Point, Name) :- 
    rts_mimic_point(P, OS, Point),
    rts_mimic_name(P,Name).

% picture_long_name( 'CHERRY_BURTON_STW', 'RBC_NO1_DRIVE_P', X).

all_mimics(Os, Pictures) :- 
    findall(X, rts_mimic_point(X, Os, _), Xs),
    sort(Xs,Pictures).


all_mimic_names(Mimics) :-
    findall(Y, rts_mimic_name(_,Y), Xs),
    sort(Xs,Mimics).



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



% Pumps

all_pumps(Os, Pumps) :- 
    findall(X, rts_pump(Os,X,_), Xs), 
    sort(Xs,Pumps).


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
all_screens(Os, Screens) :- 
    findall(S, rts_screen(Os, S, _), Screens1),
    sort(Screens1, Screens).

% all_screens('THORNTON_DALE_STW', Xs).

has_standard_screen_signals(Os, Point) :-   
    rts_screen(Os,Point,Sigs),
    has_standard_signals_(Sigs).

% pumps with standard signals
% Os must be ground.
screens_with_standard_signals(Os, Screens) :- 
    all_screens(Os, Names),
    include(has_standard_screen_signals(Os), Names, Screens).

% Os must be ground.
screens_with_nonstandard_signals(Os, Screens) :- 
    all_screens(Os, Names),
    exclude(has_standard_screen_signals(Os), Names, Screens).

% screens_with_nonstandard_signals('THORNTON_DALE_STW', Ys).

asset_points(Os, Asset, Points) :-
    findall(X, asset_to_signal(Os, Asset, X, _), Xs),
    sort(Xs,Points).

% screen_points('THORNTON_DALE_STW', 'INLET_SCREEN_MACI_PUMP', Points).

get_mimic_(Os, Point, Mimic) :- 
    rts_mimic_point(Mimic, Os, Point).

asset_to_mimic_(Os, Screen, Mimic) :- 
    asset_points(Os, Screen, Points),
    convlist(get_mimic_(Os), Points, Mimics),
    all_same(Mimics, Mimic).

mimic_to_asset_(Os, Mimic, Screen) :- 
    asset_to_mimic_(Os, Screen, Mimic).

% screen_to_mimic('THORNTON_DALE_STW', 'INLET_SCREEN_MACI_PUMP', Mimic).

join_mimic_to_screens_(Os, Mimic, Screens) :- 
    all_screens(Os, Screens1),
    include(mimic_to_asset_(Os, Mimic), Screens1, Screens).

% join_mimic_to_screens_('THORNTON_DALE_STW', 'THORNTON_DALE_STW_1', X).

screen_signals(Os, Results) :- 
    all_mimics(Os,Mimics),
    convlist([Mimic,Ans] >> (join_mimic_to_screens_(Os, Mimic, Screens),
                            rts_mimic_name(Mimic, Name),  
                            Ans = (Name,Screens)), 
                    Mimics, Results1),
    include(tuple_with_points_, Results1, Results).


% screen_signals('THORNTON_DALE_STW', Xs).

join_mimic_to_pumps_(Os, Mimic, Pumps) :- 
    all_pumps(Os, Pumps1),
    include(mimic_to_asset_(Os, Mimic), Pumps1, Pumps).

pump_signals(Os, Results) :- 
    all_mimics(Os,Mimics),
    convlist([Mimic,Ans] >> (join_mimic_to_pumps_(Os, Mimic, Pumps), 
                            rts_mimic_name(Mimic, Name), 
                            Ans = (Name, Pumps)), 
                Mimics, Results1),
    include(tuple_with_points_, Results1, Results).

tuple_with_points_((_,[_|_])) :- true.


