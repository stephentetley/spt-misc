% fact_db.pl

% For the time being, this is a useful way of decoupling ultrasonics.pl 
% from the generated facts database.

:- module(fact_db, []).
:- reexport([ facts/sensors
            , facts/relays
            , facts/level_monitors
            ]).
