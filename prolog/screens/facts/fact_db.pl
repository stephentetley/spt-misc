% fact_db.pl

% For the time being, this is a useful way of decoupling ultrasonics.pl 
% from the generated facts database.

:- module(fact_db, 
            [ screen_type/2
            ]).
        
:- reexport([ facts/screens
            , facts/installations ]).


% TODO - should we define accessors for facts with large arities?

screen_type(screen(_, _, _, _, _, Type, _, _), Type).
