/*
    osgb36.pl
    Copyright (c) Stephen Tetley 2019
    License: BSD 3 Clause
*/   

:- use_module(osgb36).

lands_end('SW3417725339').
john_o_groats('ND3828472636').

% E = 134177
% N = 025339
demo01(E,N) :- 
    lands_end(Gridref),
    gridref_geometry(Gridref, E, N).

demo01b(OSGB) :- 
    geometry_gridref(134177.0, 025339.0, OSGB).

% E = 338284
% N = 972636
demo02(E,N) :- 
    john_o_groats(Gridref),
    gridref_geometry(Gridref, E, N).


