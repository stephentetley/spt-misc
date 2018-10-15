% sensors.pl

:- module(classification,
            [ screen_category/2
            ]).




screen_category('BAR', '1D').
screen_category('BAND', '2D').
screen_category('CUP', '2D').
screen_category('DRUM', '2D').
screen_category('MESH', '2D').
screen_category('ROTARY', '2D').
screen_category(_, 'UNKNOWN').



