/*
    structs.pl
    Copyright (c) Stephen Tetley 2019
    License: BSD 3 Clause
*/   

:- module(structs, 
            [ is_rose_tree/1
            , rose_tree_data/3
            , cons_rose_tree/3
            , make_rose_tree/2
            , rose_tree_label/2
            , rose_tree_kids/2
            ]).

:- use_module(library(record)).

:- record rose_tree(label:atomic, kids:list=[]).


cons_rose_tree(Label,Kids,Ans) :- 
    make_rose_tree([label(Label), kids(Kids)], Ans).

