% utils.pl


:- module(utils,
          [all_same/2]).


% singleton is a bad name, all_same is a bit better.

% Don't define the empty case then Prolog will throw an error.
% all_same([]).
all_same([H|List], X) :- all_same_(List, H), X = H.

all_same_([], _A).
all_same_([X|List], A) :- X = A, all_same_(List, A).

% Note - sort/2 finds distict elements in a list (and orders them...)

