% common.pl

:- module(common,
          [ output_csv/3
          , nth0_cell/3
          , nth1_cell/3
          ]).

:- use_module(library(csv)).
:- use_module(library(lists)).

output_csv(File, Headers, Rows) :-
    setup_call_cleanup(
        open(File, write, Out),
            ( csv_write_stream(Out, [Headers], []),
              csv_write_stream(Out, Rows, [])
            ),
        close(Out)).

% Use atomics_to_string to pretty print a list


nth0_cell(Index, Row, Elem) :- 
    integer(Index),
    % ignore the first element which is always the symbol 'row'.
    Index0 is Index + 1,
    Row =.. Cells,
    nth0(Index0, Cells, Elem).


nth1_cell(Index, Row, Elem) :- 
    integer(Index),
    % ignore the first element which is always the symbol 'row'.
    Index0 is Index + 1,
    Row =.. Cells,
    nth1(Index0, Cells, Elem).

