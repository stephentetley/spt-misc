/* 
    demo.lgt
*/

:- use_module(library(http/json)).

:- object(demo).

    :- public(test01/1).
    test01(Ans) :- 
        read_rose_tree("data/rose_tree1.json", Ans).

    read_rose_tree(Src, Ans):-
        open(Src, read, Stream),
        json::json_read_dict(Stream, Dict),
        close(Stream), 
        decode_rose_tree(Dict, Ans).

    decode_rose_tree(Dict, Ans) :-
        Label = Dict.node,
        List = Dict.kids,
        meta::map([X,Y]>> decode_rose_tree(X,Y), List, Kids),
        Ans = node(Label, Kids).

    encode_rose_tree(node(A,Kids), Ans) :- 
        meta::map([X,Y] >> encode_rose_tree(X,Y), Kids, Kids1),
        Ans = _{ node:A, kids:Kids1}.

    write_rose_tree(Dst, Tree) :- 
        encode_rose_tree(Tree, Dict),
        open(Dst, write, Stream),
        json::json_write_dict(Stream, Dict), 
        close(Stream).


    :- public(test02/0).
    test02 :- 
        read_rose_tree("data/rose_tree1.json", Tree),
        write_rose_tree("data/rose_tree1.output.json", Tree).



:- end_object.