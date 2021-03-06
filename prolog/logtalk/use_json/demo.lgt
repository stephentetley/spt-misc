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
        meta::map([X,Y]>> decode_rose_tree(X,Y), Dict.kids, Kids),
        Ans = node(Dict.node, Kids).

    encode_rose_tree(node(A,Xs), Ans) :- 
        meta::map([X,Y] >> encode_rose_tree(X,Y), Xs, Kids),
        Ans = _{ node:A, kids:Kids}.

    write_rose_tree(Dst, Tree) :- 
        encode_rose_tree(Tree, Dict),
        open(Dst, write, Stream),
        json::json_write_dict(Stream, Dict), 
        close(Stream).


    :- public(test02/0).
    test02 :- 
        read_rose_tree("data/rose_tree1.json", Tree),
        write_rose_tree("data/rose_tree1.output.json", Tree).

    % BinTree
    read_bin_tree(Src, Ans):-
        open(Src, read, Stream),
        json::json_read_dict(Stream, Dict),
        close(Stream), 
        decode_bin_tree(Dict, Ans).

    decode_bin_tree(null, null).        
    decode_bin_tree(Dict, Ans) :-
        decode_bin_tree(Dict.left, Left),
        decode_bin_tree(Dict.right, Right),
        !,
        Ans = bin(Dict.label, Left, Right).

    :- public(test03/1).
    test03(Ans) :- 
        read_bin_tree("data/bin_tree1.json", Ans).

    encode_bin_tree(null, null). 
    encode_bin_tree(bin(A, Left, Right), Ans) :- 
        encode_bin_tree(Left, LeftDict),
        encode_bin_tree(Right, RightDict), 
        Ans = _{label:A, left:LeftDict, right:RightDict}.
    
    write_bin_tree(Dst, Tree) :- 
        encode_bin_tree(Tree, Dict),
        open(Dst, write, Stream),
        json::json_write_dict(Stream, Dict), 
        close(Stream).

    :- public(test04/0).
    test04 :- 
        read_bin_tree("data/bin_tree1.json", Tree),
        write_bin_tree("data/bin_tree1.output.json", Tree).


    % Note cannot use meta functions at the top level.
    % identity([X, Y] >> Y = X).

    :- public(identity/2).
    identity(X, Y) :- Y = X.

    :- public(test05/1).
    test05(Ans) :- 
        identity(3,Ans).

    :- meta_predicate(encode_bin_tree_cps(*, 2, 2)).
        
    encode_bin_tree_cps(null, Cont, Ans) :- 
        call(Cont, null, Ans).

    encode_bin_tree_cps(bin(A, Left, Right), Cont, Ans) :- 
        encode_bin_tree_cps(Left, 
                            ({A,Cont,Right,V2,Ans2}/[V1,Ans1] >> 
                                (encode_bin_tree_cps(Right,
                                                     ({A,Cont,V1}/[V2,Ans2] >> 
                                                            (call(Cont, dict{label:A, left:V1, right:V2}, Ans2))), 
                                                     Ans1))),
                            Ans).

    
    write_bin_tree_cps(Dst, Tree) :- 
        encode_bin_tree_cps(Tree, identity, Dict),
        open(Dst, write, Stream),
        json::json_write_dict(Stream, Dict), 
        close(Stream).

    :- public(test06/0).
    test06 :- 
        read_bin_tree_cps("data/bin_tree1.json", Tree),
        write_bin_tree_cps("data/bin_tree1.cps.json", Tree).

    :- public(frees/3).
    frees(A, B, Ans) :- 
        call(({A,B}/[Y] >> (Y is A + B)), Ans).

    :- public(test07/1).
    test07(Ans) :-
        frees(1,2, Ans).

    :- meta_predicate(decode_bin_tree_cps(*, 2, 2)).

    decode_bin_tree_cps(null, Cont, Ans) :- 
        call(Cont, null, Ans).

    decode_bin_tree_cps(Dict, Cont, Ans) :-
        decode_bin_tree_cps(Dict.left,  
                            ({A,Cont,Dict,V2,Ans2}/[V1,Ans1] >> 
                                 (decode_bin_tree_cps(Dict.right,
                                                     ({A,Cont,Dict,V1}/[V2,Ans2] >> 
                                                        (call(Cont, bin(Dict.label,V1,V2), Ans2))), 
                                                      Ans1))),
                            Ans), !.

               
    read_bin_tree_cps(Src, Ans):-
            open(Src, read, Stream),
            json::json_read_dict(Stream, Dict),
            close(Stream), 
            decode_bin_tree_cps(Dict, identity, Ans).

    :- public(test08/1).
    test08(Ans) :- 
        read_bin_tree_cps("data/bin_tree1.json", Ans).

    :- meta_predicate(decode_bin_tree_cont(*, 2, *)).
    decode_bin_tree_cont(null, Cont, Ans) :- 
        call(Cont, null, Ans).
        
    decode_bin_tree_cont(Dict, Cont, Ans) :-
        decode_bin_tree_cont(Dict.left,  identity, V1),
        decode_bin_tree_cont(Dict.right, identity, V2),
        !,
        call(Cont, bin(Dict.label, V1, V2), Ans).

               
    read_bin_tree_cont(Src, Ans):-
        open(Src, read, Stream),
        json::json_read_dict(Stream, Dict),
        close(Stream), 
        decode_bin_tree_cont(Dict, identity, Ans).
    
    :- public(test09/1).
    test09(Ans) :- 
        read_bin_tree_cps("data/bin_tree1.json", Ans).

    :- meta_predicate(encode_bin_tree_cont(*, 2, *)).
    encode_bin_tree_cont(null, Cont, Ans) :- 
        call(Cont, null, Ans). 

    encode_bin_tree_cont(bin(A, Left, Right), Cont, Ans) :- 
        encode_bin_tree_cont(Left, identity, V1),
        encode_bin_tree_cont(Right, identity, V2), 
        call(Cont, _{label:A, left:V1, right:V2}, Ans).

    write_bin_tree_cont(Dst, Tree) :- 
        encode_bin_tree_cont(Tree, identity, Dict),
        open(Dst, write, Stream),
        json::json_write_dict(Stream, Dict), 
        close(Stream).

    :- public(test10/0).
    test10 :- 
        read_bin_tree_cont("data/bin_tree1.json", Tree),
        write_bin_tree_cont("data/bin_tree1.cont.json", Tree).

:- end_object.