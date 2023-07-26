%My Testing Trees from insert_left onwards for simplicity' sake: 
%node(1, node(8,node(6, empty, node(5, empty, empty)), empty), node(2,node(3,empty,empty),node(4, empty, empty)))
%node2(1, node2(8,node2(6, leaf(7), node2(5, leaf(8), leaf(9))), leaf(21)), node2(2,node2(3,leaf(34), leaf(24)),node2(4, leaf(77), leaf(43))))


%append
append([],Y,Y).
append([H|T],Y,[H|Z]) :- append(T,Y,Z).
   % ?- append([1,2], [3,4], [1,2,3,4]).   % mode: (+,+,+)   Solution: yes
   % ?- append([1,2], [3,4], Z).           % mode: (+,+,-)   Solution: Z = [1,2,3,4]
   % ?- append([1,2], Y, [1,2,3,4]).       % mode: (+,-,+)   Solution: Y = [3,4]
   % ?- append(X, [3,4], [1,2,3,4]).       % mode: (-,+,+)   Solution: X = [1,2]
   % ?- append(X, Y, [1,2]).               % mode: (-,-,+)   Solutions: X = [], Y = [1,2]; X = [1], Y = [2]; X = [1,2], Y = []

%%% Part 1 %%%

%% Merge Sort %% 

% deal 
deal([],([],[])).
deal( [X|Xs] , ([X|Ds], Ds2) ) :- deal(Xs, (Ds2, Ds)).


% ?- deal([1,2,3,4,5], (Ds,Ds2)) 
% mode (+,(-,-))
% Solution: (Ds = [1, 3, 5], Ds2 = [2, 4])

%merge
merge(Xs, [], Xs).
merge([], Ys, Ys).
merge([X|Xs], [Y|Ys],[X|Zs]) :- X =< Y, merge(Xs,[Y|Ys],Zs).
merge([X|Xs], [Y|Ys], [Y|Zs]) :- X > Y, merge([X|Xs],Ys,Zs).

% ?- merge([1,3,5],[2,4,6],Zs).
% mode(+,+,-)
% Solution: Zs = [1, 2, 3, 4, 5, 6]


% ms
ms([], []).
ms([X],[X]).
ms([X,Y|Zs], Ls) :- deal([X,Y|Zs],(Xs,Ys)),
    				ms(Xs, As),
    				ms(Ys, Bs),
    				merge(As,Bs,Ls).

% ?- ms([12,44,52,13,25,18,4,5,1], Ls)
% mode (+,-)
% Solution: Ls = [1, 4, 5, 12, 13, 18, 25, 44, 52]


%%Back List%%
%cons
cons(X, nil, snoc(nil,X)).
cons(X, snoc(P,Y), snoc(Ps,Y)) :- cons(X, P, Ps).

% ?- cons(2, snoc(snoc(snoc(snoc(nil,1),2),3),4), Ps).
% mode (+,+,-)
% Solution: Ps = snoc(snoc(snoc(snoc(snoc(nil,2),1),2),3),4)


%toBList
toBList([], nil).
toBList([X|Xs], Ys) :- toBList(Xs, Bs),
    				   cons(X, Bs, Ys).

% ?- toBList([2,3,4,5,6,7,8,9,10], N)
% mode (+,-)
% Solution: N = snoc(snoc(snoc(snoc(snoc(snoc(snoc(snoc(snoc(nil,2),3),4),5),6),7),8),9),10)

%snoc
%snoc(nil, Y, [Y]).
snoc(Xs, Y, Zs) :- append(Xs,[Y],Zs).

% ?- snoc([1,2,3,4,5], 0, Z).
% mode (+,+,-)
% Solution: Z = [1, 2, 3, 4, 5, 0]

%fromBList
fromBList([], nil).
fromBList([X|Xs], Ys) :- fromBList(Xs, Bs),
    					 snoc(Bs,X, Ys).

% ?- fromBList(snoc(snoc(snoc(snoc(snoc(snoc(snoc(snoc(snoc(nil,1),2),3),4),5),6),7),8),9), X).
% mode (+,-)
% Solution: X = [1, 2, 3, 4, 5, 6, 7, 8, 9]


%%binaryTree%%
%num_empties
num_empties(empty,1).
num_empties(node(_,A,empty),X):- num_empties(A, T3),
    							X is T3+1.
num_empties(node(_,empty,B),X):- num_empties(B, T4),
    							X is T4+1.                             
num_empties(node(_, A, B), X):- num_empties(A, T1),
    							num_empties(B, T2),
    							X is T1+T2.

% ?- num_empties(node(1, empty, node(2,node(3,empty,empty),node(4, empty, empty))), X).
% mode (+,-)
% Solution: X = 5

%num_nodes
num_nodes(empty,0).
num_nodes(node(_, A, B), X):- num_nodes( A, T1),
							  num_nodes(B, T2),
							  X is 1+T1+T2.

% ?- num_nodes(node(44, node(5,node(66, empty, node(6, empty, empty)), empty), node(8,node(5,empty,empty),empty)),N).
% mode (+,-)
% Solution: N = 6

%insert_left
insert_left(X, empty, node(X, empty, empty)).
insert_left(X, node(N, A, B), node(N,L,B)):- insert_left(X, A, L).

% ?- insert_left(4, node(1, node(8,node(6, empty, node(5, empty, empty)), empty), node(2,node(3,empty,empty),node(4, empty, empty))), N).
% mode (+,+,-)
% Solution: N = node(1,node(8,node(6,node(4,empty,empty),node(5,empty,empty)),empty),node(2,node(3,empty,empty),node(4,empty,empty)))
    
%insert_right
insert_right(X, empty, node(X, empty, empty)).
insert_right(X, node(N, A, B), node(N,A,R)):- insert_left(X, B, R).

% ?- insert_right(13,node(6, node(5,node(55, empty, node(67, empty, empty)), empty), node(8,node(4,empty,empty),empty)),N).
% mode (+,+,-)
% Solution: node(6,node(5,node(55,empty,node(67,empty,empty)),empty),node(8,node(4,node(13,empty,empty),empty),empty))
            
%sum_nodes
sum_nodes(empty, 0).
sum_nodes(node(X, A, B), Sum) :- sum_nodes(A,T),
    							 sum_nodes(B,S),
    						     Sum is X + T + S.

% ?- sum_nodes(node(1, node(8,node(6, empty, node(5, empty, empty)), empty), node(2,node(3,empty,empty),node(4, empty, empty))),Sum).
% mode (+,-)
% Solution: Sum = 29

%inorder
inorder(empty, []).
inorder(node(A,empty,empty), [A]).
inorder(node(X, L, R), Xs):- inorder(L,Ys),
    						 inorder(R,Zs),
    						 append(Ys, [X], As),
							 append(As,Zs,Xs).  

% ?- inorder(node(2, node(5,node(4, empty, node(55, empty, empty)), empty), node(8,node(3,empty,empty),empty)),N).
% mode (+,-)
% Solution: N = [4, 55, 5, 2, 3, 8]

%%leaf-based Tree%%
%num_elts
num_elts(leaf(_), 1).
num_elts(node2(_,L,R), X):- num_elts(L,T1),
    						num_elts(R,T2),
    						X is 1 + T1 + T2.

% ?- num_elts(node2(1, node2(8,node2(6, leaf(7), node2(5, leaf(8), leaf(9))), leaf(21)), node2(2,node2(3,leaf(34), leaf(24)),node2(4, leaf(77), leaf(43)))), X).
% mode (+,-)
% Solution: X = 15

%sum_nodes2
sum_nodes2(leaf(A),A).
sum_nodes2(node2(X,Y,Z),Sum) :- sum_nodes2(Y,B),
    	   						sum_nodes2(Z,C),
    							Sum is X+B+C.

% ?- sum_nodes2(node2(1, node2(8,node2(6, leaf(7), node2(5, leaf(8), leaf(9))), leaf(21)), node2(2,node2(3,leaf(34), leaf(24)),node2(4, leaf(77), leaf(43)))), X).
% mode (+,-)
% Solution: X = 252

%inorder2
inorder2(leaf(A), [A]).
inorder2(node2(X, L, R), Xs):- inorder2(L,Ys),
    						   inorder2(R,Zs),
    						   append(Ys, [X], As),
							   append(As,Zs,Xs).

% ?- inorder2(node2(1, node2(8,node2(6, leaf(7), node2(5, leaf(8), leaf(9))), leaf(21)), node2(2,node2(3,leaf(34), leaf(24)),node2(4, leaf(77), leaf(43)))), X).
% mode (+,-)
% Solution: X = [7, 6, 8, 5, 9, 8, 21, 1, 34, 3, 24, 2, 77, 4, 43]

%conv21
conv21(leaf(A), node(A, empty, empty)).
conv21(node2(X,L,R), node(X,L2,R2)):- conv21(L,L2),
    								  conv21(R,R2).

% ?- conv21(node2(1, node2(8,node2(6, leaf(7), node2(5, leaf(8), leaf(9))), leaf(21)), node2(2,node2(3,leaf(34), leaf(24)),node2(4, leaf(77), leaf(43)))), X).
% mode (+,-)
% Solution: X = node(1,node(8,node(6,node(7,empty,empty),node(5,node(8,empty,empty),node(9,empty,empty))),node(21,empty,empty)),node(2,node(3,node(34,empty,empty),node(24,empty,empty)),node(4,node(77,empty,empty),node(43,empty,empty))))

%%% Part 2 %%%
%%BList'%%

%toBList_it
toBList_it(Xs, Ys) :- toBList_help(Xs, nil, Ys).
toBList_help([],Xs,Xs).
toBList_help([X],A,snoc(A,X)).
toBList_help(Xs,A,snoc(R,S)) :- last(Xs, S),
        					    append(X, [S], Xs),
        					    toBList_help(X, A, R).

% ?- toBList_it([1,2,3,4,5,6,7,8,9], Xs).
% mode (+,-)
% Solution: Xs = snoc(snoc(snoc(snoc(snoc(snoc(snoc(snoc(snoc(nil,1),2),3),4),5),6),7),8),9)

%fromBList_it
fromBList_it(Xs,Ys):- fromBList_help(Xs, [], Ys).
				      fromBList_help(nil,Xs,Xs).
				      fromBList_help(snoc(R, S), Xs, Ys):- fromBList_help(R, Xs, Ts),
    													   append(Ts, [S], Ys).
% ?- fromBList_it(snoc(snoc(snoc(snoc(snoc(snoc(snoc(snoc(snoc(nil,1),2),3),4),5),6),7),8),9), X).
% mode (+,-)
% Solution: X = [1, 2, 3, 4, 5, 6, 7, 8, 9]

%sum_nodes_it
sumNodes_it(T, N) :- sum_help([T], 0, N).
					 sum_help([], A, A).
					 sum_help([empty|Ts], A, N) :- sum_help(Ts, A, N).
	                 sum_help([node(E,L,R)|Ts], A, N) :- AE is A + E, sum_help([L,R|Ts], AE, N).

% ?- sumNodes_it(node(66, node(56,node(39, empty, node(2, empty, empty)), empty), node(8,node(94,empty,empty),empty)),N)
% mode (+,-)
% Solution: N = 265

%numEmpties_it
numEmpties_it(Ts, E):- numEmpt_help([Ts],0,E).
					   numEmpt_help([],E,E).
					   numEmpt_help([empty|Ts], A, E):- R is A+1, numEmpt_help(Ts,R,E).
					   numEmpt_help([node(_,L,R)|Ts], A, E):- numEmpt_help([L,R|Ts],A,E).

% ?- numEmpties_it(node(1,node(8,node(6,node(7,empty,empty),node(5,node(8,empty,empty),node(9,empty,empty))),node(21,empty,empty)),node(2,node(3,node(34,empty,empty),node(24,empty,empty)),node(4,node(77,empty,empty),node(43,empty,empty)))),X).
% mode (+,-)
% Solution: X = 16

%num_nodes_it
num_nodes_it(Ts, E):- numNode_help([Ts],0,E).
					  numNode_help([],E,E).
					  numNode_help([empty|Ts], A, E):- numNode_help(Ts, A, E).
    				  numNode_help([node(_,L,R)|Ts], A, E):- Num is A+1,
    														 numNode_help([L,R|Ts],Num,E).	

% mode(+,-)
% ?- num_nodes_it(node(1,node(4,empty,node(5,empty,empty)),node(2,empty,node(3,empty,empty))), N).
% Solution: N=5

%sum_nodes2_it
sum_nodes2_it(Ts, E):- sumNodes2_help([Ts],0,E).
					   sumNodes2_help([],E,E).
					   sumNodes2_help([leaf(Le)|Ts], A, E):- AE is A+Le, sumNodes2_help(Ts,AE,E).
					   sumNodes2_help([node2(Le,L,R)|Ts], A, E):- AE is A+Le, sumNodes2_help([L,R|Ts],AE,E).
% mode(+,-)
% ?- sum_nodes2_it(node2(1, node2(2,leaf(4),leaf(28)), leaf(2)), N).
% N=37

%inOrder2_it
inOrder2_it(T, Opr) :- inOrder2_it_help([T], [], Opr).
inOrder2_it_help([], Opr, Opr).
inOrder2_it_help([leaf(V) | Ts], A, Opr) :- append([V], A, List), inOrder2_it_help(Ts, List, Opr).
inOrder2_it_help([node2(V, L, R) | Ts], A, Opr) :- append([R], [leaf(V)], L1), append(L1, [L], R1), append(R1, Ts, Temp), inOrder2_it_help(Temp, A, Opr).

% ?- inOrder2_it(node2(6, node2(7,node2(8, leaf(9), node2(10, leaf(1), leaf(12))), leaf(13)), node2(13,node2(15,leaf(16), leaf(22)),node2(24, leaf(53), leaf(20)))), X).
% mode (+,-)
% Solution: X = [9, 8, 1, 10, 12, 7, 13, 6, 16, 15, 22, 13, 53, 24, 20]

%%% Part 4 %%%%
%%Tree to Tree2%%

%conv12
conv12(node(T,empty,empty),leaf(T)).
conv12(node(T, L, R), node2(T, L2, R2)):- conv12(L,L2),
    									  conv12(R,R2).

% ?- conv12(node(1,node(8,node(6,node(7,empty,empty),node(5,node(8,empty,empty),node(9,empty,empty))),node(21,empty,empty)),node(2,node(3,node(34,empty,empty),node(24,empty,empty)),node(4,node(77,empty,empty),node(43,empty,empty)))),X).
% mode (+,-)
% Solution: X = node2(1,node2(8,node2(6,leaf(7),node2(5,leaf(8),leaf(9))),leaf(21)),node2(2,node2(3,leaf(34),leaf(24)),node2(4,leaf(77),leaf(43))))

%%BST BST2%%
bst(empty,true).
bst(node(_,empty,empty),true).
bst(node(_,L,R),Opr) :- bst_it(L,Tree,SubOpr1), bst_it(R,Tree,SubOpr2), SubOpr1 == "LT", SubOpr2 == "GT",Opr = true, left_check(L,Tree,Opr), right_check(R,Tree,Opr), bst(L,Opr), bst(R,Opr).
bst(_,false).

%mode(+,-)
% ?- bst(node(1,empty,(9,empty,empty)),P)
% Solution -: P = false

% ?- bst(empty,P)
% Solution -: P = true

bst_it(empty,_,"GT").
bst_it(node(Tree,_,_),X,Opr) :-    Tree>X    ->  Opr = "GT".
bst_it(node(Tree,_,_),X,Opr) :- Tree==X ->  Opr = "EQ".
bst_it(node(Tree,_,_),X,Opr) :-    Tree<X ->  Opr = "LT".
% mode (+,+,-)
% ?- bst_it(node(1,node(8,node(6,node(7,empty,empty),node(5,node(8,empty,empty),node(9,empty,empty))),node(21,empty,empty)),node(2,node(3,node(34,empty,empty),node(24,empty,empty)),node(4,node(77,empty,empty),node(43,empty,empty)))), 0, X2).
% Solution: X2 = "GT"
% ?- bst_it(node(1,node(8,node(6,node(7,empty,empty),node(5,node(8,empty,empty),node(9,empty,empty))),node(21,empty,empty)),node(2,node(3,node(34,empty,empty),node(24,empty,empty)),node(4,node(77,empty,empty),node(43,empty,empty)))), 1, X2).
% Solution: X2 = "EQ"
% ?- bst_it(node(1,node(8,node(6,node(7,empty,empty),node(5,node(8,empty,empty),node(9,empty,empty))),node(21,empty,empty)),node(2,node(3,node(34,empty,empty),node(24,empty,empty)),node(4,node(77,empty,empty),node(43,empty,empty)))), 2, X2).
% Solution: X2 = "LT"

left_check(empty,_,true).
left_check(node(Tree,empty,empty),A,Opr) :- Tree<A, Opr=true.
left_check(node(Tree,L,empty),A,Opr) :- Tree<A, Opr=true,left_check(L,A,Opr).
left_check(node(Tree,empty,R),A,Opr) :- Tree<A, Opr=true,left_check(R,A,Opr).
left_check(node(Tree,L,R),A,Opr) :- Tree<A, Opr=true,left_check(L,A,Opr),left_check(R,A,Opr),!.
left_check(_,_,false).

% mode (+,+,-)
% ?- left_check(node(1,node(8,node(6,node(7,empty,empty),node(5,node(8,empty,empty),node(9,empty,empty))),node(21,empty,empty)),node(2,node(3,node(34,empty,empty),node(24,empty,empty)),node(4,node(77,empty,empty),node(43,empty,empty)))),0, X).
% Solution: X = false
% ?-left_check(node(1,node(8,node(6,node(7,empty,empty),node(5,node(8,empty,empty),node(9,empty,empty))),node(21,empty,empty)),node(2,node(3,node(34,empty,empty),node(24,empty,empty)),node(4,node(77,empty,empty),node(43,empty,empty)))),100, X).
% Solution: X = True
right_check(empty,_,true).
right_check(node(Tree,empty,empty),A,Opr) :- Tree>A, Opr=true.
right_check(node(Tree,L,empty),A,Opr) :- Tree>A, Opr=true,right_check(L,A,Opr).
right_check(node(Tree,empty,R),A,Opr) :- Tree>A, Opr=true,right_check(R,A,Opr).
right_check(node(Tree,L,R),A,Opr) :- Tree>A, Opr=true,right_check(L,A,Opr),right_check(R,A,Opr),!.
right_check(_,_,false).
% mode: (+,+,-)
%?-right_check(node(1,node(8,node(6,node(7,empty,empty),node(5,node(8,empty,empty),node(9,empty,empty))),node(21,empty,empty)),node(2,node(3,node(34,empty,empty),node(24,empty,empty)),node(4,node(77,empty,empty),node(43,empty,empty)))), 0, X2).
%Solution:X2 = true
%?-right_check(node(1,node(8,node(6,node(7,empty,empty),node(5,node(8,empty,empty),node(9,empty,empty))),node(21,empty,empty)),node(2,node(3,node(34,empty,empty),node(24,empty,empty)),node(4,node(77,empty,empty),node(43,empty,empty)))), 4, X2).
%Solution:X2 = false

bst2(leaf(_),true).
bst2(node2(Tree,L,R),Opr) :- bst2_it(L,Tree,Opr1),bst2_it(R,Tree,Opr2), Opr1 == "LT", Opr2 == "GT", Opr=true, left_check2(L,Tree,Opr),right_check2(R,Tree,Opr),bst2(L,Opr),bst2(R,Opr);
    Opr=false.
% mode (+,-)
% ?- bst2((node2(8, node2(66,node2(6, leaf(7), node2(5, leaf(8), leaf(9))), leaf(21)), node2(2,node2(3,leaf(34), leaf(24)),node2(4, leaf(77), leaf(43))))), X).
% Solution: False
% ?-bst2(leaf(3), X).
% Solution: True

bst2_it(leaf(Tree),A,Opr) :- Tree>A    ->  Opr = "GT".
bst2_it(leaf(Tree),A,Opr) :- Tree==A ->  Opr = "EQ".
bst2_it(leaf(Tree),A,Opr) :- Tree<A    ->  Opr = "LT".
bst2_it(node2(Tree,_,_),A,Opr) :- Tree>A    ->  Opr = "GT".
bst2_it(node2(Tree,_,_),A,Opr) :- Tree==A ->  Opr = "EQ".
bst2_it(node2(Tree,_,_),A,Opr) :- Tree<A    ->  Opr = "LT".

% mode (+,+,-)
% ?- bst2_it(node2(8, node2(66,node2(6, leaf(7), node2(5, leaf(8), leaf(9))), leaf(21)), node2(2,node2(3,leaf(34), leaf(24)),node2(4, leaf(77), leaf(43)))),8,X).
% Solution: X = "EQ"
% ?-bst2_it(node2(8, node2(66,node2(6, leaf(7), node2(5, leaf(8), leaf(9))), leaf(21)), node2(2,node2(3,leaf(34), leaf(24)),node2(4, leaf(77), leaf(43)))),9,X).
% Solution: X = "LT"
% ?-bst2_it(node2(8, node2(66,node2(6, leaf(7), node2(5, leaf(8), leaf(9))), leaf(21)), node2(2,node2(3,leaf(34), leaf(24)),node2(4, leaf(77), leaf(43)))),3,X).
% Solution: X = "GT"

left_check2(leaf(Tree),A,Opr) :- Tree<A,Opr=true.
left_check2(node2(Tree,L,R),A,Opr) :- Tree<A,Opr=true,left_check2(L,A,Opr),left_check2(R,A,Opr);
    Opr=false.
% mode (+,+,-)

% ?- left_check2((node2(8, node2(66,node2(6, leaf(7), node2(5, leaf(8), leaf(9))), leaf(21)), node2(2,node2(3,leaf(34), leaf(24)),node2(4, leaf(77), leaf(43))))), 0, X).
% Solution: X = false

% ?- left_check2((node2(8, node2(66,node2(6, leaf(7), node2(5, leaf(8), leaf(9))), leaf(21)), node2(2,node2(3,leaf(34), leaf(24)),node2(4, leaf(77), leaf(43))))), 100, X).
% Solution: X = true


right_check2(leaf(Tree),A,Opr) :- Tree>A,Opr=true.
right_check2(node2(Tree,L,R),A,Opr) :- Tree>A,Opr=true,right_check2(L,A,Opr),right_check2(R,A,Opr);
    Opr=false.
% mode (,)
% ?-right_check2(node2(8, node2(66,node2(6, leaf(7), node2(5, leaf(8), leaf(9))), leaf(21)), node2(2,node2(3,leaf(34), leaf(24)),node2(4, leaf(77), leaf(43)))), 8, X).
% Solution: X = false
% ?-right_check2(node2(8, node2(66,node2(6, leaf(7), node2(5, leaf(8), leaf(9))), leaf(21)), node2(2,node2(3,leaf(34), leaf(24)),node2(4, leaf(77), leaf(43)))), 0, X).
% Solution: X = true