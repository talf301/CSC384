/* ----------------------------------------------------------
    CSC384 Assignment 2 

% Surname: Friedman
% First Namer: Tal
% Student Number: 999875899

  ------------------------------------------------------ */

%do not chagne the follwoing line!
:- ensure_loaded('play.pl').

% /* ------------------------------------------------------ */
%               IMPORTANT! PLEASE READ THIS SUMMARY:
%       This files gives you some useful helpers (set &get).
%       Your job is to implement several predicates using
%       these helpers (feel free to add your own helpers if needed,
%       MAKE SURE to write comments for all your helpers, marks will
%       be deducted for bad style!).
%
%       Implement the following predicates at their designated space
%       in this file (we suggest to have a look at file ttt.pl  to
%       see how the implementations is done for game tic-tac-toe.
%
%          * initialize(InitialState,InitialPlyr).
%          * winner(State,Plyr) 
%          * tie(State)
%          * terminal(State) 
%          * moves(Plyr,State,MvList)
%          * nextState(Plyr,Move,State,NewState,NextPlyr)
%          * validmove(Plyr,State,Proposed)
%          * h(State,Val)  (see question 2 in the handout)
%          * lowerBound(B)
%          * upperBound(B)
% /* ------------------------------------------------------ */







% /* ------------------------------------------------------ */

% We use the following State Representation: 
% [Row0, Row1 ... Rown] (ours is 6x6 so n = 5 ).
% each Rowi is a LIST of 6 elements '.' or '1' or '2' as follows: 
%    . means the position is  empty
%    1 means player one has a piece in this position
%    2 means player two has a piece in this position. 



% given helper: Inital state of the board 
initBoard([ [.,.,.,.,.,.], 
            [.,.,.,.,.,.],
	    [.,.,1,2,.,.], 
	    [.,.,2,1,.,.], 
            [.,.,.,.,.,.], 
	    [.,.,.,.,.,.] ]).

testBoard([ [2,2,2,2,2,1],
[1,1,1,1,1,1],
[1,1,1,2,2,1],
[.,1,2,2,2,1],
[.,1,1,1,1,1],
[.,.,.,.,.,.]]).

%%%%%%%%%%%%%%%%%% IMPLEMENT: initialize(...)%%%%%%%%%%%%%%%%%%%%%
%%% Using initBoard define initialize(InitialState,InitialPlyr). 
%%%  holds iff InitialState is the initial state and 
%%%  InitialPlyr is the player who moves first. 
initialize(InitialState,1) :- initBoard(InitialState).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%winner(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define winner(State,Plyr) here.  
%     - returns winning player if State is a terminal position and
%     Plyr has a higher score than the other player 
winner(State,1) :- terminal(State),	count(1,State,C1), 
				   count(2,State,C2), C1 > C2.

winner(State,2) :- terminal(State), count(1,State,C1),
				   count(2,State,C2), C2 > C1.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%tie(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define tie(State) here. 
%    - true if terminal State is a "tie" (no winner) 
tie(State) :- terminal(State), count(1,State,C1), count(2,State,C2),
			  C1 =:= C2.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%terminal(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define terminal(State). 
%   - true if State is a terminal   
terminal(State) :- moves(1,State,[]), moves(2,State,[]).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%showState(State)%%%%%%%%%%%%%%%%%%%%%%%%%%
%% given helper. DO NOT  change this. It's used by play.pl
%% 
showState( G ) :- 
	printRows( G ). 
 
printRows( [] ). 
printRows( [H|L] ) :- 
	printList(H),
	nl,
	printRows(L). 

printList([]).
printList([H | L]) :-
	write(H),
	write(' '),
	printList(L).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%moves(Plyr,State,MvList)%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define moves(Plyr,State,MvList). 
%   - returns list MvList of all legal moves Plyr can make in State
%
moves(Plyr,State,MvList) :- moveshelp(Plyr,State,[],MvList,35).

%% moveshelp(Plyr,State, SoFar, MvList, Mv).
%  helper to recursively find all moves
moveshelp(_,_,MvList,MvList,-1).
moveshelp(Plyr,State,SoFar,MvList,Mv) :- Mv > -1, X is Mv // 6, Y is Mv mod 6, Nm is Mv - 1,
										 validmove(Plyr, State, [X,Y]), !,
										 moveshelp(Plyr,State,[[X,Y]|SoFar],MvList,Nm).
moveshelp(Plyr,State,SoFar,MvList,Mv) :- Mv > -1, Nm is Mv - 1, 
										 moveshelp(Plyr,State,SoFar,MvList,Nm).

%%%%%%%%%%%%%%nextState(Plyr,Move,State,NewState,NextPlyr)%%%%%%%%%%%%%%%%%%%%
%% 
%% define nextState(Plyr,Move,State,NewState,NextPlyr). 
%   - given that Plyr makes Move in State, it determines NewState (i.e. the next 
%     state) and NextPlayer (i.e. the next player who will move).
%
nextState(Plyr,[R,C],State,NewState,NextPlyr) :- validmove(Plyr,State,[R,C]),
												otherPlyr(Plyr,NextPlyr),
												flip(Plyr,[R,C],State,St1,-1,-1),
												flip(Plyr,[R,C],St1,St2,0,-1),
												flip(Plyr,[R,C],St2,St3,1,-1),
												flip(Plyr,[R,C],St3,St4,-1,0),
												flip(Plyr,[R,C],St4,St5,1,0),
												flip(Plyr,[R,C],St5,St6,-1,1),
												flip(Plyr,[R,C],St6,St7,0,1),
												flip(Plyr,[R,C],St7,St8,1,1),
												set(St8,NewState,[R,C],Plyr).

nextState(Plyr,n,State,State,NextPlyr) :- otherPlyr(Plyr,NextPlyr).
												
%% flip(Plyr,[R,C],State,NewState,Ri,Ci) 
%  checks if given direction is valid and if so preforms the necessary flips
%  in direction
flip(Plyr,[R,C],State,NewState,Ri,Ci) :- valid1Dir(Plyr, State, [R,C], Ri, Ci), !,
										 Rn is R + Ri, Cn is C + Ci, 
										 rflip(Plyr,[Rn,Cn],State,NewState,Ri,Ci).
flip(_,_,State,State,_,_).	 

rflip(Plyr,[R,C],State,NewState,Ri,Ci) :- otherPlyr(Plyr,Plyr2), get(State,[R,C],Plyr2),
										  set(State,IntState,[R,C],Plyr),Rn is R + Ri,
										  Cn is C + Ci, rflip(Plyr,[Rn,Cn],IntState,NewState,Ri,Ci).

rflip(Plyr,[R,C],State,State,_,_) :- get(State,[R,C],Plyr).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%validmove(Plyr,State,Proposed)%%%%%%%%%%%%%%%%%%%%
%% 
%% define validmove(Plyr,State,Proposed). 
%   - true if Proposed move by Plyr is valid at State.
%  two cases: either a pass or a move, check this seperately
validmove(Plyr,State,[R,C]) :- get(State, [R,C], .), validAnyDir(Plyr, State, [R,C]).
validmove(Plyr,State,n) :- moves(Plyr,State,[]).
 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%h(State,Val)%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define h(State,Val). 
%   - given State, returns heuristic Val of that state
%   - larger values are good for Max, smaller values are good for Min
%   NOTE1. If State is terminal h should return its true value.
%   NOTE2. If State is not terminal h should be an estimate of
%          the value of state (see handout on ideas about
%          good heuristics.
h(State,0) :- \+ terminal(State).
h(State,0) :- tie(State).
h(State,100) :- winner(State,2).
h(State,-100) :- winner(State,1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%lowerBound(B)%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define lowerBound(B).  
%   - returns a value B that is less than the actual or heuristic value
%     of all states.
lowerBound(-101).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%upperBound(B)%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define upperBound(B). 
%   - returns a value B that is greater than the actual or heuristic value
%     of all states.
upperBound(101).

%%%%%%% Global Helpers
%% onBoard(R,C) specifies if a tile at [R,C] is on the board
onBoard(R,C) :- R < 6, R >= 0, C < 6, C >= 0.

%% otherPlyr(+Plyr, -Plyr2) returns the other player's number, i.e. 1 -> 2, 2 -> 1
otherPlyr(Plyr, Plyr2) :- Plyr2 is (Plyr mod 2) + 1.


%% validAnyDir(Plyr, State, [R,C]_
%  true if a move at [R,C] is valid for Plyr based on any direction
%  our strategy is to check each direction individually using a helper method
validAnyDir(Plyr, State, [R,C]) :- valid1Dir(Plyr, State, [R,C], -1, -1).
validAnyDir(Plyr, State, [R,C]) :- valid1Dir(Plyr, State, [R,C], 0, -1).
validAnyDir(Plyr, State, [R,C]) :- valid1Dir(Plyr, State, [R,C], 1, -1).
validAnyDir(Plyr, State, [R,C]) :- valid1Dir(Plyr, State, [R,C], -1, 0).
validAnyDir(Plyr, State, [R,C]) :- valid1Dir(Plyr, State, [R,C], 1, 0).
validAnyDir(Plyr, State, [R,C]) :- valid1Dir(Plyr, State, [R,C], -1, 1).
validAnyDir(Plyr, State, [R,C]) :- valid1Dir(Plyr, State, [R,C], 0, 1).
validAnyDir(Plyr, State, [R,C]) :- valid1Dir(Plyr, State, [R,C], 1, 1).

%% valid1Dir(Plyr, State, [R,C], Ri, Ci) 
%  true if a move at [R,C] is valid for Plyr based only one if it will bracket 
%  enemy pieces in the direction given by Ri, Ci
%  STRATEGY: check if the next spot is of the opposite
%  colour, and finally run a "find" for a spot of the original colour
valid1Dir(Plyr, State, [R,C], Ri, Ci) :- otherPlyr(Plyr, Plyr2),
									  	 Rn1 is R + Ri,  Cn1 is C + Ci, onBoard(Rn1, Cn1),
										 get(State, [Rn1, Cn1], Plyr2), Rn2 is Rn1 + Ri,
										 Cn2 is Cn1 + Ci, find(Plyr, State, [Rn2,Cn2], Ri, Ci).
%% find(Plyr, State, [R,C], Ri, Ci) find if there is a square of the given player going
%  from (and including) [R,C] in the direction given by Ri and Ci in the given State,
%  spaces in the middle are allowed to be of the opposite player but not empty
find(Plyr, State, [R,C],_,_) :- onBoard(R,C), get(State, [R,C], Plyr).
find(Plyr, State, [R,C], Ri, Ci) :- otherPlyr(Plyr, Plyr2), get(State, [R,C], Plyr2),
									Rn is R + Ri, Cn is C + Ci, onBoard(Rn,Cn), 
									find(Plyr, State, [Rn,Cn], Ri, Ci). 

%% count(Plyr, State, Count) counts the amount of tiles belonging to Plyr
count(Plyr, State, Count) :- countHelp(Plyr, State, 35, Count),!.


countHelp(_,_, -1, 0).
countHelp(Plyr, State, Mv, Count) :- X is Mv // 6, Y is Mv mod 6, 
									 get(State, [X,Y], Plyr), !,
									 Nm is Mv - 1, countHelp(Plyr, State, Nm, C1),
									 Count is C1 + 1.
countHelp(Plyr, State, Mv, Count) :- Nm is Mv - 1, countHelp(Plyr,State,Nm,Count). 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                       %
%                                                                       %
%                Given   UTILITIES                                      %
%                   do NOT change these!                                %
%                                                                       %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% get(Board, Point, Element)
%    : get the contents of the board at position row R column C
% set(Board, NewBoard, [R, C], Value):
%    : set Value at row R column C in Board and bind resulting grid to NewBoard
%
% The origin of the board is in the upper left corner with an index of
% [0,0], the upper right hand corner has index [0,5], the lower left
% hand corner has index [5,0], the lower right hand corner has index
% [5,5] (on a 6x6 board).
%
% Example
% ?- initBoard(B), showState(B), get(B, [3,2], Value). 
%. . . . . . 
%. . . . . . 
%. . 1 2 . . 
%. . 2 1 . . 
%. . . . . . 
%. . . . . . 
%
%B = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], 
%     ['.', '.', 1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...], 
%     ['.', '.', '.', '.'|...], ['.', '.', '.'|...]]
%Value = 2 
%Yes
%?- 
%
% Setting values on the board
% ?- initBoard(B),  showState(B),set(B, NB1, [4,2], 1), set(NB1, NB2, [3,2], 1),  showState(NB2). 
%
% . . . . . . 
% . . . . . . 
% . . 1 2 . . 
% . . 2 1 . . 
% . . . . . . 
% . . . . . .
% 
% . . . . . . 
% . . . . . . 
% . . 1 2 . . 
% . . 1 1 . . 
% . . 1 . . . 
% . . . . . .
%
%B = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.', 
%1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...], ['.', '.', '.', '.'|...], ['.', '.',
% '.'|...]]
%NB1 = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.'
%, 1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...], ['.', '.', 1, '.'|...], ['.', '.
%', '.'|...]]
%NB2 = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.'
%, 1, 2, '.', '.'], ['.', '.', 1, 1, '.'|...], ['.', '.', 1, '.'|...], ['.', 
%'.', '.'|...]]

% get(Board, Point, Element): get the value of the board at position
% row R column C (indexing starts at 0).
get( Board, [R, C], Value) :- 
	nth0( R, Board, Row), 
	nth0( C, Row, Value).
 
% set( Board, NewBoard, [X, Y], Value) 

set( [Row|RestRows], [NewRow|RestRows], [0, C], Value)
    :- setInList(Row, NewRow, C, Value). 

set( [Row|RestRows], [Row|NewRestRows], [R, C], Value) :- 
	R > 0, 
	R1 is R-1, 
	set( RestRows, NewRestRows, [R1, C], Value). 

% setInList( List, NewList, Index, Value) 

setInList( [_|RestList], [Value|RestList], 0, Value). 

setInList( [Element|RestList], [Element|NewRestList], Index, Value) :- 
	Index > 0, 
	Index1 is Index-1, 
	setInList( RestList, NewRestList, Index1, Value). 
 
