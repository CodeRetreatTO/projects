%% -*- Mode: PROLOG -*-

%% glider([[0, 1], [1, 2], [2, 0], [2, 1], [2, 2]]).
%% neighbors([[-1, -1], [-1, 0], [-1, 1], [0, -1], [0, 1], [1, -1], [1, 0], [1, 1]]).

%% vector_add([X1, Y1], [X2, Y2], [X3, Y3]) :- 
%% 	X3 is X1 + X2,
%% 	Y3 is Y1 + Y2.

%% vector_list_add(Cell, [V1 | Rest1], [V2 | Rest2]) :-
%% 	vector_add(Cell, V1, V2),
%% 	vector_list_add(Cell, Rest1, Rest2).
%% vector_list_add(_, [], []).

%% census()

glider([".O.", "..O", "OOO"]).

step(In, Out) :-
	
