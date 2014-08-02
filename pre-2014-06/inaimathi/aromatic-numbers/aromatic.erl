-module(aromatic).
-compile(export_all).

lookup(73) -> 1;	%% I
lookup(86) -> 5;	%% V
lookup(88) -> 10;	%% X
lookup(76) -> 50;	%% L
lookup(67) -> 100;	%% C
lookup(68) -> 500;	%% D
lookup(77) -> 1000.	%% M

fromAromatic(List) -> fromAromatic(List, 0).
fromAromatic([A, R, A2, R2 | Rest], Sum) ->
    Prod = (A-48)*lookup(R), Next = [A2, R2 | Rest],
    case R2 > R of
	true -> fromAromatic(Next, Sum - Prod);
	false -> fromAromatic(Next, Sum + Prod)
    end;
fromAromatic([A, R], Sum) ->
    Sum + ((A-48)*lookup(R)).
