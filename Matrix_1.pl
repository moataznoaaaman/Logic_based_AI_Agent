:- include('KB.pl'). % import the knowledge base

% =====================Successor-State-Axiom-1=======================
% This axiom checks the location of neo. Given a location, checks if 
% this location is the resultant of the sequence of actions that led 
% to this state.

% Base case: At initial state s0, neo is at his initial location
neo_at(X, Y, s0):- neo_loc(X,Y).

% Is true for 6 cases
neo_at(X, Y, result(A,S)):-
    % CASE 1: A VALID down movement -> Neo's previous location was the cell above:
    (X>0, X1 is X-1, A=down,neo_at(X1, Y, S))
    ;
    % CASE 2: A VALID up movement -> Neo's previous location was the cell below:
    (grid(M,_), X1 is X+1, X1<M, A=up,neo_at(X1, Y, S))
    ;
    % CASE 3: A VALID right movement -> Neo's previous location was the cell to the left:
    (Y>0, Y1 is Y-1, A=right,neo_at(X, Y1, S))
    ;
    % CASE 4: A VALID left movement -> Neo's previous location was the cell to the right:
    (grid(_,N), Y1 is Y+1, Y1<N, A=left,neo_at(X, Y1, S))
    ;
    % CASE 5: A VALID carry -> Neo's previous location is the same:
    ((A=carry, hostages_loc(List), mem_2d([X,Y],List)),neo_at(X, Y,S))
    ;
    % CASE 6: A VALID drop -> Neo's previous location is the same:
    ((A=drop, booth(X,Y)),neo_at(X, Y,S)).

% =====================Successor-State-Axiom-2=======================
% This axiom is concerned with the hostage locations. Given a location and
% a hostage's initial location, it is true if the location can be traced back 
% to the initial location of the hostage given the sequence of actions that led 
% to this state.

% Parameters:
% X: location's X coordinate to check
% Y: location's Y coordinate to check
% X_cur_hostage: The given hostage's initial X coordinate (if hostage exists)
% Y_cur_hostage: The given hostage's initial Y coordinate (if hostage exists)
% X_other_hostage: The other hostage's initial X coordinate (if hostage exists)
% Y_other_hostage: The given hostage's initial Y coordinate(if hostage exists)
% Carried: Is the given hostage carried?
% Capacity: How many hostages neo can carry at this state

% Base case: At s0, the location matches the hostage's initial location, and the capacity equals maximum capacity since no hostage is carried
hostage_at(X_cur_hostage ,Y_cur_hostage, X_cur_hostage, Y_cur_hostage, _, _, false, Capacity, s0):- capacity(Capacity).

% Is true for 10 cases
hostage_at(X, Y, X_cur_hostage, Y_cur_hostage, X_other_hostage, Y_other_hostage, Carried, Capacity, result(A,S)):-
    
    % CASE 1: A VALID down movement and current hostage is carried -> previous location of the current hostage was the cell above:
    (Carried=true, X>0, X1 is X-1, A=down,hostage_at(X1 ,Y, X_cur_hostage, Y_cur_hostage, X_other_hostage, Y_other_hostage, Carried, Capacity, S))
    ;
    % CASE 2: A VALID up movement and current hostage is carried -> previous location of the current hostage was the cell below:
    (Carried=true, grid(M,_), X1 is X+1, X1<M, A=up, hostage_at(X1 ,Y,X_cur_hostage, Y_cur_hostage, X_other_hostage, Y_other_hostage, Carried, Capacity, S))
    ;
    % CASE 3: A VALID right movement and current hostage is carried -> previous location of the current hostage was the cell to the left:
    (Carried=true,Y>0, Y1 is Y-1, A=right,hostage_at(X ,Y1, X_cur_hostage, Y_cur_hostage, X_other_hostage, Y_other_hostage, Carried, Capacity, S))
    ;
    % CASE 4: A VALID left movement and current hostage is carried -> previous location of the current hostage was the cell to the right:
    (Carried=true,grid(_,N), Y1 is Y+1, Y1<N, A=left,hostage_at(X ,Y1, X_cur_hostage, Y_cur_hostage, X_other_hostage, Y_other_hostage, Carried, Capacity, S))
    ;
    % CASE 5: A VALID movement and current hostage is not carried -> previous location of the current hostage is the same:
    (Carried=false, (A=up; A=down; A=left; A=right), hostage_at(X ,Y, X_cur_hostage, Y_cur_hostage, X_other_hostage, Y_other_hostage, Carried, Capacity, S))
    ;
    % CASE 6: A VALID drop and current hostage is not carried -> previous location of the current hostage is the same, the current hostage was carried, capacity was decreased by 1:
    (Carried=false, Capacity>0, Capacity1 is Capacity-1, booth(X,Y), A=drop, hostage_at(X ,Y, X_cur_hostage, Y_cur_hostage, X_other_hostage, Y_other_hostage, true, Capacity1, S))
    ;
    % CASE 7: A VALID drop and current hostage is not carried, however neo was carrying both hostages previously -> previous location is the same, the current hostage was carried, capacity was decreased by 2 (since both hostages are carried):
    (Carried=false, Capacity>1, Capacity1 is Capacity-2, booth(X,Y), A=drop, hostage_at(X ,Y, X_cur_hostage, Y_cur_hostage, X_other_hostage, Y_other_hostage, true, Capacity1, S))
    ;
    % CASE 8: It is divided into 2 sub-cases
    %	CASE 8.1: A VALID drop for the other hostage and current hostage is at the booth
    %	CASE 8.2: A VALID drop for the other hostage and current hostage is not with Neo at the telephone booth(i.e is not carried by neo). This means this hostage is not being interacted with
    % In both cases the current hostage was at the same location, has the same carry status, and the capacity was decreased by 1 (because the other hostage has been dropped).
    (Capacity>0, Capacity1 is Capacity-1, (booth(X,Y) ; Carried=false), A=drop, hostage_at(X ,Y, X_cur_hostage, Y_cur_hostage, X_other_hostage, Y_other_hostage, Carried, Capacity1, S))
    ; 
    % CASE 9: A VALID carry and Neo is at the current hostage's initial location -> previous location is the same, the current hostage was not carried, capacity was increased by 1:
    (Carried=true, capacity(C), Capacity<C, Capacity1 is Capacity+1, X == X_cur_hostage, Y == Y_cur_hostage, A= carry, hostage_at(X ,Y, X_cur_hostage, Y_cur_hostage, X_other_hostage, Y_other_hostage, false, Capacity1, S))
    ;  
    % CASE 10: It is divided into 2 sub-cases
    %	CASE 10.1: VALID carry for the other hostage and current hostage is with Neo at the other hostage's initial location(i.e current hostage was carried in a previous state)
    %	CASE 10.2: VALID carry and current hostage is not with Neo at the other hostage's initial location (i.e is not carried). This means this hostage is not being interacted with
    % In both cases the current hostage was at the same location, has the same carry status, and the capacity was increased by 1 (because the other hostage has been carried).
    (capacity(C), Capacity<C, Capacity1 is Capacity+1, ((X == X_other_hostage, Y == Y_other_hostage) ; Carried=false), A= carry, hostage_at(X ,Y, X_cur_hostage, Y_cur_hostage, X_other_hostage, Y_other_hostage, Carried, Capacity1, S)).

% If the state is a variable, goal uses IDS to match the state with goal states.
% If the state is given, goal checks if the state is a goal state
goal(S):-
    (var(S),ids(0, S))
    ;  
    goal_help(S).

% IDS keeps calling depth limited search with increasing maximum depth until a solution is found
ids(N,S):- call_with_depth_limit(goal_help(S),N,R), R \= depth_limit_exceeded.
ids(N,S):- N1 is N + 1, ids(N1,S).


% Helper function that checks if a state is a goal state
goal_help(S):-
     booth(G1, G2), % get booth location
     hostages_loc(List), % get the list of hostages
     length(List, Length), % get the number of hostages
     capacity(Capacity), % get th maximum capacity
     (
     % CASE 1: No hostages -> goal if neo at the booth
     (Length == 0, neo_at(G1, G2, S))
     ;
     % CASE 2: One hostage -> goal if neo and the hostage are at the booth
     (Length == 1, first_elem(List,[H11,H12]), neo_at(G1, G2, S), hostage_at(G1 ,G2, H11, H12, -1, -1, false, Capacity, S))
     ;
     % CASE 3: Two hostages -> goal if neo and both hostages are at the booth
     (Length == 2, first_elem(List,[H11,H12]), last_elem(List,[H21,H22]), neo_at(G1, G2, S), hostage_at(G1 ,G2, H11, H12, H21, H22, false, Capacity, S)), hostage_at(G1 ,G2, H21, H22, H11, H12, false, Capacity, S)).

% Returns the first element in a list
first_elem([H|_], H).

% Returns the last element in a list
last_elem([L], L).
last_elem([_|T], L):-
    last_elem(T, L).

% Checks if something is a member of a list
mem(X,[H|T]):-
	X == H;
	mem(X,T).

% Checks if something is a member of a list of lists
mem_2d(X,[H|T]):-
	list_eq(X, H);
	mem_2d(X,T).

% Checks if two lists are equal
list_eq([], []).
list_eq([H1|T1], [H2|T2]):-
    H1 == H2,
    list_eq(T1, T2).
