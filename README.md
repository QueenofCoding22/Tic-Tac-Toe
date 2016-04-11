# Tic-Tac-Toe
Game Created in Prolog

default_board([1,2,3,4,5,6,7,8,9]).

win(Board, Player) :- rwin(Board, Player).
win(Board, Player) :- colwin(Board, Player).
win(Board, Player) :- diagwin(Board, Player).

rwin(Board, Player) :- Board = [Player,Player,Player,_,_,_,_,_,_].
rwin(Board, Player) :- Board = [_,_,_,Player,Player,Player,_,_,_].
rwin(Board, Player) :- Board = [_,_,_,_,_,_,Player,Player,Player].

colwin(Board, Player) :- Board = [Player,_,_,Player,_,_,Player,_,_].
colwin(Board, Player) :- Board = [_,Player,_,_,Player,_,_,Player,_].
colwin(Board, Player) :- Board = [_,_,Player,_,_,Player,_,_,Player].

diagwin(Board, Player) :- Board = [Player,_,_,_,Player,_,_,_,Player].
diagwin(Board, Player) :- Board = [_,_,Player,_,Player,_,Player,_,_].

%The moves that o's can make in the nine different positions
omove([Move,B,C,D,E,F,G,H,I], 1, [o,B,C,D,E,F,G,H,I]) :- number(Move).    
omove([A,Move,C,D,E,F,G,H,I], 2, [A,o,C,D,E,F,G,H,I]) :- number(Move).
omove([A,B,Move,D,E,F,G,H,I], 3, [A,B,o,D,E,F,G,H,I]) :- number(Move).
omove([A,B,C,Move,E,F,G,H,I], 4, [A,B,C,o,E,F,G,H,I]) :- number(Move).
omove([A,B,C,D,Move,F,G,H,I], 5, [A,B,C,D,o,F,G,H,I]) :- number(Move).
omove([A,B,C,D,E,Move,G,H,I], 6, [A,B,C,D,E,o,G,H,I]) :- number(Move).
omove([A,B,C,D,E,F,Move,H,I], 7, [A,B,C,D,E,F,o,H,I]) :- number(Move).
omove([A,B,C,D,E,F,G,Move,I], 8, [A,B,C,D,E,F,G,o,I]) :- number(Move).
omove([A,B,C,D,E,F,G,H,Move], 9, [A,B,C,D,E,F,G,H,o]) :- number(Move).
omove(Board, _, Board) :- fail.

%Moves that x can make in the different poisitons
xmove([Move,B,C,D,E,F,G,H,I], 1, [x,B,C,D,E,F,G,H,I]) :- number(Move).
xmove([A,Move,C,D,E,F,G,H,I], 2, [A,x,C,D,E,F,G,H,I]) :- number(Move).
xmove([A,B,Move,D,E,F,G,H,I], 3, [A,B,x,D,E,F,G,H,I]) :- number(Move).
xmove([A,B,C,Move,E,F,G,H,I], 4, [A,B,C,x,E,F,G,H,I]) :- number(Move).
xmove([A,B,C,D,Move,F,G,H,I], 5, [A,B,C,D,x,F,G,H,I]) :- number(Move).
xmove([A,B,C,D,E,Move,G,H,I], 6, [A,B,C,D,E,x,G,H,I]) :- number(Move).
xmove([A,B,C,D,E,F,Move,H,I], 7, [A,B,C,D,E,F,x,H,I]) :- number(Move).
xmove([A,B,C,D,E,F,G,Move,I], 8, [A,B,C,D,E,F,G,x,I]) :- number(Move).
xmove([A,B,C,D,E,F,G,H,Move], 9, [A,B,C,D,E,F,G,H,x]) :- number(Move).
xmove(Board, _, Board) :- write('illegal x move.'), nl.

% displays the nine positions to replicate a tic tac toe board. 
disp([A,B,C,D,E,F,G,H,I]) :-
        write('|'),
        write([A,B,C]),write('|'),nl,
        write('|'),
        write([D,E,F]),write('|'),nl, write('|'),
        write([G,H,I]),write('|'),nl,nl.

start :-
    how_to_play,
    % play([]).
    default_board(B),
    play(B).

how_to_play :-
        write('You are player x, enter positions followed by a period.'),
        nl,
        % disp([1,2,3,4,5,6,7,8,9]).
        default_board(B),
        disp(B).

check_win(Board, Who) :- win(Board, Who),
        write(Who), write(' won!'), nl, abort.

% a draw if no numbers left in board
check_draw(Board) :-
    check_draw2(Board), write('had a draw'), nl, abort.
check_draw2([]).
check_draw2([X | Rest]) :-
    \+member(X, [1,2,3,4,5,6,7,8,9]),
    check_draw2(Rest).

% retrives an o on the board
retrieve_o(Board, M, Newboard) :-
   default_board(Legal_values),
   repeat,
      (
      write('O\'s move '),
      read(Value),
         (
         Value == 'q', write('O quits.'), nl, abort
         ;
         member(Value, Legal_values), M = Value,
         omove(Board, M, Newboard)
         )
      )
   .
retrieve_x(Board, M, Newboard) :-
   default_board(Legal_values),
   repeat,
      (
      write('X\'s move '),
      read(Value),
         (
         Value == 'q', write('X quits.'), nl, abort
         ;
         member(Value, Legal_values), M = Value,
         xmove(Board, M, Newboard)
         )
      )
   .
play(Board) :- check_win(Board, x).
play(Board) :- check_win(Board, o).
play(Board) :-
    
    retrieve_x(Board,N,Newboard),
    write('board is '), write(Board), nl,
    write('N is '), write(N), nl,
    xmove(Board, N, Newboard),
    write('new board is '), write(Newboard), nl,
    % disp(Newboard),(win(x),write('X wins'),abort; true),
    disp(Newboard), !,
    (check_win(Newboard, x) ; write('No win'), nl, true),
    write('about to check_draw'), nl,
    (check_draw(Newboard) ; true),
    write('returned from check_draw'), nl,
   
    retrieve_o(Newboard,_,Newnewboard),
    % disp(Newnewboard),(write('o takes this position'), write(M)), nl;
    disp(Newnewboard), (check_win(Newboard, o) ; true),
    % write('o loses'), abort.
    play(Newnewboard).
  
 
