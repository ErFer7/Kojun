% Parser
% Leitura e processamento de dados

% :- module(parser, [my_read_file/3]).

% read_file(File, Firt_Number, StrList) :-
%     open(File, read, Stream),
%     read_line(Stream, [Firt_Number]),
%     read_line(Stream, List),
%     close(Stream).

% my_read_file(File,Firt_Number ,List):-
%     open(File, read, Stream),
%     read_line(Stream, [Firt_Number]),
%     read_line(Stream, List),
%     close(Stream).

% read_line(Stream, List) :-
%     read_line_to_codes(Stream, Line),
%     atom_codes(A, Line),
%     atomic_list_concat(As, ' ', A),
%     maplist(atom_number, As, List).
