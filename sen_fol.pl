% sen_fol.pl 
% 
%   This file is widely inspired from Witzig, S. (2003). 
%   "Accessing wordnet from Prolog". Artificial Intelli-
%	gence Centre, University of Georgia, 1-18.  
%
%	You can find that source file at 
%						http://ai1.ai.uga.edu/mc/pronto/
%
%	I translated only the forms requiring noun arguments
% 	(or no arguments at all). The translation of all the
% 	other forms  would require a  more accurate model of 
%	verbs,   such  as  described  in the  Chapter  4  of 
% 	the book Pereira & Shieber, 1987.
%
%	Moreover, to support PP you would need the is-a rel
% 	that you could build from WordNet, to check if the 
%	args of a verb are actually of the required semantic
%	area. E.g.: 'I go to school' ==> 'school' must be a 
%	place.
%

%% Form of clauses: sen_fol(FR, Word, FOL).

:- op(100,fx,--).

%% Utilities to create verbal forms on-the-fly.
create_iv(Word, X^ --Term):- Term =.. [Word, X].
create_tv_unary(Word, X^Y^ --Term):- Term =.. [Word, X, Y].
create_tv_binary(Word, X^Y^Z^ --Term):- Term =.. [Word, X, Y, Z].


%% Verbal FOL dynamically created
sen_fol_iv(1, Word, FOL):- atom_concat(Word, s, V), create_iv(Word, FOL).				%iv
sen_fol_iv(2, Word, FOL):- atom_concat(Word, s, V), create_iv(Word, FOL). 				%iv 
sen_fol_tv(8, Word, FOL):-	atom_concat(Word, s, V), create_tv_unary(V, FOL).		%tv
sen_fol_tv(9, Word, FOL):-	atom_concat(Word, s, V), create_tv_unary(V, FOL).		%tv
sen_fol_tv(10, Word, FOL):-	atom_concat(Word, s, V), create_tv_unary(V, FOL).		%tv
sen_fol_tv(11, Word, FOL):-	atom_concat(Word, s, V), create_tv_unary(V, FOL).		%tv
sen_fol_tv(14, Word, FOL):- atom_concat(Word, s, V), create_tv_binary(V, FOL).		%tv