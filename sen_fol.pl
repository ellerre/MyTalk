% sen_fol.pl 
% 
%  == TO WRITE ==
%	I translated only the forms requiring noun arguments
% 	(or no arguments at all). The translation of all the
% 	other forms  would require a  more accurate model of 
%	verbs,   such  as  described  in the  Chapter  4  of 
% 	the book Pereira & Shieber, 1987.

%% Form of clauses: sen_fol(FR, Word, FOL).

:- op(100,fx,--).

%% Utilities to create verbal forms on-the-fly.
create_iv(Word, X^ --Term):- Term =.. [Word, X].
create_tv_unary(Word, X^Y^ --Term):- Term =.. [Word, X, Y].
create_tv_binary(Word, X^Y^Z^ --Term):- Term =.. [Word, X, Y, Z].


%% Verbal FOL dynamically created
sen_fol_iv(1, Word, FOL):- atom_concat(Word, s), create_iv(Word, FOL).			%iv
sen_fol_iv(2, Word, FOL):- atom_concat(Word, s), create_iv(Word, FOL). 			%iv 
sen_fol_tv(8, Word, FOL):-	atom_concat(Word, s, V), create_tv_unary(V, FOL).		%tv
sen_fol_tv(9, Word, FOL):-	atom_concat(Word, s, V), create_tv_unary(V, FOL).		%tv
sen_fol_tv(10, Word, FOL):-	atom_concat(Word, s, V), create_tv_unary(V, FOL).		%tv
sen_fol_tv(11, Word, FOL):-	atom_concat(Word, s, V), create_tv_unary(V, FOL).		%tv
sen_fol_tv(14, Word, FOL):- atom_concat(Word, s, V), create_tv_binary(V, FOL).		%tv


%sen( 3,'It is ','ing.'). 								
%sen(4,'Something is ','ing PP.'). 
%sen(5,'Something ','s something Adjective/Noun.').	(only two verbs in the whole lexicon)
%sen(6,'Something ','s Adjective/Noun.').			(do not treat adjectives so far, and only two verbs in the whole lexicon)
%sen(7,'Somebody ','s Adjective.').					(do not treat adjectives so far, and only two verbs in the whole lexicon)
%sen(12,'Something ','s to somebody.').				(do not treat prepositional phrases)
%sen(13,'Somebody ',' on something.').				(do not treat prepositional phrases)
%sen(15,'Somebody ','s something to somebody.').	(do not treat prepositional phrases)
%sen(16,'Somebody ','s something from somebody.').	(do not treat prepositional phrases)
%sen(17,'Somebody ','s somebody with something.').	(do not treat prepositional phrases)
%sen(18,'Somebody ','s somebody of something.').	(do not treat prepositional phrases)
%sen(19,'Somebody ','s something on somebody.').	(do not treat prepositional phrases)
%sen(20,'Somebody ','s somebody PP.').				(too complex for the moment, do not treat prepositional phrases)
%sen(21,'Somebody ','s something PP.').				(too complex for the moment, do not treat prepositional phrases)
%sen(22,'Somebody ','s PP.').						(too complex for the moment, do not treat prepositional phrases)
%sen(23,'Somebody\'s (body part)','s.').			(too complex for the moment)

/* Following frames are not so common and too complex to be treatd from MyTalk in this version
sen(24,'Somebody ','s somebody to INFINITIVE.').		-
sen(25,'Somebody ','s somebody INFINITIVE.').			rov
sen(26,'Somebody ','s that CLAUSE.').					-
sen(27,'Somebody ','s to somebody.').					-
sen(28,'Somebody ','s to INFINITIVE.').					-
sen(29,'Somebody ','s whether INFINITIVE.').			-
sen(30,'Somebody ','s somebody into V-ing something.').	-
sen(31,'Somebody ','s something with something.').		-
sen(32,'Somebody ','s INFINITIVE.').					-
sen(33,'Somebody ','s VERB-ing.').						-
sen(34,'It ','s that CLAUSE.').							-
sen(35,'Something ','s INFINITIVE.').					-
*/