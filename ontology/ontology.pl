/*******************************************************
		Ontology for the MyTalk Program

***** **** **** **** **** **** **** **** **** **** *****
==> Based on the WordNet hypernym relationships

==> Based on Chapter 6 of the book:
	Blackburn, P., & Bos, J. (2005). Representation and 
	inference for natural language. A first course in 
	computational semantics. CSLI.

==> Generates ontology and disjointness predicates as 
	soon as the file is opened.

===> IMPORTANT
	Since Prolog is not able to manage the entire WN
	ontology, we tested this program only on a subset
	of it: for that reason, all WN predicates are here
	followed by the suffix _demo.
	We hope that in the future someone will take care
	of a really working integration with WN.	
*
******************************************************/

:- ensure_loaded('./wordnet_demo/wn_hyp_demo.pl').
:- ensure_loaded('./wordnet_demo/wn_s_demo.pl').
:- ensure_loaded('./wordnet_demo/wn_ant_demo.pl').

:- dynamic isa/2.

%%%%%%%%%%%%%%%% generateIsa(I) %%%%%%%%%%%%%%%%%%%%%%%
%% This function creates the relationship is-a
%  from their basic lexicon. In MyTalk, the 
%  lexicon is wordnet: therefore, the isa() rel
%  is the predicate hyp(SYNSET1, SYNSET2) that
%  is defined in wn_hyp.pl.
% 		

generateIsa(I):-
	setof(
			isa(Hypo,Hyper), 
			(hyp_demo(Syn1, Syn2), s_demo(Syn1, _, Hypo, n,_,_), s_demo(Syn2, _, Hyper, n,_,_)),
			I
		).

% Identity and transitivity
isa(X, Y) :- X == Y.
%isa(X, Y) :- isa(X, Z), isa(Z, Y).

%%%%%%%%%%%%%%%% generateDisjoint(I) %%%%%%%%%%%%%%%%%%

%% 	generateDisjoint
%
% On the basis of the list resulting from
% * generateIsa *, we calculate a list of 
% disjoint(A, B) terms and update the list
% of  * isa/2 * relationship, by removing 
% the  square  brackets from the hypernym, 
% since we do not need them anymore.
%
% HOWEVER being an antonym of a word, does 
% not mean that  you can always say,  the
% antonym is not the word.
%
% Sadly, in WN we have only this kind of 
% relationship, so this is the only way to
% handle the disjointness.

generateDisjoint(I):-
	setof(
			disjoint(A,B), 
			(	ant_demo(Syn1, Wn1, Syn2, Wn2), 
				s_demo(Syn1, Wn1, A, n,_,_), 
				s_demo(Syn2, Wn2, B, n,_,_),
				isa(A, C), isa(B, C)),
			I
		).


%%%%%%%%%%%%%%%% consistent(X,Y) %%%%%%%%%%%%%%%%%%%%%

% To use the ontology directly in Prolog, we 
% define the * consistency * predicate,  and 
% take % inspiration from Exercise 6.3.5  of 
% the book mentioned above.


consistent(X,X).

consistent(X,Y):-
	isa(X,Y),
	\+ inconsistent(X,Y).

consistent(X,Y):-
	isa(Y, X),
	\+ inconsistent(X,Y).

consistent(X,Y):-
	isa(X, Z),
	isa(Y, Z),
	\+ inconsistent(X,Y).

inconsistent(X,Y):-
	disjoint(X,Y).

inconsistent(X,Y):-
	isa(X,Z),
	inconsistent(Z,Y).

inconsistent(X,Y):-
	isa(Y,Z),
	inconsistent(X,Z).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %%%%%  Automatic generation of ontology %%%%%

generateOntology:- 
		bagof(X, generateIsa([X]), Bag),
		generateOntology(Bag).

generateOntology([]).
generateOntology([H|T]):- 
		asserta(H),
		generateOntology(T).
		
generateDisjointness:- 
		bagof(X, generateDisjoint([X]), Bag),
		generateDisjointness(Bag).

generateDisjointness([]).

generateDisjointness([H|T]):-
		assert(H), 
		H =.. [disjoint, X, Y],
		assert(disjoint(Y,X)),
		generateDisjointness(T).



:- generateOntology.
:- generateDisjointness.



%%%%%%%% find_hyp_chain(Word, List) %%%%%%%%%%%%%%
%
% Utility to find the chain of hypernym of a given
% word.
%

find_hyp_chain(Word, [NewWord | List]):-
	s_demo(Sy, _, Word, _, _, _),
	hyp_demo(Sy, NewSy),
	s_demo(NewSy, _, NewWord, _, _, _), 
	find_hyp_chain(NewWord, List).

find_hyp_chain(entity, []).



