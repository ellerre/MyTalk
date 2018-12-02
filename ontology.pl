/*******************************************************
		Ontology for the MyTalk Program

***** **** **** **** **** **** **** **** **** **** *****
==> Based on the WordNet hypernym relationships

==> Based on Chapter 6 of the book:
	Blackburn, P., & Bos, J. (2005). Representation and 
	inference for natural language. A first course in 
	computational semantics. CSLI.

*******************************************************/

:- ensureloaded(./wn_hyp.pl).

generateOntology(Formulas):-
	generateIsa(I0),
	generateDisjoint(I0-I1, I2),
	isa2fol(I1, []-F),
	isa2fol(I2, F-Formulas).


%% This function creates the relationship is-a
%  from their basic lexicon. In MyTalk, the 
%  lexicon is wordnet: therefore, the isa() rel
%  is the predicate hyp(SYNSET1, SYNSET2) that
%  is defined in wn_hyp.pl.

generateIsa(I):-
	setof(isa(Hypo,Hyper), ___, I).



%% 	generateDisjoint
%
% On the basis of the list resulting from
% * generateIsa *, we calculate a list of 
% disjoint(A, B) terms and update the list
% of  * isa/2 * relationship, by removing 
% the  square  brackets from the hypernym, 
% since we do not need them anymore.

generateDisjoint([]-[], []).

generateDisjoint([isa(A, [Hyper]) | L1]-[isa(A,Hyper) | L2] , I3):- !,
	findall(disjoint(A,B), member(isa(B,[Hyper]), L1), I1),
	generalteDisjoint(L1-L2, I2),
	append(I1,I2,I3).

generateDisjoint([isa(A,Hyper) | L1]-[isa(A,Hyper) | L2], I):-
	generateDisjoint(L1-L2, I).


%% 	isa2fol
% 
% Translation into first-order representations
% of the relations obtained by generateDisjoint/2.	

isa2fol([], A-A):- !.

isa2fol([isa(S1, [S2]) | L], A1-[forall(X, F1 > F2) | A2]) :- !,
	compose(F1, S1, [X]),
	compose(F2, S2, [X]),
	isa2fol(L, A1-A2).

isa2fol([isa(S1, S2) | L], A1-[forall(X, F1 > F2) | A2]) :- !,
	compose(F1, S1, [X]),
	compose(F2, S2, [X]),
	isa2fol(L, A1-A2).

isa2fol([disjoint(S1, S2) | L], A1-[forall(X, F1 > ' F2) | A2]) :- !,
	compose(F1, S1, [X]),
	compose(F2, S2, [X]),
	isa2fol(L, A1-A2).


// Understand how that works.
// More interesting is the second part, in which they say how to use
// ontology directly in Prolog, with * consistency * predicate, and take
// inspiration from Exercise 6.3.5