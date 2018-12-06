/******************************************************
					MyTALK Program

This  program  is  an  extension  of  the Talk program 
presented  in  (Pereira and Shieber, 1987).  The  main 
improvements are that:
 - a  morphological  analysis  is performed, so not to 
 	write into the code any possible form of any pos-
 	sible word.
 - integration with WordNet allows to handle any common
 	name and (theoretically) any verb. For verb treat-
 	ment, see *sen_fol.pl*.
 - a basic syntactic analysis is performed, allowing
 	to check the concordance in number between subject 
 	and its verb.
 - transformation  in  clauses  is much more powerful,
 	since it allows to handle a significant number of
 	LFs if compared to Talk.

 - ATTENTION. The predicate "Reply", in "assertion"
 	mode, has active a demo of a semantic analysis.
 	Since it is quite constraining on the proper 
 	noun, my advice is to comment the predicate
 	'check_consistency' until you are not sufficiently
 	confident.

However, all the limits of Talk are still present.
Finally, this program is extremely inefficient, and
could be easily improved (such as in the verbal part)

******************************************************/



/*=====================================================
					Operators
=====================================================*/

:- op(500,xfy,&).
:- op(510,xfy,=>).
:- op(100,fx,--).

:- ensure_loaded('./dcgcompiler.pl').
:- ensure_loaded('./morphological_parser/pronto_morph_engine.pl').
:- ensure_loaded('./wordnet/wn_s.pl').
:- ensure_loaded('./wordnet/wn_fr.pl').
:- ensure_loaded('./sen_fol.pl').
:- ensure_loaded('./ontology/ontology.pl').

:- dynamic n/2.


/*=====================================================
					Dialogue Manager
=====================================================*/
%%% main_loop
%%% =========

main_loop :-
	write('>> '), 					% prompt the user
	read_sent(Words), 				% read a sentence
	talk(Words, Reply),				% process it with TALK
	print_reply(Reply), 			% generate a printed reply
	main_loop. 						% pocess more sentences


%%% talk(Sentence, Reply)
%%% =====================
%%%
%%% 	Sentence ==> sentence to form a reply to
%%% 	Reply <== appropriate reply to the sentence

talk(Sentence, Reply) :-
	% parse the sentence
	parse(Sentence, LF, Type),
	
	% convert the FOL logical form into a Horn
	% clause, if possible
	clausify(LF, Clause, FreeVars), !,
	
	% concoct a reply, based on the clause and
	% whether sentence was a query or assertion
	catch(reply(Type, FreeVars, Clause, Reply),
		 _, write('Can\'t answer: too difficult or some logical passage is missing! ')).


% No parse was found, sentence is too difficult.
talk(_Sentence, error('too difficult')).


%%% reply(Type, FreeVars, Clause, Reply)
%%% ====================================
%%%
%%% 	Type ==> the constant "query" or "assertion"
%%% 			 depending on whether clause should
%%% 			 be interpreted as a query or
%%% 			 assertion.
%%% 	FreeVars ==> the free variables (to be
%%% 				 interpreted existentially) in the
%%% 				 clause.
%%% 	Clause 	 ==> the clause being replied to
%%% 	Reply 	 <== the reply
%%%
%%% 	If the clause is interpreted as an assertion,
%%% 	the predicate has a side effect of asserting
%%% 	the clause to the database.

% Replying to a query.

reply(query, FreeVars,
		(answer(Answer):-Condition), Reply) :-
	% find all the answers that satisfy the query,
	% replying with that set if it exists, or "no"
	% or "none" if it doesn't.
	(setof(Answer, FreeVars^Condition, Answers),
	expand_answers(Answer, FreeVars, Answers, ExtendedAnswers)
		-> Reply = answer(ExtendedAnswers)
		; (Answer = []
			-> Reply = answer([none])
			; Reply = answer([no]))), !.


% Replying to an assertion.

reply(assertion, _FreeVars,
	  Assertion, asserted(Assertion)) :-

	  %% Here we insert the semantic control.
	  check_consistency(Assertion), !,

	% assert the assertion and tell user what we asserted
	% before, we check if the assertion already existed.
	  do_assertion(Assertion).

% Replying to some other type of sentence.

reply(_Type, _FreeVars, _Clause, error('Statement semantically inconsistent')).


%%%% Reply utilities
%%%% ========================================

%%% DO ASSERTION
do_assertion(Assertion):-	  
	retract(Assertion) 
		->	assert(Assertion)
		; 	assert(Assertion), !.


%%%% EXPAND ANSWER
% The following is used in case of a recursive reply.
%	Eg. if the response is a common name, I check if there
% 	are responses to common_name(X), otherwise I return it 
% 	as an atom.

expand_answers(_, _, [],[]).

expand_answers(Answer, FreeVars, [First | Rest], [NewResp|Still]):- 
	current_predicate((First)/1), !, 
	Pred =.. [First, X],
	setof(X, FreeVars^Pred, NewResp),
	expand_answers(Answer, FreeVars, Rest, Still).

expand_answers(Answer, FreeVars, [First | Rest], [First | Resp]):- 
	expand_answers(Answer, FreeVars, Rest, Resp).


%%%% CHECK CONSISTENCY
%
%	This is the demo of the integration with a 
%	semantic analyser.
%
%

check_consistency(Assertion):-					% Check consistency of proper nouns
	Assertion =.. [ Head, Name ],
	pn(Name, Name, sg, Kind),
	consistent(Head, Kind).


check_consistency(Assertion):-					% Not interested in other cases
	Assertion =.. [ Head, Name ],
	\+ pn(Name, Name, sg, Kind).

check_consistency(Assertion):-					% Not interested in other cases
	Assertion =.. [ Head, _ | T ],
	T \== [].


%%% print_reply(Reply)
%%% ==================
%%%
%%% 	Reply ==> reply generated by reply predicate
%%% 			  that is to be printed to the
%%% 			  standard output.

print_reply(error(ErrorType)) :-
	write('Error: "'), write(ErrorType), write('."'), nl.

print_reply(asserted(Assertion)) :-
	write('Asserted "'), write(Assertion), write('."'), nl.

print_reply(answer(Answers)) :-
	print_answers(Answers).


%%% print_answer(Answers)
%%% =====================
%%%
%%% 	Answers ==> nonempty list of answers to be printed
%%% 				to the standard output separated
%%% 				by commas.

print_answers([Answer]) :- !,
	write(Answer), write('.'), nl.

print_answers([Answer|Rest]) :-
	write(Answer), write(', '),
	print_answers(Rest).


%%% parse(Sentence, LF, Type)
%%% =========================
%%%
%%% 	Sentence ==> sentence to parse
%%% 	LF <== logical form (in FOL) of sentence
%%% 	Type <== type of sentence
%%% 			(query or assertion)


% Parsing an assertion: a finite sentence without gaps.
parse(Sentence, LF, assertion) :-
	s(LF, nogap, Sentence, []).

% Parsing a query: a question.
parse(Sentence, LF, query) :-
	q(LF, Sentence, []), !.










/*=====================================================
					Clausifier
=====================================================*/

%%% clausify(FOL, Clause, FreeVars)
%%% ===============================
%%%
%%% 	FOL ==> FOL expression to be converted
%%% 			to clause form
%%% 	Clause <== clause form of FOL expression
%%% 	FreeVars <== free variables in clause
% Universals: variable is left implicitly scoped.
clausify(all(X,F0),F,[X|V]) :- clausify(F0,F,V).


% Implications: consequent must be a literal,
% 				antecedent is clausified specially.
clausify(A0=>C0,(C:-A),V) :-
	clausify_literal(C0,C),
	clausify_antecedent(A0,A,V).	

% Literals: left unchanged (except literal
% 			marker is removed).
clausify(C0,C,[]) :- clausify_literal(C0,C).


%% IMPORTANT NOTE
% Note that conjunctions and existentials are
% disallowed, since they can't form Horn clauses.
% An example of resulting clauses can be obtained
% removing the comments above, or below:

%	clausify(exists(X,F0),F,[X|V]) :-
%		clausify_antecedent(F0,F,V).

%	clausify(A0=>C0,(C:-A),V) :-
%			clausify_antecedent(C0, C, _),
%			clausify_antecedent(A0,A,V).	


%%% clausify_antecedent(FOL, Clause, FreeVars)
%%% ==========================================
%%%
%%% 	FOL ==> FOL expression to be converted
%%% 			to clause form
%%% 	Clause <== clause form of FOL expression
%%% 	FreeVars ==> list of free variables in clause

% Variables: it is not a FOL to be converted, so fail
clausify_antecedent(Var, _, _):- var(Var), !, fail.

% Literals: left unchanged (except literal
% 			marker is removed).
clausify_antecedent(L0,L,[]) :- clausify_literal(L0,L).

% Conjunctions: each conjunct is clausified separately.
clausify_antecedent(E0&F0,(E,F),V) :-
	clausify_antecedent(E0,E,V0),
	clausify_antecedent(F0,F,V1),
	conc(V0,V1,V).

% Existentials: variable is left implicitly scoped.
clausify_antecedent(exists(X,F0),F,[X|V]) :-
	clausify_antecedent(F0,F,V).



%%% clausify_literal(Literal, Clause)
%%% =================================
%%%
%%% 	Literal ==> FOL literal to be converted
%%% 				to clause form
%%% 	Clause <== clause form of FOL expression
% If compound, clausifies the internal structure
% and then builds a clause composing external and
% internal  FOL.   Otherwise,  literal  is  left
% unchanged (except literal  marker is removed).

clausify_literal(--L, Z):- 
	L =.. [External | Arg], 		% Tries to decompose
	Arg = [Var | _],
	\+ var(Var),					% check if compound
	clausify_compound(L, Internal), 
	Z =.. [External | Internal], !.

clausify_literal(--L, Z):- 
	L=.. [External, Var | Rest],	% Starts with a variable
	clausify_compound(Rest, Res),
	Z =.. [External, Var | Res].

clausify_literal([[]], []).			% End recursion

clausify_compound([Var | Rest], [Var | Still]):- var(Var),  %a variable, actually
						clausify_literal( [Rest], Still),
						!.
clausify_compound([Atom | Rest], [Atom | Still]):- atom(Atom),  %an atom, actually
						clausify_literal( [Rest], Still),
						!.						


clausify_compound(L , [Z | Still]):- L =.. [ _ , Interm | Rest],
						 Interm =.. [^,B,Interm2], 
						 Interm2 =.. [--, Interm3], 
						 Interm3 =.. [Z , B],
						 Rest = [Var],
						 \+ var(Var),
						 Var == [],
						 clausify_literal( Rest , Still), 
						 !.

clausify_compound(L , [Z | Still]):- L =.. [ _ , Interm | Rest],
						 Interm =.. [^,B,Interm2], 
						 Interm2 =.. [--, Interm3], 
						 Interm3 =.. [Z , B],
						 Rest = [Var],
						 \+ var(Var),
						 \+ atom(Var),
						 clausify_literal( Rest , Still), 
						 !.

clausify_compound(L , [Z | Still]):- L =.. [ _ , Interm | Rest],
						 Interm =.. [^,B,Interm2], 
						 Interm2 =.. [--, Interm3], 
						 Interm3 =.. [Z , B],
						 Rest = [Var],
						 \+ var(Var),
						 Var == [],
						 clausify_compound( Rest , Still), 
						 !.	

clausify_compound(L , [Z | Still]):- L =.. [ _ , Interm | Rest],
						 Interm =.. [^,B,Interm2], 
						 Interm2 =.. [--, Interm3], 
						 Interm3 =.. [Z , B],
						 Rest = [Var],
						 \+ var(Var),
						 \+ atom(Var),
						 clausify_compound( Rest , Still), 
						 !.						 

clausify_compound(L , [Z | Rest]):- L =.. [_ , Interm | Rest],
						 Interm =.. [^,B,Interm2], 
						 Interm2 =.. [--, Interm3], 
						 Interm3 =.. [Z , B].
						 %manage Rest and parse it. It can be a variable - just to add 
						 %or another thing to be clausified (in this case, recursively call)
						 %this same clause.

clausify_compound([], []).














/*=====================================================
						Grammar

Nonterminal names:
			q 			Question
			sinv 		INVerted Sentence
			s 			noninverted Sentence
			np 			Noun Phrase
			vp 			Verb Phrase
			iv 			Intransitive Verb
			tv 			Transitive Verb
			aux 		AUXiliary verb
			rov 		subject-Object Raising Verb
			optrel 		OPTional RELative clause
			relpron 	RELative PRONoun
			whpron 		WH PRONoun
			det 		DETerminer
			n 			Noun
			pn 			Proper Noun


Typical order of and values for arguments:

1. verb form:

(main verbs) finite, nonfinite, etc.
(auxiliaries and raising verbs) Form1-Form2
	where 	Form1 is form of embedded VP
			Form2 is form of verb itself)


2. FOL logical form


3. gap information:
	nogap or gap(Nonterm, Var)
		where 	Nonterm is nonterminal for gap
				Var is the LF variable that
						the filler will bind

=====================================================*/
%%% 					Questions


q(S => --answer(X)) -->
	whpron, vp(finite, X^S, nogap, _).
q(S => --answer(X)) -->
	whpron, sinv(S, gap(np, X)).
q(S => --answer(yes)) -->
	sinv(S, nogap).
q(S => --answer(yes)) -->
	[is],
	np((X^S0)^S, nogap, _),
	np((X^true)^exists(X,S0&true), nogap, _).


%%% 				Declarative Sentences

s(S, GapInfo) -->
		np(VP^S, nogap, Num),
		vp(finite, VP, GapInfo, Num), {!}.


%%% 				Inverted Sentences

sinv(S, GapInfo) -->
	aux(finite/Form, VP1^VP2),
	np(VP2^S, nogap, Num),
	vp(Form, VP1, GapInfo, Num).


%%% 				Noun Phrases
np(NP, nogap, Num) -->
		det(N2^NP, Num), n(N1, Num, det), optrel(N1^N2), {!}.
np(NP, nogap, Num) --> pn(NP, Num).
np(NP, nogap, Num) --> n(NP, Num, nodet).
np(NP, nogap, Num) --> n((NP^S)^S, Num, nodet).
np((X^S)^S, gap(np, X), _) --> [].


%%% 				Verb Phrases


vp(Form, X^S, GapInfo, Num) -->
	tv(Form, X^VP, Num),
	np(VP^S, GapInfo, _).

vp(Form, VP, nogap, Num) -->
	iv(Form, VP, Num).

vp(Form1, VP2, GapInfo, Num) -->
	aux(Form1/Form2, VP1^VP2),
	vp(Form2, VP1, GapInfo, Num).

vp(Form1, VP2, GapInfo, Num) -->
	rov(Form1/Form2, NP^VP1^VP2),
	np(NP, GapInfo, Num),
	vp(Form2, VP1, nogap, Num).

vp(Form2, VP2, GapInfo, Num) -->
	rov(Form1/Form2, NP^VP1^VP2),
	np(NP, nogap, Num),
	vp(Form1, VP1, GapInfo, Num).

%% Verb =to be=
vp(finite, X^S, GapInfo, sg) -->
	[am],
	np((X^P)^exists(X,S&P), GapInfo, sg).

vp(finite, X^S, GapInfo, sg) -->
	[is],
	np((X^P)^exists(X,S&P), GapInfo, sg).

vp(finite, X^S, GapInfo, pl) -->
	[are],
	np((X^P)^exists(X,S&P), GapInfo, pl).


%%% 				Relative Clauses

optrel((X^S1)^(X^(S1&S2))) -->
	relpron, vp(finite,X^S2, nogap, _).

optrel((X^S1)^(X^(S1&S2))) -->
	relpron, s(S2, gap(np, X)).
	optrel(N^N) --> [].


/*=====================================================
					Dictionary
=====================================================*/

/*-----------------------------------------------------
				   Preterminals
-----------------------------------------------------*/

det(LF, Num) --> [D], {det(D, LF, Num)}.


n(LF, Num, det) --> [N], {n(N, LF, Num)}.
n((E^S)^S, Num, nodet) --> [N], {n(N, E, Num)}.

pn((E^S)^S, Num) --> [PN], {pn(PN, E, Num, _)}.
aux(Form, LF) --> [Aux], {aux(Aux, Form, LF)}.
relpron --> [RP], {relpron(RP)}.
whpron --> [WH], {whpron(WH)}.

% Verb entry arguments: //TO UPDATE
%	1. nonfinite form of the verb
% 	2. third person singular present tense form of the verb
%	3. past tense form of the verb
% 	4. past participle form of the verb
% 	5. pres participle form of the verb
% 	6. logical form of the verb


iv(nonfinite, LF, Num) --> [IV], {morph_atoms(IV, R), iv(R, _, _, _, _, _, _, _, LF, Num), !}.
iv(finite, LF, Num) --> [IV], {morph_atoms(IV, R), iv(R, _, _, _, _, _, _, _, LF, Num), !}.
iv(finite, LF, Num) --> [IV], {morph_atoms(IV, R), iv(_, R, _, _, _, _, _, _, LF, Num), !}.
iv(finite, LF, Num) --> [IV], {morph_atoms(IV, R), iv(_, _, R, _, _, _, _, _, LF, Num), !}.
iv(finite, LF, Num) --> [IV], {morph_atoms(IV, R), iv(_, _, _, R, _, _, _, _, LF, Num), !}.
iv(past_participle, LF, Num) --> [IV], {morph_atoms(IV, R), iv(_, _, _, _, R, _, _, _, LF, Num), !}.
iv(past_participle, LF, Num) --> [IV], {morph_atoms(IV, R), iv(_, _, _, _, _, R, _, _, LF, Num), !}.
iv(past_participle, LF, Num) --> [IV], {morph_atoms(IV, R), iv(_, _, _, _, _, _, R, _, LF, Num), !}.
iv(pres_participle, LF, Num) --> [IV], {morph_atoms(IV, R), iv(_, _, _, _, _, _, _, R, LF, Num), !}.

tv(nonfinite, LF, Num) --> [TV], {morph_atoms(TV, R), tv(R, _, _, _, _, _, _, _, LF, Num), !}.
tv(finite, LF, Num) --> [TV], {morph_atoms(TV, R), tv(R, _, _, _, _, _, _, _, LF, Num), !}.
tv(finite, LF, Num) --> [TV], {morph_atoms(TV, R), tv(_, R, _, _, _, _, _, _, LF, Num), !}.
tv(finite, LF, Num) --> [TV], {morph_atoms(TV, R), tv(_, _, R, _, _, _, _, _, LF, Num), !}.
tv(finite, LF, Num) --> [TV], {morph_atoms(TV, R), tv(_, _, _, R, _, _, _, _, LF, Num), !}.
tv(past_participle, LF, Num) --> [TV], {morph_atoms(TV, R), tv(_, _, _, _, R, _, _, _, LF, Num), !}.
tv(past_participle, LF, Num) --> [TV], {morph_atoms(TV, R), tv(_, _, _, _, _, R, _, _, LF, Num), !}.
tv(past_participle, LF, Num) --> [TV], {morph_atoms(TV, R), tv(_, _, _, _, _, _, R, _, LF, Num), !}.
tv(pres_participle, LF, Num) --> [TV], {morph_atoms(TV, R), tv(_, _, _, _, _, _, _, R, LF, Num), !}.

rov(nonfinite /Requires, LF)
		--> [ROV], {rov(ROV, _, _, _, _, LF, Requires)}.
rov(finite /Requires, LF)
		--> [ROV], {rov(_, ROV, _, _, _, LF, Requires)}.
rov(finite /Requires, LF)
		--> [ROV], {rov(_, _, ROV, _, _, LF, Requires)}.
rov(past_participle/Requires, LF)
		--> [ROV], {rov(_, _, _, ROV, _, LF, Requires)}.
rov(pres_participle/Requires, LF)
		--> [ROV], {rov(_, _, _, _, ROV, LF, Requires)}.


/*-----------------------------------------------------
					Lexical Items
-----------------------------------------------------*/

relpron( that ).
relpron( who ).
relpron( whom ).

whpron( who ).
whpron( whom ).
whpron( what ).

det( every, (X^S1)^(X^S2)^ all(X,S1=>S2), sg ).
det( a, (X^S1)^(X^S2)^exists(X,S1&S2), sg ).
det( some, (X^S1)^(X^S2)^exists(X,S1&S2), sg ).
det( some, (X^S1)^(X^S2)^exists(X,S1&S2), pl ).

n(Word, Q, Num):- word(Word, Num, W), create_n(W, Q), assert(n(W, Q)), !.
create_n(W, X^ --Term):- Term =.. [W, X].
 
word(Word, pl, W) :-
   morph_atoms(Word,[[W,-es]]),
   s(_,_,W,n,_,_)
   ;
   fail.

word(Word, pl, W) :-
   morph_atoms(Word,[[W,-s]]),
   s(_,_,W,n,_,_)
   ;
   fail.

word(Word, pl, W) :-
   morph_atoms(Word,[[W,-pl]]),
   s(_,_,W,n,_,_)
   ;
   fail.


word(Word, sg, W) :-
   morph_atoms(Word,[[W]]),
   s(_,_,W,n,_,_)
   ;
   fail.

pn( bertrand, bertrand, sg, man).
pn( bill, bill, sg, woman ).
pn( gottlob, gottlob, sg, man ).
pn( firework, firework, sg, song).
pn( principia, principia, sg, book ).
pn( shrdlu, shrdlu, sg, program ).
pn( terry, terry, sg, woman ).


iv( no, [[W, -s]], [[W, -ed]], [[W, -past]],
	[[W, -ed]], [[W, -en]], 
	[[W, -past]], [[W, -ing]], FOL, sg):- s(SysID,W_Num,W,v,_,_), 
										  fr(SysID, FR, W_Num), 
										  sen_fol_iv(FR, W, FOL).

iv( [[W]], no, [[W, -ed]], [[W, -past]],
	[[W, -ed]], [[W, -en]],
	 [[W, -past]], [[W, -ing]], FOL, pl):- s(SysID,W_Num,W,v,_,_), 
										   fr(SysID, FR, W_Num), 
										   sen_fol_iv(FR, W, FOL).


tv( no, [[W, -s]], [[W, -ed]], [[W, -past]],
	[[W, -ed]], [[W, -en]], 
	[[W, -past]], [[W, -ing]], FOL, sg):- s(SysID,W_Num,W,v,_,_), 
										  fr(SysID, FR, W_Num), 
										  sen_fol_tv(FR, W, FOL).

tv( [[W]], no, [[W, -ed]], [[W, -past]],
	[[W, -ed]], [[W, -en]], 
	[[W, -past]], [[W, -ing]], FOL, pl):- s(SysID,W_Num,W,v,_,_), 
										  fr(SysID, FR, W_Num), 
										  sen_fol_tv(FR, W, FOL).


rov( want, wants, wanted,
	 wanted, wanting,
	% semantics is partial execution of
	% NP ^ VP ^ Y ^ NP( X^want(Y,X,VP(X)) )
	((X^ --want(Y,X,Comp))^S) ^ (X^Comp) ^ Y ^ S,
	% form of VP required:
	infinitival).

aux( to, 	infinitival/nonfinite, 	VP^ VP 		).
aux( does,  finite/nonfinite, 		VP^ VP 		).
aux( did, 	finite/nonfinite, 		VP^ VP 		).



/*=====================================================
				Auxiliary Predicates                   
=====================================================*/

%%% conc(List1, List2, List)
%%% ========================
%%%
%%% 	List1 ==> a list
%%% 	List2 ==> a list
%%% 	List <== the concatenation of the two lists
conc([], List, List).

conc([Element|Rest], List, [Element|LongRest]) :-
	conc(Rest, List, LongRest).


%%% read_sent(Words)
%%% ================
%%%
%%% 	Words ==> set of words read from the
%%% 			  standard input
%%%
%%% 	Words are delimited by spaces and the
%%% 	line is ended by a newline. Case is not
%%% 	folded; punctuation is not stripped.

read_sent(Words) :-
	get0(Char),						% prime the lookahead
	read_sent(Char, Words). 		% get the words

% Newlines end the input.
read_sent(C, []) :- newline(C), !.

% Spaces are ignored.
read_sent(C, Words) :- space(C), !,
	get0(Char),
	read_sent(Char, Words).

% Everything else starts a word.
read_sent(Char, [Word|Words]) :-
	read_word(Char, Chars, Next), 	% get the word
	name(Word, Chars),				% pack the characters
									% into an atom
	read_sent(Next, Words). 		% get some more words



%%% read_word(Chars)
%%% ================
%%%
%%% 	Chars ==> list of characters read from standard
%%% 			  input and delimited by spaces or
%%% 			  newlines
% Space and newline end a word.
read_word(C, [], C) :- space(C), !.
read_word(C, [], C) :- newline(C), !.


% All other chars are added to the list.
read_word(Char, [Char|Chars], Last) :-
	get0(Next),
	read_word(Next, Chars, Last).


%%% space(Char)
%%% ===========
%%%
%%% 	Char === the ASCII code for the space
%%% 			 character

space(32).


%%% newline(Char)
%%% =============
%%%
%%% 	Char === the ASCII code for the newline
%%% 			 character

newline(10).