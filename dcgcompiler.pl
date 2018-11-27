/******************************************************
					
					DCG Compiler

******************************************************/


/*=====================================================
				Operator Declarations
=====================================================*/
:- op(1200,xfx,--->).


%%% These declarations are required by certain Prolog
%%% systems for predicates that are to be asserted
%%% at run-time. Predicates are specified by terms
%%% of the form name/arity.

:- dynamic (--->)/2, parse/3, connect/3.

/*======================================================
					Compiler Driver
======================================================*/
%%% compile
%%% =======
%%%
%%% 		Generates compiled clauses by partial
%%% 		execution of the DCG metainterpreter below,
%%% 		and adds them to the Prolog database.

compile :-
	program_clause(Clause),
	partially_execute(Clause, CompiledClause),
	add_rule(CompiledClause),
	fail.

%%% add_rule(Clause)
%%% ================
%%%
%%% 	Clause ==> clause to be added to database
%%% 			   after rewriting into a normal
%%% 			   form that changes calls to parse
%%% 			   into calls on particular
%%% 			   nonterminals.

add_rule((Head :- Body)) :-
	rewrite(Head, NewHead),
	rewrite(Body, NewBody),
	write('Asserting "'),
	write((NewHead :- NewBody)),
	write('."'), nl,
	assert((NewHead :- NewBody)).


%%% rewrite(Term, NewTerm)
%%% ======================
%%%
%%% 	Term ==> a term encoding a literal or
%%% 			 sequence of literals
%%% 	NewTerm <== the term rewritten so literals
%%% 				of the form
%%% 					parse(s(...),...)
%%% 				are rewritten into the form
%%% 					s(...,...)

rewrite((A,B), (C,D)) :- !,
	rewrite(A, C), rewrite(B, D).
rewrite(parse(Term, P1, P2), NewLiteral) :- !,
	Term =.. [Function|Args],
	conc(Args, [P1, P2], AllArgs),
	NewLiteral =.. [Function|AllArgs].
rewrite(Term,Term).


/*=====================================================
		Partial Execution of Prolog Programs
=====================================================*/
%%% partially_execute(Term, NewTerm)
%%% ================================
%%%
%%% 	Term ==> term encoding Prolog clause,
%%% 			 literal list or literal to be
%%% 		partially executed with respect to the
%%% 		program clauses and auxiliary clauses
%%% 		given by program_clause and clause
%%% 		predicates respectively.
%%%
%%% 	NewTerm <== the partially executed term.

% Partially executing a clause involves
% expanding the body.
partially_execute((Head:-Body),
		(Head:-ExpandedBody)) :- !,
	partially_execute(Body, ExpandedBody).

% Partially expanding a literal list involves
% conjoining the respective expansions.
partially_execute((Literal, Rest), Expansion) :- !,
	% expand the first literal
	partially_execute(Literal, ExpandedLiteral),
	% and the rest of them
	partially_execute(Rest, ExpandedRest),
	% and conjoin the results
	conjoin(ExpandedLiteral, ExpandedRest, Expansion).

% Partially executing an auxiliary literal involves
% replacing it with the body of a matching clause (if
% there are any). Nonauxiliary literals, or those
% not matching any clauses, are left unchanged.
partially_execute(Literal, Expansion) :-
	( aux_literal(Literal),
		setof(Body, Literal^aclause((Literal :- Body)),
				[_Clause|_Others]) )
	-> ( aclause((Literal :- Body)),
		 partially_execute(Body, Expansion) )
	; Expansion = Literal.


/*-----------------------------------------------------
						Utilities
-----------------------------------------------------*/
%%% conc(List1, List2, List)
%%% ========================
%%%
%%% 		List1 ==> a list
%%% 		List2 ==> a list
%%% 		List <== the concatenation of the two lists

conc([], List, List).

conc([Element|Rest], List, [Element|LongRest]) :-
	conc(Rest, List, LongRest).


%%% conjoin(Conjunct1, Conjunct2, Conjunction)
%%% ==========================================
%%%
%%% 	Conjunct1 ==> two terms to be conjoined
%%% 	Conjunct2 ==>
%%% 	Conjunction <== result of the conjunction

% Conjoining a conjunction works just like
% concatenation (conc).
conjoin((A,B), C, ABC) :- !,
	conjoin(B, C, BC),
	conjoin(A, BC, ABC).

% Conjoining true and anything leaves the other
% conjunct unchanged.
conjoin(true, A, A) :- !.
conjoin(A, true, A) :- !.

% Otherwise, use the normal comma conjunction
% operator.
conjoin(A, C, (A,C)).



%%% aclause(Clause)
%%% ===================
%%%
%%% 	Clause <== the head and body of a clause
%%% 				encoded with the unary ‘clause’;
%%% 				unit clauses can be encoded directly
%%% 				with clause and the Body returned will
%%% 				be ‘true’. Furthermore, the top-to-
%%% 				bottom clause ordering is preserved.

aclause((Head :- Body)) :-
	clause(Clause),
	(Clause = (Head :- Body)
		-> true
		; (Clause = Head, Body = true)).



/*=====================================================
			Program to Partially Execute
=====================================================*/

/*-----------------------------------------------------
Control Information for Partial Executor
-----------------------------------------------------*/
aux_literal( (_ ---> _) ).
aux_literal( parse(_, _, _) ).


/*-----------------------------------------------------
	DCG Metainterpreter to be Partially Executed
		Encoded form of program in Section 6.3.1
-----------------------------------------------------*/
program_clause(( parse(NT, P_0, P) :-
					(NT ---> Body),
					parse(Body, P_0, P) )).

program_clause(( connect(Word, [Word|Rest], Rest) :-
true )).

clause(( 		parse((Body1, Body2), P_0, P) :-
					parse(Body1, P_0, P_1),
					parse(Body2, P_1, P) 			 )).

clause(( 		parse([], P, P)						 )).

clause(( 		parse([Word|Rest], P_0, P) :-
					connect(Word, P_0, P_1),
					parse(Rest, P_1, P) 			 )).

clause(( 		parse({Goals}, P, P) :- call(Goals)	 )).


/*=====================================================
						Operators
=====================================================*/

/*-------------------------------------------------
	Sample Data for Program to Partially Execute:
	The parse-tree building DCG of Program 3.11
-----------------------------------------------------*/
clause(( 		s(s(NP,VP)) ---> np(NP), vp(VP) 		)).
clause(( 		np(np(Det,N,Rel)) --->
					det(Det),
					n(N),
					optrel(Rel) 						)).
clause(( 		np(np(PN)) ---> pn(PN) 					)).
clause(( 		vp(vp(TV,NP)) ---> tv(TV), np(NP) 		)).
clause(( 		vp(vp(IV)) ---> iv(IV) 					)).
clause(( 		optrel(rel(epsilon)) ---> [] 			)).
clause(( 		optrel(rel(that,VP)) ---> [that], vp(VP))).
clause(( 		pn(pn(terry)) ---> [terry] 				)).
clause(( 		pn(pn(shrdlu)) ---> [shrdlu] 			)).
clause(( 		iv(iv(halts)) ---> [halts] 				)).
clause(( 		det(det(a)) ---> [a] 					)).
clause(( 		n(n(program)) ---> [program] 			)).
clause(( 		tv(tv(writes)) ---> [writes] 			)).