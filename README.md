# MyTalk

### What is MyTalk

<i>MyTalk</i> is a Prolog tool for NLP, based on WordNet lexical database, for the English language. It is an extension of the <i>Talk</i> program, presented in (Pereira and Shieber, 1987). First of all, just as <i>Talk</i>, <i>MyTalk</i> is able to:
<ul>
  <li>perform the syntactic parsing of a declarative sentence or of a query, returning as result the corresponding logical form (FOL);</li>
  <li>transform the obtained logical form into a Horn clause (if possible)</li>
  <li>add the clause to the Prolog database (declarative sentence) or obtain the response to the query</li>
</ul>

In addition, MyTalk:
 <ul>
  <li>performs a morphological analysis of the words, based on the ProNTo_Morph tool, which checks the concordance in number between subject and verb of a sentence;</li>
  <li>instead of using a fixed lexicon cabled within the program, it uses the WordNet&copy; lexical database to check if a word exists;</li>
  <li>supports the usage of common nouns as subjects and is able to handle a wider range of input assertions and queries. It is also able to transform a wider range of FOLs into Horn clauses.
  <li>uses the hypernym and antonym WordNet&copy; relationships format to define a demo ontology, used to perform a demo semantic check on input assertions. Such ontology is based on the considerations presented in the Chapter 6 of (Blackburn and Bos, 2005).
</ul>

The access to WordNet is partially inspired to the files defined by the WordNet integration ProNTo tool, which was an useful source of inspiration.

### References

Fundamental reading to understand and extend this work are:
<ul>
<li>Pereira, F. C., & Shieber, S. M. (1987). <i>Prolog and natural-language analysis<\i>. Microtome Publishing. 
  In particolare, è fondamentale la lettura del capitolo 4.</li>

<li>Blackburn, P., & Bos, J. (2005). Representation and inference for natural language. <i>A first course in computational semantics. CSLI<\i>.</li>

<li>Miller, G. A. (1995). WordNet: a lexical database for English. <i>Communications of the ACM<\i>, 38(11), 39-41.</li>

<li>Ribeiro, C. et al. (2004). INQUER: A WordNet-based Question-Answering Application. In LREC.</li>

<li>ProNTo, Prolog Natural Language Tools (http://ai1.ai.uga.edu/mc/pronto/).</li>

 
### How to use MyTalk

This program was tested using SWI-Prolog (http://www.swi-prolog.org/) , both versions 5.2.13 and 7.6.4. If you find any bug or want furhter information about this program, feel free to send me an e-mail.

The entry-point of the program is the file <i>myTalk.pl<\i>: open it with SWI-Prolog or consult it from the current environment. Type the predicate <i>main_loop.<\i>: the user interface is the same of the orginal Talk program.
