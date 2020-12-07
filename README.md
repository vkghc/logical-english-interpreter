# Logical English

Logical English is a proof-of-concept artificial language which is close enough to English to be readily understandable by people, and structured enough to lend itself to computer analysis. A version of it can be used to automate some reasoning on legal contracts. 

This repo contains details of the language (see pdf files), and a simple interpreter written in Prolog. To test the interpreter, you can load the *examples.pl* file in a SWI-Prolog session. You can then type simple Logical Enlgish sentences and analyse them with the ```trasnlate/1``` predicate. For example, typing:

```
?- translate "A person succeeds if the person studies. Mary studies.".
```
in your session will analyse the text in quotation marks and create logical predicates. You can then make simple queries: 

```
?- succeeds(X). 
X = mary. 
```
