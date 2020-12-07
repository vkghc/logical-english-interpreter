# Logical English

Logical English is a proof of concept artificial language which is close enough to English to be readily understandable by people, and structured enough to lend itself to computer analysis. A version of this can be used to automate some reasoning on legal contracts. 

This repo contains details of the language (see pdf files), and a simple interpreter for it written in Prolog. To test it, you can load the example.pl file in a SWI-Prolog session. For example, typing

```
?- translate "A person succeeds if the person studies. Mary studies.".
```
in your SWI-Prolog session will analyse the English text and create logical predicates. You can make simple queries: 

```
?- succeeds(X). 
X = mary. 
```
