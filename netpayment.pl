/* Prolog level code expressing the idea of payment netting */ 
:- use_module(library(clpfd)).
:- discontiguous relat_is/3.
:- discontiguous type_is/2.
:- discontiguous happens/2.
:- discontiguous happens/4.
:- discontiguous relat_is/6.

type_is(party, bank).
type_is(counterparty, bank).
type_is(party, corp).
type_is(counterparty, corp).

type_is(payment, p1).
type_is(payment, p2).
type_is(payment, p3).
type_is(payment, p4).

type_is(obligation, p1).
type_is(obligation, p2).
type_is(obligation, p3).
type_is(obligation, p4).

relat_is(denominated, p1, gbp).
relat_is(denominated, p2, gbp).
relat_is(denominated, p3, gbp).
relat_is(denominated, p4, usd).

relat_is(due, p1, 20110303).
relat_is(due, p2, 20110303).
relat_is(due, p3, 20110303).
relat_is(due, p4, 20110303).

specifies(t1, p1).
specifies(t2, p2).
specifies(t3, p3).
specifies(t1, p4).

relat_is(value, p1, 100, 20110303).
relat_is(value, p2, 200, 20110303).
relat_is(value, p3, 300, 20110303).
relat_is(value, p4, 400, 20110303).

relat_is(directed, p1, bank, corp).
relat_is(directed, p2, bank, corp).
relat_is(directed, p3, corp, bank).
relat_is(directed, p4, bank, corp).

relat_is(agreement, bank, corp, isda).
relat_is(agreement, corp, bank, isda).

relat_is(governed, t1, isda).
relat_is(governed, t2, isda).
relat_is(governed, t3, isda).

happens([p1, p2, p3], gbp, t1, 20110303).

%% this predicate links useful attributes of
% the payment entity together
owes(Party, Counterparty, Currency,
     Transaction, Date, Payment) :- 
    type_is(payment, Payment),
    type_is(party, Party),
    type_is(counterparty, Counterparty),
    specifies(Transaction, Payment),
    relat_is(denominated, Payment, Currency),
    relat_is(due, Payment, Date),
    relat_is(directed, Payment, Party, Counterparty),
    relat_is(governed, Transaction, isda),
    relat_is(agreement, Party, Counterparty, isda).

%?- owes(bank, corp, gbp, t1, 20110303, Payment).
%@ Payment = p1 ;
%@ Payment = p2 ;
%@ false.



%% ListOfPayments is a list of the IDs of relevant
% payments for a given party, ccy, date, transaction
relat_is(aggregatepayment, ListOfPayments, Party,
	 Currency, Date, Transaction) :-

    % compiles a list of payment IDs given
    % characteristics
    findall(Payment,
	    (
		owes(Party, _, Currency,
		     Transaction, Date, Payment)
	    ),
	    ListOfPayments).

%?- relat_is(aggregatepayment, ListOfPayments, bank,
%    gbp, 20110303, Transaction).
%@ ListOfPayments = [p1, p2].



%% AA is the sum of all payments from ListOfPayments
relat_is(aggregateamount, AA, ListOfPayments) :-
    findall(Amount,
	    (
		member(Payment, ListOfPayments),
		relat_is(value, Payment, Amount, _)
	    ),
	    ListOfAmounts),
    sum_list(ListOfAmounts, AA).

%?- relat_is(aggregateamount, AA, [p1, p2]).
%@ AA = 300.


%% NetPayment is a list all relevant payments IDs
% between party and counterparty on a date;
% Amount is the net amount to be paid by the party
% with bigger aggregate obligations
relat_is(netpayment, NetPayment, Party, Counterparty,
	 Currency, Date, Transaction, Amount) :-

    % types are required here to instantiate variables
    type_is(party, Party),
    type_is(party, Counterparty),
    Party \= Counterparty,
    
    relat_is(aggregatepayment, ListOfPayments1,
	     Party, Currency, Date, Transaction),
    relat_is(aggregateamount, A1, ListOfPayments1),

    relat_is(aggregatepayment, ListOfPayments2,
	     Counterparty, Currency, Date, Transaction),
    relat_is(aggregateamount, A2, ListOfPayments2),
    
    A1 #>= A2, 
    append(ListOfPayments1, ListOfPayments2, NetPayment),
    Amount #= A1 - A2.

%?-relat_is(netpayment, NetPayment, Party, Counterparty,
%	 gbp, 20110303, [t2,t3], Amount).
%@ NetPayment = [p1, p2, p3],
%@ Party = bank,
%@ Counterparty = corp,
%@ Amount = 0 .

%?-relat_is(netpayment, [p1, p2, p3], Party, Counterparty,
%	 gbp, 20110303, t1, Amount).

%?-relat_is(netpayment, [p1, p2, p3], Party, Counterparty,
%	 gbp, 20110303, t1, A).
%@ Party = bank,
%@ Counterparty = corp,
%@ A = 0 .



type_is(netpayment, NetPayment) :-
    owes(Party, Counterparty, Currency,
	 Transaction, Date, Payment),
    type_is(party, Party),
    type_is(counterparty, Counterparty),
    type_is(payment, Payment),
    relat_is(netpayment, NetPayment, Party, Counterparty,
	     Currency, Date,Transaction, _).
%?- type_is(netpayment, A).
%@ A = [p1, p2, p3] ;
%@ A = [p1, p2, p3] ;
%@ A = [p3, p1, p2] ;
%@ A = [p4] ;
%@ false.



relat_is(satisfied, Obligation, Date) :-
  type_is(obligation, Obligation),
  type_is(payment, Obligation),
  relat_is(due, Obligation, Date),  
  relat_is(netpayment, NetPayment, _, _,
	 Currency, Date, Transaction, _),
  member(Obligation, NetPayment),
  happens(NetPayment, Currency, Transaction, Date).

%?- relat_is(satisfied, p3, 20110303).
%@ true .

happens(NetPayment, Currency, Transaction, Date) :-
    type_is(netpayment, NetPayment),
    relat_is(netpayment, NetPayment, Party, Counterparty,
	     Currency, Date, Transaction, Amount),
    transfers(Party, Amount, Account, Date),
    transfers(customary, way, Party, Amount, Currency),
    transfers(freely, transferable, funds, Party, Amount),
    relat_is(correctly, specified, Account, Counterparty,
	     Transaction, Payment),
    specifies(Transaction, Payment),
    relat_is(Transaction, Party, Counterparty), 
    relat_is(governed, Transaction, isdaAgreement).


relat_is(aggregatepayment, ListOfPayments, Party,
	 Currency, Date, Transaction) :-

    % compiles a list of payment IDs given
    % characteristics
    findall(Payment,
	    (
		owes(Party, _, Currency,
		     Transaction, Date, Payment)
	    ),
	    ListOfPayments).

%?- relat_is(aggregatepayment, ListOfPayments, bank,
%    gbp, 20110303, Transaction).
%@ ListOfPayments = [p1, p2].




%?-relat_is(netpayment, NetPayment, Party, Counterparty,
%	 gbp, 20110303, [t3], Amount).

relat_is(aggregatepayment, designated, ListOfPayments, Party,
	 Currency, Date, LT) :-

    % compiles a list of payment IDs given
    % characteristics
    findall(T,
	    (
		type_is(designated, T)
	    ),
	    LT),

    findall(Payment,
	    (
		member(T, LT),
		owes(Party, _, Currency,
		     T, Date, Payment)
	    ),
	    ListOfPayments).
type_is(designated, t2).
type_is(designated, t3).



relat_is(netpayment, designated, NetPayment, Party, Counterparty,
	 Currency, Date, Transaction, Amount) :-

    % types are required here to instantiate variables
    type_is(party, Party),
    type_is(party, Counterparty),
    Party \= Counterparty,
    
    relat_is(designated, ListOfPayments1,
	     Party, Currency, Date, Transaction),
    relat_is(aggregateamount, A1, ListOfPayments1),

    relat_is(designated, ListOfPayments2,
	     Counterparty, Currency, Date, Transaction),
    relat_is(aggregateamount, A2, ListOfPayments2),
    
    A1 #>= A2, 
    append(ListOfPayments1, ListOfPayments2, NetPayment),
    Amount #= A1 - A2.

%?-relat_is(anetpayment, NetPayment, Party, Counterparty,
%	 gbp, 20110303, [t2,t3], Amount).
%@ NetPayment = [p3, p2],
%@ Party = corp,
%@ Counterparty = bank,
%@ Amount = 100 .
















