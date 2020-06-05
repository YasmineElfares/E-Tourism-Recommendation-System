offerMean(offer(dahab, [diving, snorkeling, horseRiding], 10000, 2020-02-12, 2020-03-12,
period(2020-03-15, 2020-04-15), 10, 5), bus).
offerMean(offer(taba, [diving], 1000, 2020-02-12, 2020-03-12, period(2020-06-01, 2020-08-31), 10, 1),
bus).
offerAccommodation(offer(dahab, [diving, snorkeling, horseRiding], 10000, 2020-02-12, 2020-03-12,
period(2020-03-15, 2020-04-15), 10, 5), hotel).
offerAccommodation(offer(taba, [diving], 1000, 2020-02-12, 2020-03-12, period(2020-06-01, 2020-08-31),
10, 1), cabin).
customerPreferredActivity(customer(ahmed, aly, 1993-01-30, single, 0, student), diving, 100).
customerPreferredActivity(customer(ahmed, aly, 1993-01-30, single, 0, student), snorkeling, 100).
customerPreferredActivity(customer(ahmed, aly, 1993-01-30, single, 0, student), horseRiding, 20).
customerPreferredActivity(customer(mohamed, elkasad, 1999-01-30, single, 0, student), snorkeling, 60).
customerPreferredActivity(customer(mohamed, elkasad, 1999-01-30, single, 0, student), diving, 20).
customerPreferredActivity(customer(mohamed, elkasad, 1999-01-30, single, 0, student), horseRiding,
50).
customerPreferredMean(customer(ahmed, aly, 1993-01-30, single, 0, student), bus, 100).
customerPreferredMean(customer(mohamed, elkasad, 1999-01-30, single, 0, student), bus, 10).
customerPreferredAccommodation(customer(ahmed, aly, 1993-01-30, single, 0, student), hotel, 20).
customerPreferredAccommodation(customer(ahmed, aly, 1993-01-30, single, 0, student), cabin, 50).
customerPreferredAccommodation(customer(mohamed, elkasad, 1999-01-30, single, 0, student), hotel,
100).
customerPreferredAccommodation(customer(mohamed, elkasad, 1999-01-30, single, 0, student), cabin,
79).


  
  
subset4([],[]).
subset4([E|Tail], [E|NTail]):-
  subset4(Tail, NTail).
subset4([_|Tail], NTail):-
  subset4(Tail, NTail).


 possibleSubset(Set, SubSet) :-
  length(Set, LSet),
  between(0,LSet, LSubSet),
  length(NSubSet, LSubSet),
  permutation(NSubSet,SubSet),
  subset4(Set,NSubSet).
  
  
choosePreferences([],[]).
  
choosePreferences(L,ChosenPreferences):-

	member(activity([X|T]),L),
	delete(L,activity([X|T]),R),
	possibleSubset([X|T],Rsub),
	((Rsub=[])->J=[];J=[activity(Rsub)]),
	possibleSubset(R,R1),
	
	append(R1,J,ChosenPreferences).
	
choosePreferences(L,ChosenPreferences):-
	\+member(activity([_|_]),L),
	possibleSubset(L,ChosenPreferences).
	
	

	
	

preferenceSatisfactionh(_,_,[],0).
	
preferenceSatisfactionh(O,Customer,ChosenPrefs,S):-
   
   member(means(M),ChosenPrefs),
   offerMean(O,M),
   customerPreferredMean(Customer,M,S).
 
preferenceSatisfactionh(O,Customer,ChosenPrefs,S):-   
   
   member(accommodation(A),ChosenPrefs),
   offerAccommodation(O,A),
   customerPreferredAccommodation(Customer,A,S).
   
   
 preferenceSatisfactionh(_,Customer,ChosenPrefs,S):-   
   
   member(activity([X1|T1]),ChosenPrefs),
   member(Elem,[X1|T1]),
   setof(S1,customerPreferredActivity(Customer,Elem,S1),L),
   sumlist(L,S).
   
     
 preferenceSatisfaction(O,Customer,ChosenPrefs,S):-
 
  (setof(S1,preferenceSatisfactionh(O,Customer,ChosenPrefs,S1),L))->sumlist(L,S); S=0.
   

  

 
 
  %(StartA <= EndB) and (EndA >= StartB)    
										     
  
  overlapPeriod(period(A,B),period(C,D)):-
    B@>=C, A@=<D.

  


  
 getOffer(ChosenPrefs,Offer):-
    %means
    offerMean(offer(Destination, Activities,Cost,A2,B2,period(C,D),C2,D2),M),
    ((member(means(M1),ChosenPrefs))->M1 = M; M=_),
    %accommodation
    offerAccommodation(offer(Destination, _,Cost,A2,B2,period(C,D),C2,D2),Y),
    ((member(accommodation(Y1),ChosenPrefs))->Y1=Y;Y=_),
    %activity
	((member(activity(X4),ChosenPrefs))->possibleSubset(Activities,X4); _ = _),
	%period
	((member(period(P11,P22),ChosenPrefs))->(P11=P1,P22=P2);(C = P1,D = P2)),
	overlapPeriod(period(P1,P2),period(C,D)),
	%destination
	((member(dest(D55),ChosenPrefs))-> Destination=D55 ;_ = Destination),
	%budget	
	((member(budget(Budget),ChosenPrefs))-> _=_;Cost = Budget),
	Cost =< Budget,
	Offer = offer(Destination, Activities,Cost,A2,B2,period(C,D),C2,D2).
	
	
recommendOfferForCustomer(Prefs, ChosenPrefs, O):-
   choosePreferences(Prefs,ChosenPrefs),
   getOffer(ChosenPrefs,O).
   
  
recommendOffer([Customer|T], [CustPref|Y], Offer, CustomersChosen):-
  offerMean(offer(A, B, C, D, E, F, G, N),_),
  Offer=offer(A, B, C, D, E, F, G, N),
recommendOfferHelp([Customer|T], [CustPref|Y], Offer,List,Numbers),
recommendOfferHelp2(CustomersChosen,List,Numbers,N).
 

recommendOfferHelp([], [],_,[],[]).
 
recommendOfferHelp([Customer|T], [CustPref|Y],Offer,List,Numbers):-
 setof(ChosenPrefs,recommendOfferForCustomer(CustPref, ChosenPrefs, Offer),[Ch|Mo]),
 oneCustSat(Offer,Customer,[Ch|Mo],_,S),
 recommendOfferHelp(T, Y, Offer,List1,Numbers1),
 List=[rating(Customer,S)|List1],
 Numbers=[S|Numbers1].

oneCustSat(_,_,[],[],0).
oneCustSat(Offer,Customer,[Ch|Mo],R,S):-
  preferenceSatisfaction(Offer,Customer,Ch,S1),
  oneCustSat(Offer,Customer,Mo,R1,_),
  R=[S1|R1],
  max_list(R,S).

 recommendOfferHelp2([],[],[],_).
 recommendOfferHelp2([],_,_,0).
 
 recommendOfferHelp2(CustomersChosen,[rating(X,R)|T],Numbers,N):-
 N>0,
 max_list(Numbers,Max),
 delete2(Max,Numbers,N1),
 member(rating(Cust,Max),[rating(X,R)|T]),
 Curr=rating(Cust,Max),
 delete2(Curr,[rating(X,R)|T],R1),
 N54 is N-1,
 recommendOfferHelp2(CustomersChosen1,R1,N1,N54),
 CustomersChosen=[Cust|CustomersChosen1].

 
delete2(X,[X|T],T):-!. %Break
delete2(X,[Y|T],[Y|T1]):-delete2(X,T,T1).

