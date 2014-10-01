c_numbers(N):-
	course(N,_,_).

c_pl(N):-
	course(N,programming_languages,_).

c_notpl(N):-
	course(N,U,_), U \= programming_languages.

c_inst60(L):-
	course(60,_,L).

c_inst60_sorted(L):-
	course(60,_,X), sort(X,L).

c_inst20(L):-
	course(20,_,L).

c_inst20_sorted(L):-
	course(20,_,X), sort(X,L).

c_inst_sorted(N,L):-
	course(N,_,X), sort(X,L).

c_single_inst(N):-
	course(N,_,[H|[]]), H \= [].

c_multi_inst(N):-
	course(N,_,[H|T]), H \= [], T \= [].

c_exclusive(I,N):-
	course(N,_,[I]).

c_12_inst_1or(N):-
	(
		course(N,_,[H|[]])
	;	course(N,_,[H|[Y|[]]]) 
	).

c_12_inst_2wo(N):-
	course(N,_,[H|[]]).

c_12_inst_2wo(N):-
	course(N,_,[H|[Y|[]]]).


delete_question("157").

sortappend(X,Y,Z):-
	append(X,Y,G), sort(G,Z).


distribute(W, [], []).
distribute(W,[H|T],Y):-
	distribute(W,T,R2),
	append([[W,H]],R2,Y).

myfor(L,U,Result) :-
	L=<U,
	L1 is L + 1,
	myfor(L1,U,Res1),
	Result = [L|Res1].
myfor(L,U,[]) :- L > U.

crossmyfor(X,Y,Z) :-
	myfor(1,X,R1),
	myfor(1,Y,R2),
	crossit(R1,R2,Z),!.
crossit([],[],[]).
crossit([],Y,[]).
crossit(X,[],[]).
crossit([H|T],Y,Z) :-
	T \= [],
	crossit(T,Y,Z1),
	distribute(H,Y,Z2),
	append(Z2,Z1,Z).
crossit([H|[]],Y,Z) :-
	distribute(H,Y,Z).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%5a
getallmeetings([],[]).
getallmeetings([H|T],Z):-
	getJob(H,L1),
	getallmeetings(T,X),
	append(L1,X,W),
	sort(W,Z).
getJob([H|T],H):- T = [].
getJob([H|T],Z):-getJob(T,Z).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%5b
checkMeeting(N,M1,[JH|_],N):-  %return the name if matches.
	M1=JH.
checkMeeting(N,M1,[],[]).
checkMeeting(N,M1,[JH|JT],X):-
	M1\=JH,
	checkMeeting(N,M1,JT,X).

getNamesOfSameMeeting([],M1,[]). %when the name list empty, return []
getNamesOfSameMeeting([[N|[J]]|CR],M1,Z4):-
	[N|[J]] \= [],      %J is the list of all meetings of the person. [XX,YY,ZZ]
	checkMeeting(N,M1,J,U),   
	getNamesOfSameMeeting(CR,M1,ZZ),  
	Z3=[U|ZZ],
	sort(Z3,Z4).
loopEachMeetings(C,[],[]).
loopEachMeetings(C,[M1|MR],R3):-
	getNamesOfSameMeeting(C,M1,J),
	delete(J,[],K),
	X=[M1,K],
	loopEachMeetings(C,MR,V),
	R3=[X|V].
participants([],[]):-!.
participants(C,R):-
	getallmeetings(C,X), %X has all meetings
	loopEachMeetings(C,X,R).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%5c
%
osched(_,_,[],[]).
osched(MR,MH,C,Z):- C\=[], getallmeetings(C,M), length(M,R), MR*MH =:= R, crossmyfor(MR,MH,R1), participants(C,R2),permutation(R2,R3), sched(R1,R3,Z).
osched(MR,MH,C,Z):- C\=[], getallmeetings(C,M), length(M,R), MR*MH > R, crossmyfor(MR,MH,R1), participants(C,R2),comb(R,R1,R3), sched2(R3,R2,Z).
sched2(R1,R3,Z):- permutation(R3,R4), sched(R1,R4,Z).
sched([X|T1],[Y|T2],Z) :- append([[X,Y]],R,Z), sched(T1,T2,R).
sched([],[],[]).
    
comb(0,_,[]).
comb(N,[X|T],[X|Comb]):-N>0,N1 is N-1,comb(N1,T,Comb).
comb(N,[_|T],Comb):-N>0,comb(N,T,Comb).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%5d

xsched(MR,MH,C,Z).







t1([H|[]],2).

