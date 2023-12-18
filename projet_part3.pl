generer_random_Iname(RI) :-
    random(0,100000, RN),
    atom_concat('i', RN, RIS),
    term_string(RI, RIS).


troisieme_etape(Abi, Abr) :-
    tri_Abox(Abi, Lie, Lpt, Li, Lu, Ls),
    resolution(Lie,Lpt,Li,Lu,Ls,Abr),
    nl,write('Youpiiiiii, on a demontre la proposition initiale !!!').


tri_Abox(Abi, Lie, Lpt, Li, Lu, Ls) :-
    get_Lie(Abi, [], Lie),
    get_Lpt(Abi, [], Lpt),
    get_Li(Abi, [], Li),
    get_Lu(Abi, [], Lu),
    get_Ls(Abi, [], Ls, Lie, Lpt, Li, Lu).


get_Lie([], Init,Lie) :- Lie = Init.
get_Lie([(I, some(R, C)) | L], Init, Lie) :-
    concatenate(Init, [(I, some(R, C))], Y),
    get_Lie(L, Y, Lie).


get_Lie([(_, _) | L], Init, Lie) :-
    get_Lie(L, Init, Lie).


get_Lpt([], Init,Lpt) :- Lpt = Init.
get_Lpt([(I, all(R, C)) | L], Init, Lpt) :-
    concatenate(Init, [(I, all(R, C))], Y),
    get_Lpt(L, Y, Lpt).


get_Lpt([(_, _) | L], Init, Lpt) :-
    get_Lpt(L, Init, Lpt).


get_Li([], Init,Li) :- Li = Init.
get_Li([(I, and(C1, C2)) | L], Init, Li) :-
    concatenate(Init, [(I, and(C1, C2))], Y),
    get_Li(L, Y, Li).


get_Li([(_, _) | L], Init, Li) :-
    get_Li(L, Init, Li).


get_Lu([], Init,Lu) :- Lu = Init.
get_Lu([(I, or(C1, C2)) | L], Init, Lu) :-
    concatenate(Init, [(I, or(C1, C2))], Y),
    get_Lu(L, Y, Lu).


get_Lu([(_, _) | L], Init, Lu) :-
    get_Lu(L, Init, Lu).




get_Ls([], Init, Ls, Lie, Lpt, Li, Lu) :- Ls = Init.
get_Ls([(I, C) | L], Init, Ls, Lie, Lpt, Li, Lu) :-
    ((member((I, C), Lie);
    member((I, C), Lpt);
    member((I, C), Li);
    member((I, C), Lu)),
    get_Ls(L, Init, Ls, Lie, Lpt, Li, Lu)); %soit C appartient � Lie, Lpt, Li ou Lu dans ce cas l� on passe au prochain term.
    concatenate(Init, [(I, C)], Y), %sinon on l'ajoute dans Ls
    get_Ls(L, Y, Ls, Lie, Lpt, Li, Lu).


evolue((I, some(R, C)), Lie, Lpt, Li, Lu, Ls, Lie1, Lpt1, Li1, Lu1, Ls1) :-
    cocatenate(Lie, [(I, some(R, C))], Lie1).

evolue((I, and(C1, C2)), Lie, Lpt, Li, Lu, Ls, Lie1, Lpt1, Li1, Lu1, Ls1) :-
    cocatenate(Li, [(I, C1)], Li1),
    concatenat(Li, [(I, C2)], Li1).

evolue((I, or(C1, C2)), Lie, Lpt, Li, Lu, Ls, Lie1, Lpt1, Li1, Lu1, Ls1) :-
    cocatenate(Lu, [(I, or(C1, C2))], Lu1).
evolue((I, all(R, C)), Lie, Lpt, Li, Lu, Ls, Lie1, Lpt1, Li1, Lu1, Ls1) :-
    cocatenate(Lpt, [(I, all(R, C))], Lpt1).
evolue((I, C), Lie, Lpt, Li, Lu, Ls, Lie1, Lpt1, Li1, Lu1, Ls1) :-
    cocatenate(Ls, [(I, C)], Ls1).
evolue((I, not(C)), Lie, Lpt, Li, Lu, Ls, Lie1, Lpt1, Li1, Lu1, Ls1) :-
    cocatenate(Ls, [(I, not(C))], Ls1).


% predicat est vrai s'il y a une clash. i,e s'il existe un element a et
% son oppos� not(a) dans Ls.

test_clash([(_, C) | L], Li) :-
    member((_, not(C)), Li); %not(C) est dans la liste d'instance, Clash
    test_clash(L, Li).



complete_some([(A, some(R, C))], Lie, Lpt, Li, Lu, Ls, Abr) :-
    nl,write('R�gle il existe'), nl,
    generer_random_Iname(B),
    evolue((B, C), Lie, Lpt, Li, Lu, Ls, Lie1, Lpt1, Li1, Lu1, Ls1),
    concatenate(Abr, [(A, B, R)], Abr2), %mettre � jour Abr
    (test_clash(Ls, Ls); %soit il y a un clash et on ar�te
    resolution(Lie1, Lpt, Li, Lu, Ls, Abr2) %sinon on continue la r�solution
    ).


resolution(Lie, Lpt, Li, Lu, Ls, Abr).
    complete_some(Lie, Lpt, Li, Lu, Ls, Abr);
    transformation_and(Lie, Lpt, Li, Lu, Ls, Abr);
    deducation_all(Lie, Lpt, Li, Lu, Ls, Abr);
    transformation_or(Lie, Lpt, Li, Lu, Ls, Abr).









