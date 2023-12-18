%predicats utils
delete_element(_, [], []).

delete_element(Element, [Element | Tail], Tail).

delete_element(Element, [Head | Tail], [Head | ResultTail]) :-
    delete_element(Element, Tail, ResultTail).


% Predicate to extract substring until the first opening parenthesis
get_type(Term, Substring) :-
    term_to_atom(Term, Atom),  % Convert the term to an atom
    atom_chars(Atom, AtomChars),  % Convert the atom to a list of characters
    get_substring_chars(AtomChars, SubstringChars),  % Get the substring characters
    atom_chars(Substring, SubstringChars).  % Convert the substring characters back to an atom

% Base case: Empty list
get_substring_chars([], []).

% Stop when an opening parenthesis is encountered
get_substring_chars(['('|_], []) :- !.

% Add the current character to the substring
get_substring_chars([Char|Rest], [Char|Substring]) :-
    get_substring_chars(Rest, Substring).



/*troisieme_etape(Abi,Abr) :-
    tri_Abox(Abi,Lie,Lpt,Li,Lu,Ls),
    resolution(Lie,Lpt,Li,Lu,Ls,Abr),
    nl,write('Youpiiiiii, on a demontre la
    proposition initiale !!!').*/


generer_random_Iname(RI) :-
    random(0,100000, RN),
    atom_concat('i', RN, RIS),
    term_string(RI, RIS).


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
    get_Ls(L, Init, Ls, Lie, Lpt, Li, Lu)); %soit C appartient à Lie, Lpt, Li ou Lu dans ce cas là on passe au prochain term.
    concatenate(Init, [(I, C)], Y), %sinon on l'ajoute dans Ls
    get_Ls(L, Y, Ls, Lie, Lpt, Li, Lu).




evolue((I, and(C1, C2)), Lie, Lpt, Li, Lu, Ls, Lie1, Lpt1, Li1, Lu1, Ls1) :-
    cnamea(C1), cnamea(C2),
    concatenate(Ls, [(I, C1)], LsC1),
    concatenate(LsC1, [(I, C2)], Ls1),
    Lie1 = Lie, Lpt1 = Lpt, Li1 = Li, Lu1 = Lu;
    get_type(C1, T), get_type(C2, T1),
    (T == and, T1 == and, %and and
    concatenate(Li, [(I, C1)], LiC1),
    concatenate(LiC1, [(I, C2)], Ls1);
    T == and, T1 == or, %and or
    concatenate(Li, [(I, C1)], Li1),
    concatenate(Lu, [(I, C2)], Lu1);
    T == or, T1 == and, %or and
    concatenate(Li, [(I, C2)], Li1),
    concatenate(Lu, [(I, C1)], Lu1);
    T == or, T1 == or, %and or
    concatenate(Li, [(I, C1)], Li1),
    concatenate(Lu, [(I, C2)], Lu1)

    ).

evolue_single((I, C), Lie, Lpt, Li, Lu, Ls, Lie1, Lpt1, Li1, Lu1, Ls1) :-
    get_type(C, T),
    (T == and,
    concatenate(Li, [(I, C1)], Li1));
    (T == or,
    concatenate(Li, [(I, C1)], Li1));
    (T == some,
    concatenate(Li, [(I, C1)], Li1));
    (T == all,
    concatenate(Li, [(I, C1)], Li1));
    (cnamea(C),
    concatenate(Li, [(I, C1)], Li1)).




evolue((I, C), Lie, Lpt, Li, Lu, Ls, Lie1, Lpt1, Li1, Lu1, Ls1) :-
    concatenate(Ls, [(I, C)], Ls1),
    Lie1 = Lie, Lpt1 = Lpt, Li1 = Li, Lu1 = Lu.
evolue((I, not(C)), Lie, Lpt, Li, Lu, Ls, Lie1, Lpt1, Li1, Lu1, Ls1) :-
    concatenate(Ls, [(I, not(C))], Ls1),
    Lie1 = Lie, Lpt1 = Lpt, Li1 = Li, Lu1 = Lu.



% predicat est vrai s'il y a une clash. i,e s'il existe un element a et
% son opposé not(a) dans Ls.

test_clash([(I, C) | L], Li) :-
    member((I, not(C)), Li); %not(C) est dans la liste d'instance, Clash
    test_clash(L, Li).



complete_some([(A, some(R, C)) | L], Lpt, Li, Lu, Ls, Abr) :-
    nl,write('Règle il existe'), nl,
    generer_random_Iname(B),
    evolue((B, C), L, Lpt, Li, Lu, Ls, Lie1, Lpt1, Li1, Lu1, Ls1),
    concatenate(Abr, [(A, B, R)], Abr1), %mettre à jour Abr
    (test_clash(Ls1, Ls1); %soit il y a un clash et on arête
    resolution(Lie1, Lpt1, Li1, Lu1, Ls1, Abr1) %sinon on continue la résolution
    ).


transformation_and(Lie, Lpt, [(A, and(C1, C2)) | L], Lu, Ls, Abr) :-
    nl,write('Règle et'), nl,
    evolue((A, and(C1, C2)) , Lie, Lpt, L, Lu, Ls, Lie1, Lpt1, Li1, Lu1, Ls1),
    (test_clash(Ls1, Ls1); %soit il y a un clash et on arête
    resolution(Lie1, Lpt1, Li1, Lu1, Ls1, Abr) %sinon on continue la résolution
    ).


transformation_or(Lie, Lpt, Li, [(A, or(C1, C2)) | L], Ls, Abr) :-
    nl,write('Règle or'), nl,
    (evolue((A, C1) , Lie, Lpt, Li, L, Ls, Lie1, Lpt1, Li1, Lu1, Ls1),
    (test_clash(Ls1, Ls1); %soit il y a un clash et on arête
    resolution(Lie1, Lpt1, Li1, Lu1, Ls1, Abr) %sinon on continue la résolution
    ),%premier branche
    evolue((A, C2) , Lie, Lpt, Li, L, Ls, Lie2, Lpt2, Li2, Lu2, Ls2),
    (test_clash(Ls2, Ls2); %soit il y a un clash et on arête
    resolution(Lie2, Lpt2, Li2, Lu2, Ls2, Abr) %sinon on continue la résolution
    )).%second branche


deduction_all(Lie, [(A, all(R, C)) | L], Li,Lu,Ls,Abr) :-
    nl,write('Règle pour tout'), nl,
    get_a_b_R(A, R, Abr, [], La_b_R), %list contenant les elements de types (a, b) : R
    put_bc_for_all(C, La_b_R, L, Ls1),
    (test_clash(Ls1, Ls1); %soit il y a un clash et on arête
    resolution(Lie, L, Li, Lu, Ls1, br) %sinon on continue la résolution
    ).

%ce prédicat nous donne toutes les instances de type (a,b):R
get_a_b_R(_, _, [], Init, Lfinal) :- Lfinal = Init.
get_a_b_R(A, R,[(A1, B1, R1) | L], Init, Lfinal) :-
    A == A1, R == R1, %soit A, B, R est dans la liste
    concatenate(Init, [(A,B1,R)], L1),
    get_a_b_R(A, R, L, L1, Lfinal);
    get_a_b_R(A, R, L, Init, Lfinal). % sinon on vérifie le prochain élement

%ce prédicat ajoute b: C pour chaque element de type (a, b, R)
put_bc_for_all(_, [], Ls, L_final) :- L_final = Ls.
put_bc_for_all(C, [(_, B, _) | L], Ls, Ls_final) :-
    evolue((B, C), L, _, _, _, Ls, _, _, _, _, Ls1),
    deduction_for_all(C, L, Ls1, Ls_final).




resolution(Lie, Lpt, Li, Lu, Ls, Abr) :-
    complete_some(Lie, Lpt, Li, Lu, Ls, Abr);
    transformation_and(Lie, Lpt, Li, Lu, Ls, Abr);
    deduction_all(Lie, Lpt, Li, Lu, Ls, Abr);
    transformation_or(Lie, Lpt, Li, Lu, Ls, Abr).




















