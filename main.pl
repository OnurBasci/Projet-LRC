%PART 1
%predicats utils
concatenate([], L, L).
concatenate([X|Rest1], L2, [X|Result]) :-
    concatenate(Rest1, L2, Result).

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




concept(C) :- cnamea(C).
concept(C) :- cnamena(C).

% On vérifie la grammaire de la logique ALC
concept(not(C)) :- concept(C).
concept(or(C1, C2)) :- concept(C1), concept(C2).
concept(and(C1, C2)) :- concept(C1), concept(C2).
concept(all(R, C)) :- rname(R), concept(C).
concept(some(R, C)) :- rname(R), concept(C).


instC(I, C) :- iname(I), concept(C).

% Pour la Tbox
definitionT(CNA, CNA2) :- cnamena(CNA), concept(CNA2).
verif_Tbox([(CNA, CNA2) | L]) :-
    definitionT(CNA, CNA2),
    verif_Tbox(L).
verif_Tbox([]).

%pour la Abox
verif_AboxI([(I, C) | L]) :-
    inst(I,C),
    verif_AboxI(L).
verif_AboxI([]).

verif_AboxR([(I1, I2, R) | L]) :-
    instR(I1, I2, R),
    verif_AboxR(L).
verif_AboxR([]).

verif_Abox(Abi, Abr) :-
    verif_AboxI(Abi),
    verif_AboxR(Abr).



% Vérification d'auto réference, on garde toutes les élements non
% atomique dans une liste et on vérifie si on rencontre 2 fois cet
% élement. Si c'est le cas alors autoref est vrai.
% L c'est pour obtenir les élements rencontrés

verif_Autoref([C | L]) :-
    equiv(C, E),
    (autoref(C, E, []);
    verif_Autoref(L)).

autoref(C, not(C2), L) :-
    autoref(C, C2, L).

autoref(C, and(C1, C2), L) :-
    autoref(C, C1, L); autoref(C, C2, L).

autoref(C, or(C1, C2), L) :-
    autoref(C, C1, L), autoref(C, C2, L).

autoref(C, all(_, C2), L) :-
    autoref(C, C2, L).

autoref(C, some(_, C2), L) :-
    autoref(C, C2, L).

autoref(C, C1, L) :-
    member(C, L);
    cnamena(C1), concatenate(L, [C1], L2), equiv(C1, Z), autoref(C, Z, L2).

pas_autoref(C, CG, L) :- \+ autoref(C, CG, L).


%Mettre sous forme négatif

nnf(not(and(C1,C2)),or(NC1,NC2)):- nnf(not(C1),NC1),nnf(not(C2),NC2),!.
nnf(not(or(C1,C2)),and(NC1,NC2)):- nnf(not(C1),NC1),
nnf(not(C2),NC2),!.
nnf(not(all(R,C)),some(R,NC)):- nnf(not(C),NC),!.
nnf(not(some(R,C)),all(R,NC)):- nnf(not(C),NC),!.
nnf(not(not(X)),Y):- nnf(X,Y),!.
nnf(not(X),not(X)):-!.
nnf(and(C1,C2),and(NC1,NC2)):- nnf(C1,NC1),nnf(C2,NC2),!.
nnf(or(C1,C2),or(NC1,NC2)):- nnf(C1,NC1), nnf(C2,NC2),!.
nnf(some(R,C),some(R,NC)):- nnf(C,NC),!.
nnf(all(R,C),all(R,NC)) :- nnf(C,NC),!.
nnf(X,X).

%Remplacer les concepts Génerales

atom_concate5(A1, A2, A3, A4, A5, X) :-
    atom_concat(A1, A2, Temp1),
    atom_concat(Temp1, A3, Temp2),
    atom_concat(Temp2, A4, Temp3),
    atom_concat(Temp3, A5, X).

atom_concate3(A1, A2, A3, X) :-
    atom_concat(A1, A2, Temp1),
    atom_concat(Temp1, A3, X).


forme_atomique(and(CG1, CG2), X) :-
    forme_atomique(CG1,S1),forme_atomique(CG2, S2),
    term_string(S1, S_1), term_string(S2, S_2),
    atom_concate5("and(", S_1,"," ,S_2,")", X1),
    term_string(X, X1).


forme_atomique(or(CG1, CG2), X) :-
    forme_atomique(CG1,S1),forme_atomique(CG2, S2),
    term_string(S1, S_1), term_string(S2, S_2),
    atom_concate5("or(", S_1,"," ,S_2,")", X1),
    term_string(X, X1).

forme_atomique(all(R, CG), X) :-
    forme_atomique(CG,S1),
    term_string(S1, S_1), term_string(R, S_2),
    atom_concate5("all(", S_2,"," ,S_1,")", X1),
    term_string(X, X1).

forme_atomique(some(R, CG), X) :-
    forme_atomique(CG,S1),
    term_string(S1, S_1), term_string(R, S_2),
    atom_concate5("some(", S_2,"," ,S_1,")", X1),
    term_string(X, X1).

forme_atomique(not(CG), X) :-
    forme_atomique(CG,S1),
    term_string(S1, S_1),
    atom_concate3("not(",S_1,")" , X1),
    term_string(X, X1).


forme_atomique(CG, X) :-
    cnamena(CG), equiv(CG, X); X = CG.


% CG concept complex, X resultat sous forme normale négatif avec des
% élements atomique
traitement_elem_complex(CG, X) :-
    forme_atomique(CG, Y),
    nnf(Y, X).

traitement_Tbox([], Init, L) :- L = Init.
traitement_Tbox([(CA, CG) | L], Init, Lfinal) :-
    pas_autoref(CA, CG, []), %check if there is not autoref
    traitement_elem_complex(CG, Y),
    concatenate(Init, [(CA, Y)], Lc),
    traitement_Tbox(L, Lc, Lfinal).


% cela marche de la même manière que Tbox comme les element de Abox
% d'instance est composé de (instance, element complex)
traitement_AboxI([], Init, L) :- L = Init.
traitement_AboxI([(CA, CG) | L], Init, Lfinal) :-
    pas_autoref(CA, CG, []), %check if there is not autoref
    traitement_elem_complex(CG, Y),
    concatenate(Init, [(CA, Y)], Lc),
    traitement_AboxI(L, Lc, Lfinal).


%PART 2


saisie_et_traitement_prop_a_demontrer(Abi,Abi1,Tbox) :-
    nl,write('Enter the number of the type of proposal you wish to demonstrate:'),nl,
    write('1 A given instance belongs to a given concept.'),nl,
    write('2 Two concepts have no elements in common (they have an empty intersection).'),nl, read(R), suite(R,Abi,Abi1,Tbox).

suite(1,Abi,Abi1,Tbox) :-
    acquisition_prop_type1(Abi,Abi1,Tbox),!.

suite(2,Abi,Abi1,Tbox) :-
    acquisition_prop_type2(Abi,Abi1,Tbox),!.

suite(R,Abi,Abi1,Tbox) :-
    nl,write('This answer is incorrect.'),nl,
    saisie_et_traitement_prop_a_demontrer(Abi,Abi1,Tbox).

get_prop1(I, C) :-
    write('Entrez I : '), nl,
    read(I),
    write('Entrez C : '), nl,
    read(C),
    instC(I, C).

get_prop2(C1, C2) :-
    write('Entrez C1 : '), nl,
    read(C1),
    write('Entrez C2 : '), nl,
    read(C2),
    concept(C1), concept(C2).



%on suppose que le Tbox est déja traité
acquisition_prop_type1(Abi, Abi1, Tbox) :-
    get_prop1(I, C),
    traitement_elem_complex(not(C), Y),
    concatenate(Abi, [(I, Y)], Abi1).


generer_random_Iname(RI) :-
    random(0,100000, RN),
    atom_concat('i', RN, RIS),
    term_string(RI, RIS).


acquisition_prop_type2(Abi,Abi1,Tbox) :-
    generer_random_Iname(RI),
    get_prop2(C1, C2),
    traitement_elem_complex(not(and(C1, C2)), Y),
    concatenate(Abi, [(RI,Y)], Abi1).


%PART 3

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
    evolue_single((I, C1), Lie, Lpt, Li, Lu, Ls, Liei, Lpti, Lii, Lui, Lsi),
    evolue_single((I, C2), Liei, Lpti, Lii, Lui, Lsi, Lie1, Lpt1, Li1, Lu1, Ls1).


evolue((I, C), Lie, Lpt, Li, Lu, Ls, Lie1, Lpt1, Li1, Lu1, Ls1) :-
    evolue_single((I, C), Lie, Lpt, Li, Lu, Ls, Lie1, Lpt1, Li1, Lu1, Ls1).


evolue_single((I, C), Lie, Lpt, Li, Lu, Ls, Lie1, Lpt1, Li1, Lu1, Ls1) :-
    (get_type(C, T), T == and,
    Lie1 = Lie, Lpt1 = Lpt, Lu1 = Lu, Ls1 = Ls,
    concatenate(Li, [(I, C)], Li1));
    (get_type(C, T), T == or,
    Lie1 = Lie, Lpt1 = Lpt, Li1 = Li, Ls1 = Ls,
    concatenate(Lu, [(I, C)], Lu1));
    (get_type(C, T), T == some,
    Li1 = Li, Lpt1 = Lpt, Lu1 = Lu, Ls1 = Ls,
    concatenate(Lie, [(I, C)], Lie1));
    (get_type(C, T), T == all,
    Lie1 = Lie, Li1 = Li, Lu1 = Lu, Ls1 = Ls,
    concatenate(Lpt, [(I, C)], Lpt1));
    (get_type(C, T), T == not,
    Lie1 = Lie, Li1 = Li, Lu1 = Lu, Lpt1 = Lpt,
    concatenate(Ls, [(I, C)], Ls1));
    (cnamea(C),
    Lie1 = Lie, Lpt1 = Lpt, Lu1 = Lu, Li1 = Li,
    concatenate(Ls, [(I, C)], Ls1)).




% predicat est vrai s'il y a une clash. i,e s'il existe un element a et
% son opposé not(a) dans Ls.

test_clash([(I, C) | L], Li) :-
    member((I, not(C)), Li); %not(C) est dans la liste d'instance, Clash
    test_clash(L, Li).


print_current_states(Lie, Lpt, Li, Lu, Ls) :-
    nl, write('current state'),nl,
    nl, write('Lie: '), write(Lie),nl,
    nl, write('Lpt: '), write(Lpt),nl,
    nl, write('Li: '), write(Li),nl,
    nl, write('Lu: '), write(Lu),nl,
    nl, write('Ls: '), write(Ls),nl.



complete_some([(A, some(R, C)) | L], Lpt, Li, Lu, Ls, Abr) :-
    nl,write('Règle il existe'), nl,
    generer_random_Iname(B), generer_random_Iname(Buffer),
    nl, write('before some '), write(Buffer),nl,
    concatenate([(A, some(R, C))], L, Lie),
    print_current_states(Lie, Lpt, Li, Lu, Ls),
    evolue((B, C), L, Lpt, Li, Lu, Ls, Lie1, Lpt1, Li1, Lu1, Ls1),
    concatenate(Abr, [(A, B, R)], Abr1), %mettre à jour Abr
    nl, write('after some'), write(Buffer), nl,
    print_current_states(Lie1, Lpt1, Li1, Lu1, Ls1),
    (test_clash(Ls1, Ls1); %soit il y a un clash et on arête
    resolution(Lie1, Lpt1, Li1, Lu1, Ls1, Abr1) %sinon on continue la résolution
    ).



transformation_and(Lie, Lpt, [(A, and(C1, C2)) | L], Lu, Ls, Abr) :-
    nl,write('Règle et'), nl,
    concatenate([(A, and(C1, C2))], L, Li),
    generer_random_Iname(Buffer),
    nl, write('before and'), write(Buffer), nl,
    print_current_states(Lie, Lpt, Li, Lu, Ls),
    evolue((A, and(C1, C2)) , Lie, Lpt, L, Lu, Ls, Lie1, Lpt1, Li1, Lu1, Ls1),
    nl, write('after and'),  write(Buffer), nl,
    print_current_states(Lie1, Lpt1, Li1, Lu1, Ls1),
    (test_clash(Ls1, Ls1); %soit il y a un clash et on arête
    resolution(Lie1, Lpt1, Li1, Lu1, Ls1, Abr) %sinon on continue la résolution
    ).


transformation_or(Lie, Lpt, Li, [(A, or(C1, C2)) | L], Ls, Abr) :-
    nl,write('Règle or'), nl,
    (evolue((A, C1) , Lie, Lpt, Li, L, Ls, Lie1, Lpt1, Li1, Lu1, Ls1),
     print_current_states(Lie1, Lpt1, Li1, Lu1, Ls1),
    (test_clash(Ls1, Ls1); %soit il y a un clash et on arête
    resolution(Lie1, Lpt1, Li1, Lu1, Ls1, Abr) %sinon on continue la résolution
    ),%premier branche
    evolue((A, C2) , Lie, Lpt, Li, L, Ls, Lie2, Lpt2, Li2, Lu2, Ls2),
    print_current_states(Lie2, Lpt2, Li2, Lu2, Ls2),
    (test_clash(Ls2, Ls2); %soit il y a un clash et on arête
    resolution(Lie2, Lpt2, Li2, Lu2, Ls2, Abr) %sinon on continue la résolution
    )).%second branche


deduction_all(Lie, [(A, all(R, C)) | L], Li,Lu,Ls,Abr) :-
    nl,write('Règle pour tout'), nl,
    get_a_b_R(A, R, Abr, [], La_b_R), %list contenant les elements de types (a, b) : R
    put_bc_for_all(C, La_b_R, L, Ls1),
    print_current_states(Lie, L, Li, Lu, Ls1),
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
    put_bc_for_all(C, L, Ls1, Ls_final).




resolution(Lie, Lpt, Li, Lu, Ls, Abr) :-
    complete_some(Lie, Lpt, Li, Lu, Ls, Abr);
    transformation_and(Lie, Lpt, Li, Lu, Ls, Abr);
    deduction_all(Lie, Lpt, Li, Lu, Ls, Abr);
    transformation_or(Lie, Lpt, Li, Lu, Ls, Abr).


% First step of the program
premiere_etape(Tboxt, Abit, Abr) :-
    % Retrieving TBox information
    setof((CA, CG), equiv(CA, CG), Tbox),
    % Retrieving Abox individuals
    setof((I1, I2), inst(I1, I2), Abi),
    % Retrieving Abox relationships
    setof((I1, I2, R), instR(I1, I2, R), Abr),

    % Displaying retrieved information
    nl, write('Normal TBox: '), write(Tbox),
    nl, write('Normal Abox Individuals: '), write(Abi),
    nl, write('Normal Abox Relationships: '), write(Abr),

    % Checking TBox syntax
    write('[LOG] Checking TBox syntax...'), nl,
    (verif_Tbox(Tbox) ->
        write('[LOG] TBox syntax verification successful'), nl;
        write('[ERROR] Syntax error in TBox'), nl),

    % Checking ABox syntax
    write('[LOG] Checking ABox syntax...'), nl,
    (verif_Abox(Abi, Abr) ->
        write('[LOG] ABox syntax verification successful'), nl;
        write('[ERROR] Syntax error in ABox'), nl),

    % Checking for self-references
    setof(X, cnamena(X), Lcc),    % Retrieving the list of non-atomic concepts
    (\+verif_Autoref(Lcc) ->
        write('[LOG] No self-references in TBox'), nl ;
        write('[ERROR] Self-references detected in TBox'), nl),

    % Processing Boxes
    write('[LOG] Processing Boxes...'), nl,
    traitement_Tbox(Tbox, [], Tboxt),
    traitement_AboxI(Abi, [], Abit),

    % Transformation completed
    write('[LOG] Transformation completed'), nl.

% Second step of the program
deuxieme_etape(Abi, Abi1, Tbox) :-
    saisie_et_traitement_prop_a_demontrer(Abi, Abi1, Tbox).

% Third step of the program
troisieme_etape(Abi, Abr) :-
    % Displaying Abox Individuals and Relationships
    nl, write('Abox Individuals: '), nl, write(Abi),
    nl, write('Abox Relationships: '), nl, write(Abr),

    % Sorting Abox
    tri_Abox(Abi, Lie, Lpt, Li, Lu, Ls),

    % Displaying separator
    write('====================='), nl, nl,

    % Resolution
    resolution(Lie, Lpt, Li, Lu, Ls, Abr).

% Main program
main_programme :-
    % Calling the first step
    premiere_etape(Tbox, Abi, Abr),

    % Calling the second step
    deuxieme_etape(Abi, Abi1, Tbox),

    % Calling the third step
    (troisieme_etape(Abi1, Abr) ->
        write('There is an open leaf: The proposition could not be proven.');
        write('All leaves are closed: The proposition has been proven.')), nl,

    % Displaying completion message
    write('[LOG] Program completed!').

% Running the program
main_programme.



