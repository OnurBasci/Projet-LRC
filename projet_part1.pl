%predicats utils
concatenate([], L, L).
concatenate([X|Rest1], L2, [X|Result]) :-
    concatenate(Rest1, L2, Result).


concept(C) :- cnamea(C).
concept(C) :- cnamena(C).

% On vérifie la grammaire de la logique ALC (sujet I.3)
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



% V�rification d'auto r�ference, on garde toutes les �lements non
% atomique dans une liste et on v�rifie si on rencontre 2 fois cet
% �lement. Si c'est le cas alors autoref est vrai.
% L c'est pour obtenir les �lements rencontr�s

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


%Mettre sous forme n�gatif

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

%Remplacer les concepts G�nerales

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


% CG concept complex, X resultat sous forme normale n�gatif avec des
% �lements atomique
traitement_elem_complex(CG, X) :-
    forme_atomique(CG, Y),
    nnf(Y, X).

traitement_Tbox([], Init, L) :- L = Init.
traitement_Tbox([(CA, CG) | L], Init, Lfinal) :-
    pas_autoref(CA, CG, []), %check if there is not autoref
    traitement_elem_complex(CG, Y),
    concatenate(Init, [(CA, Y)], Lc),
    traitement_Tbox(L, Lc, Lfinal).


% cela marche de la m�me mani�re que Tbox comme les element de Abox
% d'instance est compos� de (instance, element complex)
traitement_AboxI([], Init, L) :- L = Init.
traitement_AboxI([(CA, CG) | L], Init, Lfinal) :-
    pas_autoref(CA, CG, []), %check if there is not autoref
    traitement_elem_complex(CG, Y),
    concatenate(Init, [(CA, Y)], Lc),
    traitement_AboxI(L, Lc, Lfinal).




















