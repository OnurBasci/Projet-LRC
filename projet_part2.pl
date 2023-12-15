deuxieme_etape(Abi,Abi1,Tbox) :-
    saisie_et_traitement_prop_a_demontrer(Abi,Abi1,Tbox).

saisie_et_traitement_prop_a_demontrer(Abi,Abi1,Tbox) :-
    nl,write('Entrez le numero du type de proposition que vous voulez demontrer :'),nl,
    write('1 Une instance donnee appartient a un concept donne.'),nl,
    write('2 Deux concepts n"ont pas d"elements en commun(ils ont une intersection vide).'),nl, read(R), suite(R,Abi,Abi1,Tbox).

suite(1,Abi,Abi1,Tbox) :-
    acquisition_prop_type1(Abi,Abi1,Tbox),!.

suite(2,Abi,Abi1,Tbox) :-
    acquisition_prop_type2(Abi,Abi1,Tbox),!.

suite(R,Abi,Abi1,Tbox) :-
    nl,write('Cette reponse est incorrecte.'),nl,
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
    concatenate(Abi, [(RI,Y), Abi1], Abi1).






