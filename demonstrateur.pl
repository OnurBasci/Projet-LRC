premiere_etape(Tboxt, Abit, Abr) :-
    setof((CA, CG), equiv(CA, CG), Tbox),       % Récupération de la TBoxt
    setof((I1, I2), inst(I1, I2), Abi),         % Récupération de Abit
    setof((I1, I2, R), instR(I1, I2, R), Abr),   % Récupération de Abrt

    nl, write("Tbox normal: "), write(Tbox),
    nl, write("Abox I normal: "), write(Abi),
    nl, write("Abox R normal: "), write(Abr),


    % Vérification de la Tbox
    write('[LOG] Vérification de la TBox ...'), nl,
    (verif_Tbox(Tbox) ->
        write('[LOG] Vérification de la TBox réussi'), nl;
        write('[ERREUR] Il y a erreur de syntaxe dans la TBox'), nl),

    % Vérification de la Abox
    write('[LOG] Vérification de la ABox ...'), nl,
    (verif_Abox(Abi,Abr) ->
        write('[LOG] Vérification de la ABox réussi'), nl;
        write('[ERREUR] Il y a erreur de syntaxe dans la ABox'), nl),

    % Vérification des auto-référencements
    setof(X, cnamena(X), Lcc),    % Récupération de la liste des concepts non atomiques
    (\+verif_Autoref(Lcc) ->
        write('[LOG] Il n\'y a pas auto-référencement dans la TBox'), nl ;
        write('[ERREUR] Il y a auto-référencement dans la TBox'), nl),

    write('[LOG] Traitement des Boxs'), nl,
    traitement_Tbox(Tbox, [], Tboxt),

    traitement_AboxI(Abi, [], Abit),

    write('[LOG] Transformation terminée'),nl.

deuxieme_etape(Abi,Abi1,Tbox) :-
    saisie_et_traitement_prop_a_demontrer(Abi,Abi1,Tbox).

troisieme_etape(Abi,Abr) :-
    nl, write("Abi: "), nl, write(Abi),
    nl, write("Abr: "), nl, write(Abr),
    tri_Abox(Abi,Lie,Lpt,Li,Lu,Ls),
    write('====================='),nl,nl,
    resolution(Lie,Lpt,Li,Lu,Ls,Abr).

programme :-
    load_files('projet_part1.pl'),
    load_files('projet_part2.pl'),
    load_files('projet_part3.pl'),
    load_files('t_abox.pl'),
    % Test
    % load_files('./Test/T-A_Box_erreur_syntaxe1.pl'),
    % load_files('./Test/T-A_Box_erreur_syntaxe2.pl'),
    % load_files('./Test/T-A_Box_erreur_syntaxe3.pl'),
    % load_files('./Test/T-A_Box_erreur_cyclique.pl'),
    % load_files('./Test/T-A_Box_def_cmplx_a_dev_TBox.pl'),
    % load_files('./Test/T-A_Box_def_cmplx_a_dev_ABox.pl'),

    premiere_etape(Tbox, Abi, Abr),             % Call de la première partie
    deuxieme_etape(Abi,Abi1,Tbox),
    (troisieme_etape(Abi1,Abr)->
	write('Il y a une feuille ouverte : on n\'a pas pu démontré la proposition');
	write('Toutes les feuilles sont fermées : on a démontré la proposition')),nl,
    write('[LOG] Programme terminé !').
programme.















