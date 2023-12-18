premiere_etape(Tboxt, Abit, Abr) :-
    setof((CA, CG), equiv(CA, CG), Tbox),       % R�cup�ration de la TBoxt
    setof((I1, I2), inst(I1, I2), Abi),         % R�cup�ration de Abit
    setof((I1, I2, R), instR(I1, I2, R), Abr),   % R�cup�ration de Abrt

    nl, write("Tbox normal: "), write(Tbox),
    nl, write("Abox I normal: "), write(Abi),
    nl, write("Abox R normal: "), write(Abr),


    % V�rification de la Tbox
    write('[LOG] V�rification de la TBox ...'), nl,
    (verif_Tbox(Tbox) ->
        write('[LOG] V�rification de la TBox r�ussi'), nl;
        write('[ERREUR] Il y a erreur de syntaxe dans la TBox'), nl),

    % V�rification de la Abox
    write('[LOG] V�rification de la ABox ...'), nl,
    (verif_Abox(Abi,Abr) ->
        write('[LOG] V�rification de la ABox r�ussi'), nl;
        write('[ERREUR] Il y a erreur de syntaxe dans la ABox'), nl),

    % V�rification des auto-r�f�rencements
    setof(X, cnamena(X), Lcc),    % R�cup�ration de la liste des concepts non atomiques
    (\+verif_Autoref(Lcc) ->
        write('[LOG] Il n\'y a pas auto-r�f�rencement dans la TBox'), nl ;
        write('[ERREUR] Il y a auto-r�f�rencement dans la TBox'), nl),

    write('[LOG] Traitement des Boxs'), nl,
    traitement_Tbox(Tbox, [], Tboxt),

    traitement_AboxI(Abi, [], Abit),

    write('[LOG] Transformation termin�e'),nl.

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

    premiere_etape(Tbox, Abi, Abr),             % Call de la premi�re partie
    deuxieme_etape(Abi,Abi1,Tbox),
    (troisieme_etape(Abi1,Abr)->
	write('Il y a une feuille ouverte : on n\'a pas pu d�montr� la proposition');
	write('Toutes les feuilles sont ferm�es : on a d�montr� la proposition')),nl,
    write('[LOG] Programme termin� !').
programme.















