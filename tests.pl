%PARTIE 1

%TEST TBOX
traitement_elem_complex_Tbox(not(and(or(sculpteur, editeur), auteur)), X).

traitement_Tbox([(sculpteur,and(personne,some(aCree,editeur)))], [], L).

%TEST AUTOREF
autoref(sculpteur, and(personne,some(aCree,editeur)), []). %retourne false car il n'y a pas d'autoref.

autoref(sculpteur, and(personne,some(aCree,sculpture)), []). %retourne vrai si les 3 lignes suivantes sont ajoutées dans t_abox
equiv(sculpture, and(objet, all(cree_par, sculpteur))).
cnamena(sculpture).
rname(cree_par).

% les mêmes tests peut se faire avec pas_autoref, resultat inverse est
% attendu

% si ces lignes ne sont pas ajouter, le prédicat retounrne false, car il
% n'y a pas d'autoref.

%TEST TRAITEMENT A_BOX
traitement_AboxI([(personne, sculpteur)], [], L).


%tbox
% [(sculpteur,and(personne,some(aCree,sculpture))),(auteur,and(personne,some(aEcrit,livre))),(editeur,and(personne,and(not(some(aEcrit,livre)),some(aEdite,livre)))),(parent,and(personne,some(aEnfant,anything)))]
%


%abox
% [(michelAnge,personne), (david,sculpture), (sonnets,livre),
% (vinci,personne), (joconde,objet)]



%PARTIE 2
%prop1
saisie_et_traitement_prop_a_demontrer([(michelAnge, personne)], Abi1, [])

