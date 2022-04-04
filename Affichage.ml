(*
module type SIGAFFICHAGE = 
	sig

		val aff : string -> unit
		
		val afficher_contenu_sac : Objet.Objet.sac -> string
		
		val afficher_personnage : Personnage.Personnage.personnage -> string

		val phrase_intialisation_surprise : Monstre.Monstre.monstre -> string

		val phrase_attaque_monstre : Monstre.Monstre.monstre -> int -> string

		val phrase_tentative_perso : int->string

		val phrase_mort_personnage : Personnage.Personnage.personnage -> Monstre.Monstre.monstre -> string

		val phrase_mort_nuit : Personnage.Personnage.personnage -> Monstre.Monstre.monstre -> string

		val phrase_nuit : Personnage.Personnage.personnage -> string

		val phrase_perte_objet : int -> Objet.Objet.contenu -> string

		val phrase_evenement_monstre : Monstre.Monstre.monstre -> string

		val phrase_level_up : int -> string

		val phrase_demande_action : unit->string

		val phrase_exp_obj_up : int -> Objet.Objet.objet -> string

		val demander_action : unit->string

		val demander_reaction : unit->string
	end;;
		*)
		
module Affichage =
	struct
	
	(** Fonction produisant une ligne de nbfois tirets
	@param nbfois : le nombre de tirets que l'on veut
	@return la ligne de nbfois tirets*)
	let ligne_de_tirets : int -> string = fun nbfois ->
		let nombrefois = nbfois-2 in 
		let rec aux = fun nb ligne ->
		if nb<=0 then ligne
		else aux (nb-1) (ligne^"-")
		in aux nombrefois ""

	(** Fonction produisant mettant des croix aux bornes d'un string
	@param ligne : le string que l'on veut borner
	@return le string born� de tirets*)
	let borner_ligne_croix : string -> string = fun ligne ->
		"+"^ligne^"+"

	(** Fonction permettant de genrer la classe du personnage
	@param perso : le personnage dont on souhaite genrer la classe
	@return le string de la classe genr�e*)
	let genrer_classe : Personnage.Personnage.personnage -> string = fun perso ->
		let classe=perso.classe in
		match classe with 
		| Guerrier when perso.genre = Femme -> "Guerriere"
		| Guerrier -> "Guerrier"
		| Archer when perso.genre = Femme -> "Archere"
		| Archer  -> "Archer"
		| Magicien when perso.genre = Femme -> "Magicienne"
		| Magicien -> "Magicien"

	(** Fonction permettant de mettre au pluriel si besoin les diff�rents contenus du sac
	@param qte : la quantite de l'objet du sac
	@param ctn : le contenu de l'objet du sac*)
	let bien_ecrire_contenu : int-> Objet.Objet.contenu -> string = fun qte ctn->
		let laqte = string_of_int qte in
		match ctn with
		| Eponge when qte>1 -> laqte^" eponges"
		| Eponge-> laqte^" eponge"
		| Piece when qte>1 -> laqte^" pieces"
		| Piece -> laqte^" piece"
		| Poulet when qte >1 -> laqte^" poulets"
		| Poulet -> laqte^" poulet"

	(** Fonction permettant de convertir un nom de monstre en string
	@param m : le monstre dont on veut convertir le nom
	@return le string du nom du monstre*)
	let string_monstre : Monstre.Monstre.monstre -> string = fun m -> match m.monstre with
    |Golem -> "golem"
    |Sanglier -> "sanglier"
    |Moustiques n-> "nu�e de moustiques"
    
    
	(** Fonction permettant d'afficher le contenu du sac
	@param sac : le sac dont on veut afficher le contenu
	@return le string de l'affichage du sac*)
	let afficher_contenu_sac : Objet.Objet.sac->string = fun sac ->
		let rec aux = fun sac affichage ->
		match sac with
		| [] -> affichage
		| Objet.Objet.{quantite = qte ; obj = _}::tail when qte<=0 -> aux tail affichage
		| Objet.Objet.{quantite = qte ; obj = ctn}::[] -> affichage^(bien_ecrire_contenu qte ctn)
		| Objet.Objet.{quantite = qte ; obj = ctn }::tail -> aux tail (affichage^(bien_ecrire_contenu qte ctn)^"\n")
		in aux sac ""
		
	(** Fonction permettant d'afficher la ligne de l'identit� du personnage
	@param perso : le personnage dont on souhaite afficher l'identit�
	@return le string de l'affichage de l'identit�*)
	let afficher_identite : Personnage.Personnage.personnage->string = fun perso ->
		"| "^(perso.nom)^" | "^(genrer_classe perso)^" niveau "^(string_of_int perso.lvl)^" |"

	(** Fonction permettant d'avoir la longueur de l'affichage de l'identit�
	@param perso : le personnage dont on souhaite avoir la longueur de l'affichage d'identit�
	@return le integer de la longueur de l'affichage de l'identit�*)
	let longueur_affichage : Personnage.Personnage.personnage->int = fun perso -> 
		String.length (afficher_identite perso)
		
	(** Fonction permettant de cr�er une ligne de nbfois d'espaces
	@param nbfois : le nombre d'espaces qu'on souhaite avoir
	@return : le string de la ligne d'espaces*)
	let ligne_espace: int -> string = fun nbfois ->
		let rec aux = fun nbfois ligne ->
		if nbfois<=0 then ligne
		else aux (nbfois-1) (ligne^" ")
		in aux nbfois ""	

	(** Fonction permettant d'afficher la ligne des Points de vie du personnage
	@param perso : le personnage dont on souhaite afficher la ligne de points de vie
	@return : le string de l'affichage de la ligne des points de vie du personnage*)
	let afficher_pv : Personnage.Personnage.personnage->string = fun perso ->
		let lg = longueur_affichage perso in
		let stringpv= "| Points de vie |" in
		let stringnbpv=(string_of_float perso.pv)^" |" in
		let nbespaces = lg-(String.length (stringpv) + String.length (stringnbpv)) in
		stringpv^ligne_espace nbespaces^stringnbpv

	(** Fonction permettant d'afficher la ligne de l'exp�rience du personnage
	@param perso : le personnage dont on souhaite afficher de l'exp�rience
	@return : le string de l'affichage de la ligne de l'exp�rience du personnage*)	
	let afficher_exp : Personnage.Personnage.personnage->string = fun perso ->
		let lg = longueur_affichage perso in
		let lgstringpv=String.length "| Points de vie |" in
		let lgstringexp= String.length "| Experience |" in
		let nbespacesexp= lgstringpv-lgstringexp in
		let stringexp="| Experience"^ligne_espace nbespacesexp^" |" in
		let stringnbexp=(string_of_int perso.exp)^" |" in
		let nbespacesnbexp = lg-(String.length (stringexp) + String.length (stringnbexp)) in
		stringexp^(ligne_espace nbespacesnbexp)^stringnbexp
		
	(** Fonction permettant d'afficher les lignes du sac pour l'affichage de l'�tat du personnage
	@param : perso : le personnage dont on veut l'affichage du sac
	@return : le string de l'affichage de les lignes du sac pour l'affichage de l'�tat du personnage*)
	let afficher_sac_pas_complet : Personnage.Personnage.personnage ->string = fun perso ->
		let lesac= perso.sac in
		let lg = longueur_affichage perso in 
		let rec aux = fun sac affichage->
		match sac with
		[] -> affichage
		| Objet.Objet.{quantite= qte ; obj = ctn}::tail when qte<=0 -> aux tail affichage
		| Objet.Objet.{quantite= qte ; obj = ctn}::[] -> 
		 let bec="|  "^(bien_ecrire_contenu qte ctn) in let lg2= String.length bec in affichage^bec^(ligne_espace (lg-lg2-1))^"|"
		| Objet.Objet.{quantite = qte ; obj = ctn }::o::tail when o.quantite>0 -> let bec="|  "^(bien_ecrire_contenu qte ctn) in let lg2= String.length bec in aux (o::tail) (affichage^bec^(ligne_espace (lg-lg2-1))^"|"^"\n")
		| Objet.Objet.{quantite = qte ; obj = ctn }::tail -> let bec="|  "^(bien_ecrire_contenu qte ctn) in let lg2= String.length bec in aux tail (affichage^bec^(ligne_espace (lg-lg2-1))^"|")
		in aux lesac ""

	(** Fonction permettant d'afficher la ligne du sac pour l'affichage de l'�tat du personnage
	@param perso : le personnage dont on veut afficher l'�tat
	@return : le string de la ligne du sac pour l'affichage de l'�tat du personnage*)
	let afficher_partie_sac : Personnage.Personnage.personnage ->string = fun perso ->
		let lg = longueur_affichage perso in 
		let sac = "| Sac" in
		let lgsac=String.length sac in
		sac^(ligne_espace (lg-1-lgsac))^"|"

	(** Fonction permettant d'afficher la partie sac pour l'affichage de l'�tat du personnage
	@param perso : le personnage dont on veut afficher le sac 
	@return : le string de l'affichage de la partie sac pour l'affichage de l'�tat du personnage*)	
	let afficher_sac : Personnage.Personnage.personnage->string = fun perso ->
		(afficher_partie_sac perso)^"\n"^(afficher_sac_pas_complet perso)

	(**Fonction permettant d'affichage l'�tat du personnage
	@param perso : le personnage dont on veut afficher l'�tat
	@return : le string de l'affichage de l'�tat du personnage*)
	let afficher_personnage : Personnage.Personnage.personnage -> string = fun perso -> 
		let lg = longueur_affichage perso in
		let ligne_croix = borner_ligne_croix (ligne_de_tirets lg) in
		ligne_croix^"\n"^(afficher_identite perso)^"\n"^ligne_croix^"\n"^(afficher_pv perso)^"\n"^ligne_croix^"\n"^(afficher_exp perso)^"\n"^ligne_croix^"\n"^(afficher_sac perso)^"\n"^ligne_croix
	

(** Fonction permettant l'affichage de string
@param s : le string que l'on souhaite afficher*)
let aff : string->unit = fun s ->
	print_string(s)

(** Fonction permettant de mettre un article indefini devant le type du monstre
@param monstre : le monstre pour lequel on veut l'article indefini
@return : le string de l'article indefini avec le type du monstre*)
let un_nom_monstre : Monstre.Monstre.monstre->string = fun monstre ->
	let m = monstre.monstre in
	match m with
	| Golem -> "un Golem"
	| Moustiques n when n=1-> "une Nuee de "^(string_of_int n)^" moustique"
	| Moustiques n -> "une Nuee de "^(string_of_int n)^" moustiques"
	| Sanglier -> "un Sanglier"

(** Fonction permettant de donner aleatoirement une phrase pour malheureuse_rencontre
@ param monstre : le monstre de la malheureuse rencontre
@ return le string de la phrase pour la malheureuse rencontre*)
let phrase_intialisation_surprise : Monstre.Monstre.monstre->string = fun monstre ->
	let lenb=Random.int 3 in
	match lenb with
	| 0 -> "Soudain, vous tombez nez a nez avec "^(un_nom_monstre monstre)^"."
	| 1 -> "Tout à coup, "^(un_nom_monstre monstre)^" surgit."
	| 2 -> "D’un seul coup, vous vous retrouver face à face à "^(un_nom_monstre monstre)^"."
	| _ -> ""
	
(** Fonction permettant d'accorder au feminin et masculin un mot selon le genre du personnage
@param perso : le personnage dont on veut genre le mot
@param mot : le mot que l'on souhaite genrer
@return le string du mot genrer*)
let accord_fem_masc : Personnage.Personnage.personnage -> string -> string = fun perso mot -> match perso.genre with
    |Homme -> mot
    |Femme -> mot^"e"

(**Fonction permettant d'avoir le pronom personnel correspondant au monstre
@param monstre : le monstre dont on souhaite avoir le pronom personnel
@return le string du pronom personnel*)
let pronom_monstre : Monstre.Monstre.monstre -> string = fun monstre -> 	match monstre.monstre with
    |Golem -> "Il"
    |Sanglier -> "Il"
    |Moustiques n -> "Elle"


(**Fonction permettant de mettre au pluriel un mot si besoin (selon un int)
@param nb : le nombre (int) permettant de savoir si il y a plusieurs fois le mot
@param mot : le mot que l'on souhaite mettre au pluriel ou pas*)
let accord_pluriel_int : int->string->string = fun nb mot ->
	match nb with
	| n when n>1 -> mot^"s"
	| _ -> mot

(**Fonction permettant de mettre au pluriel un mot si besoin (selon un float)
@param nb : le nombre (float) permettant de savoir si il y a plusieurs fois le mot
@param mot : le mot que l'on souhaite mettre au pluriel ou pas*)
let accord_pluriel_float : float->string->string = fun nb mot ->
	match nb with
	| n when n>1. -> mot^"s"
	| _ -> mot

(** Fonction permettant de generer aletoirement une phrase de tentative d'attaque de monstre avec le nombre de points de vie retires au personnage
@param monstre : le monstre qui va faire l'attaque
@param pv : le nombre de points de vie retires au personnage
@return le string de la tentative du monstre avec le nombre de points de vie retires au personnage*)
let phrase_tentative_monstre : Monstre.Monstre.monstre->float->string = fun monstre pv ->
	let pv_retires = string_of_float pv in
	let lenb=Random.int 3 in
	match lenb with
	| 0 -> "vous touche et vous perdez "^pv_retires^" "^(accord_pluriel_float pv "point")^" de vie."
	| 1 -> "vous blesse et vous retire "^pv_retires^" "^(accord_pluriel_float pv "point")^" de vie."
	| 2 -> "fait mouche et vous ote "^pv_retires^" "^(accord_pluriel_float pv "point")^" de vie."
	| _ -> ""

(** Fonction permettant de mettre un article defini devant le type de monstre
@param monstre : le monstre pour lequel on veut l'article defini
@return le string de l'article defini avec le type du monstre*)
let le_nom_monstre : Monstre.Monstre.monstre->string = fun monstre ->
	let m = monstre.monstre in
	match m with
	| Golem -> "le Golem"
	| Moustiques n when n=1-> "la Nuee de "^(string_of_int n)^" moustique"
	| Moustiques n -> "la Nuee de "^(string_of_int n)^" moustiques"
	| Sanglier -> "la Sanglier"

(** Fonction permettant de generer aleatoirement une phrase lorsque le monstre attaque
@param monstre : le monstre qui attaque
@param pv_retires : le nombre de points de vie retires au personnage
@return le string de l'attaque du monstre*)
let phrase_attaque_monstre : Monstre.Monstre.monstre->float->string = fun monstre pv_retires ->
	let lenb=Random.int 3 in
	match lenb with
	| 0 -> (le_nom_monstre monstre)^" tente de vous blesser. "^ (pronom_monstre monstre)^" "^(phrase_tentative_monstre monstre pv_retires)^"."
	| 1 -> (le_nom_monstre monstre)^" vous attaque violemment. "^(pronom_monstre monstre)^" "^(phrase_tentative_monstre monstre pv_retires)^"."
	| 2 -> (le_nom_monstre monstre)^" vous charge brutalement. "^(pronom_monstre monstre)^" "^(phrase_tentative_monstre monstre pv_retires)^"."
	| _ -> ""

(**Fonction permettant de generer aleatoirement la phrase de la tentative du personnage
@param pv : le nombre de points de vie retires au monstre
@return le string de la phrase de la tentative du personnage*)
let phrase_tentative_perso : int->string = fun pv ->
	let lenb=Random.int 3 in
	let pv_retires = string_of_int pv in
	if pv=0 then
		match lenb with
		| 0 -> "Vous attaquer mais vous manquez votre cible."
		| 1 -> "Vous attaquer le monstre mais vous le rater."
		| 2 -> "Vous tenter de blesser le monstre mais il esquive votre attaque."
		| _ -> ""
	else
		match lenb with
		| 0 -> "Vous attaquer et vous touchez le monstre. Il perd "^pv_retires^" "^(accord_pluriel_int pv "point")^" de vie." 
		| 1 -> "Vous attaquer et blessez le monstre. Vous lui retirez "^pv_retires^" "^(accord_pluriel_int pv "point")^" de vie." 
		| 2 -> "Vous tenter de blesser le monstre et faites mouche. Vous lui otez "^pv_retires^" "^(accord_pluriel_int pv "point")^" de vie." 
		| _ -> ""

(** Fonction permettant de mettre un article demonstratif devant le type de monstre
@param monstre : le monstre pour lequel on veut l'article demonstratif
@return le string de l'article demonstratif avec le type du monstre*)
let ce_nom_monstre : Monstre.Monstre.monstre->string = fun monstre ->
	let m = monstre.monstre in
	match m with
	| Golem -> "ce Golem"
	| Moustiques n when n=1-> "cette Nuee de "^(string_of_int n)^" moustique"
	| Moustiques n -> "cett Nuee de "^(string_of_int n)^" moustiques"
	| Sanglier -> "cet Sanglier"

(** Fonction permettant de generer aleatoirement une phrase de mort de monstre
@param monstre : le monstre mourrant
@return le string de la phrase de la mort du monstre*)
let phrase_mort_monstre : Monstre.Monstre.monstre->string = fun monstre ->
	let lenb=Random.int 3 in
	match lenb with
	| 0 -> (le_nom_monstre monstre)^" meurt dans d’affreuses (vraiment affreuses) souffrances."
	| 1 -> "L’horrible "^(string_monstre monstre)^" tombe a terre en laissant planer un silence de mort."
	| 2 -> "“O desespoir ! Vous etes bien trop fort !” hurle "^(le_nom_monstre monstre)^" en s’effondrant au sol..."
	| _ -> ""

(** Fonction permettant de generer aletoirement une phrase de mort pour le personnage
@param perso : le personnage mourrant
@param mosntre : le monstre ayant tue le personnage
@return le string de la phrase de la mort du personnage*)
let phrase_mort_personnage : Personnage.Personnage.personnage-> Monstre.Monstre.monstre->string = fun perso monstre->
	let lenb=Random.int 3 in
	match lenb with
	| 0 -> "Vous tombez au combat aujourd’hui. D’autre loueront vos exploits mais aujourd’hui "^(ce_nom_monstre monstre)^" a entache votre reputation a tout jamais."
	| 1 -> "Vous etes "^(accord_fem_masc perso "mort")^"."
	| 2 -> "Vous mourrez dans d’affreuses souffrances."
	| _ -> ""

(** Fonction permettant de generer une phrase lorsque le personnage meurt durant la nuit
@param perso : le personnage allant mourir
@param monstre : le monstre tuant le personnage
@return le string de la phrase de mort du personnage*)
let phrase_mort_nuit : Personnage.Personnage.personnage-> Monstre.Monstre.monstre->string = fun perso monstre->
	match monstre.monstre with
		| Golem -> "Vous installez votre campement et tombez rapidement "^(accord_fem_masc perso "endormi")^". Pendant votre sommeil, un Golem surgit et vous fracasse le crane. Vous êtes accord_fem_masc mort."
		| Moustiques n -> "Vous etes "^(accord_fem_masc perso "fatigue")^" et decidez de dormir a la belle etoile pret d’un marecage. Cependant, une Nuee de moustique apparait et vous pique inlassablement jusqu’a vous tuer. Vous etes "^(accord_fem_masc perso "mort")^"."
		| _ -> "Vos nombreux baillements vous poussent a installer un campement pour dormir e l’oree d’une foret. Vous pensant a l’abri vous vous endormez. Or, un Sanglier vous charge et vous pietine. Vous gemissez de douleur jusqu’a la mort."

(** Fonction permettant de generer aleatoirement une phrase lorsque le personnage dort sans mourir
@param perso : le personnage allant dormir
@return le string de la phrase de repos*)		
let phrase_nuit : Personnage.Personnage.personnage->string = fun perso ->
	let lenb=Random.int 3 in
		match lenb with
		| 0 -> "Vous installez votre campement et tombez rapidement "^(accord_fem_masc perso "endormi")^". Au petit matin, vous vous réveillez sans encombre et gagnez 4 points vies."
		| 1 -> "Vous etes "^(accord_fem_masc perso "fatigue")^" et decidez de dormir a la belle etoile pret d’un marecage. La lueur du Soleil levant sur l’eau vous reveille et vous gagnez 4 points de vies."
		| 2 -> "Vos nombreux baillements vous poussent a installer un campement pour dormir a l’oree d’une foret. A l’aube, la rosée du matin vous reveille et vous gagnez 4 points de vie."
		| _ -> ""


(**Fonction permettant de generer aleatoirement une phrase lorsque le personnage perd un objet
@param nb : la quantite de l'objet perdu
@param ctn : le contenu de l'objet perdu	
@return le string de la phrase de la perte de l'objet*)	
let phrase_perte_objet : int-> Objet.Objet.contenu->string = fun nb ctn ->
	let lenb=Random.int 3 in
		match lenb with
		| 0 -> "Dans votre precipitation a fuir vous perdez "^(string_of_int nb)^(bien_ecrire_contenu nb ctn)^"."
		| 1 -> "Vous fuyez rapidement mais egarez "^(string_of_int nb)^(bien_ecrire_contenu nb ctn)^"."
		| 2 -> "En prenant la fuite vous laissez echapper "^(string_of_int nb)^(bien_ecrire_contenu nb ctn)^"."
		| _ -> ""

(** Fonction permettant de generer aleatoirement un evenement avec un monstre
@param monstre : le monstre de l'evenement
@return le string de la phrase de l'evenement*)
let phrase_evenement_monstre : Monstre.Monstre.monstre->string = fun monstre ->
	let lenb=Random.int 4 in
		match monstre.monstre with
		| Golem ->
			(match lenb with
			| 0 -> "Le sol tremble et un enorme Golem passe au dessus de votre tete sans vous voir."
			| 1 -> "Au loin, vous appercevez une tete de Golem au dessus des arbres de la foret."
			| 2 -> "Vous voyez un Golem qui prend tranquillement un bain de boue dans le marai en chantonnant."
			| 3 -> "Vous sentez une odeur pestinentielle vennant vers vous : c’est un Golem puant qui flatule devant votre nez."
			| _ -> "")
		| Sanglier ->
			(match lenb with
			| 0 -> "Vous entendez du bruit a l’oree de la foret et vous apercevez un Sanglier."
			| 1 -> "Vous voyez un Sanglier qui fouille le sol au loin."
			| 2 ->"Un Sanglier cherche des baies en se baladant entre les buissons."
			| 3 -> "Vous reconnaissez, au loin, la musique “Hakuna Matata” : c’est un Sanglier qui l’interprete gracieusement. Entrainnez par la musique, vous commencez a chantonner a votre tour."
			| _ -> "")
		| Moustiques n ->
			(match lenb with
			| 0 -> "Vous entendez de “bizzz…bizzzz…bizzzzz” autour de vous : c’est "^(un_nom_monstre monstre)^"."
			| 1 -> "Vous appercevez "^(un_nom_monstre monstre)^" au loin."
			| 2 -> "Un bruit assourdissant passe pret de vous : vous reconnaissez "^(un_nom_monstre monstre)^"."
			| 3 -> "Vous voyez "^(un_nom_monstre monstre)^"passant entre les arbres."
			| _ -> "")

(** Fonction permettant de generer aletoirement une phrase de passage de niveau
@param lvl : le niveau qu'a atteint le personnage
@return le strign de la phrase de passage de niveau*)
let phrase_level_up : int->string = fun lvl ->
	let lenb=Random.int 3 in
		match lenb with
		| 0 -> "Feliciations ! Vous progressez au niveau "^(string_of_int lvl)^"."
		| 1 -> "Bravo ! Vous augmentez au niveau "^(string_of_int lvl)^"."
		| 2 -> "Hourra ! Vous passez au niveau "^(string_of_int lvl)^"."
		| _ -> ""

(** Fonction permettant de donner le string d'un potentiel gain d'objet
@param obj : l'objet gagné par le personnage
@return le string du gain d'objet ou un point sinon*)
let gain_objet : Monstre.Monstre.loot -> string = fun obj ->
	match obj with
	| Rien -> "."
	| Objet {quantite=qte;obj=ctn} -> " et "^string_of_int qte^" "^(bien_ecrire_contenu qte ctn)^"."

(** Fonction permettant de generer aleatoirement une phrase de gain de points d'experience et d'objet
@param exp : le nombre de points d'experience gagne par le personnage
@param obj : l'objet porté par le monstre et gagne par le personnage
@return le string du gain de points d'experience et de l'objet gagne*)
let phrase_exp_obj_up : int -> Monstre.Monstre.loot -> string = fun exp obj ->
	let lenb=Random.int 3 in
		match lenb with
		| 0 -> "Vous gagnez "^(string_of_int exp)^" "^(accord_pluriel_int exp "point")^" d'experience"^(gain_objet obj)
		| 1 -> "Vous remportez "^(string_of_int exp)^" "^(accord_pluriel_int exp "point")^" d'experience"^(gain_objet obj)
		| 2 -> "Vous obtenez "^(string_of_int exp)^" "^(accord_pluriel_int exp "point")^" d'experience"^(gain_objet obj)
		| _ -> ""

(** Fonction permettant de generer aleatoirement une question pour faire une action
@return le string de la question*)
let phrase_demande_action : unit->string = fun () ->
	let lenb=Random.int 3 in
		match lenb with
		| 0 -> "Que voulez-vous faire ?"
		| 1 -> "Que faites-vous ?"
		| 2 -> "Quelle action voulez-vous affectuer ?"
		| _ -> ""


exception Choix_invalide of string

(** Fonction permettant de demander a l'utilisateur un choix d'action
@return le choix de l'utilisateur*)
let rec demander_action : unit -> string = fun () ->
	print_string "(C) Continuer votre chemin \n(D) Dormir \n(M) Manger \n(V) Visualiser l'etat de votre personnage \n(Q) Quitter l'aventure \n<?> ";
	let reponse = read_line () in
		if reponse = "C" || reponse = "c" || reponse = "D" || reponse = "d" || reponse = "M" || reponse = "m" || reponse = "V" || reponse = "v" || reponse = "Q" || reponse = "q"  then reponse else raise (Choix_invalide "\n Votre choix est invalide ! \n")


(** Fonction permmettant de demander a l'utilisateur un choix de réaction par rapport à un évènement
@return le choix de l'utilisateur*)
let demander_reaction : unit -> string = fun () ->
	print_string "(A) Attaquer \n(F) Fuir \n(V) Visualiser l'etat de votre personnage \n<?> ";
	let reponse = read_line () in
		if reponse = "A" || reponse = "a" || reponse = "F" || reponse = "f" || reponse = "V" || reponse = "v" then reponse else raise (Choix_invalide "\n Votre choix est invalide ! \n")

	(** Fonction qui affiche l'introduction dans l'univers du jeu*)
	let debut_partie : unit -> unit = fun () -> print_string " Bienvenue jeune aventurier(e), vous vous apretez a vous lancer dans une aventure remplis d'embuche.
Parviendrez vous a terrasser tout les monstres qui se dresseront sur votre chemin pour atteindre le niveau 10 ou allez vous mourrir dans d'atroce souffrance ?"

(** Fonction permettant de demander a l'utilisateur son nom
@return le nom de l'utilisateur*)
let demander_nom : unit -> string = fun () -> print_string "Tout d'abord quel est votre nom jeune aventurier ? ";
    read_line ()

(** Fonction permettant de demander a l'utilisateur son genre *)
let demander_genre : unit -> string = fun () -> print_string ("Enchanté"^demander_nom()^"etes vous un (H) Homme ou une (F) Femme ?\n<?> ");
    let reponse = read_line() in
        if reponse = "H" || reponse = "F" || reponse = "h" || reponse = "f" then reponse else raise (Choix_invalide "\n Votre choix est invalide ! \n")

    
(** Fonction permettant de demander a l'utilisateur sa classe
@param genre : le genre du personnage dont on veut demander la classe
@return le choix de classe de l'utilisateur*)
let demander_classe : Personnage.Personnage.genre -> string = fun genre -> if genre = Personnage.Personnage.Homme then print_string "Voulez vous etre un (G) Guerrier \n(A) Archer \n(M) Magicien \n<?> "
     else print_string "Voulez vous etre une (G) Guerriere \n(A) Archere \n(M) Magicienne \n<?> "
     ;
    let reponse = read_line () in 
    if reponse = "G" || reponse = "g" || reponse = "A" || reponse = "a" || reponse = "M" || reponse = "m" then reponse
    else raise (Choix_invalide "\n Votre choix est invalide ! \n")



	end
;;