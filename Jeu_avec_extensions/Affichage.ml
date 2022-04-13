module type SIGAFFICHAGE = 
	sig

		val aff : string -> unit
		
		val afficher_contenu_sac : Objet.Objet.sac -> string
		
		val afficher_personnage : Personnage.Personnage.personnage -> string

		val phrase_manger : unit -> string

		val phrase_intialisation_surprise : Monstre.Monstre.monstre -> string

		val phrase_attaque_monstre : Monstre.Monstre.monstre -> float -> string

		val phrase_tentative_perso : int->string

		val phrase_mort_monstre : Monstre.Monstre.monstre->string

		val phrase_mort_personnage : Personnage.Personnage.personnage -> Monstre.Monstre.monstre -> string

		val phrase_mort_nuit : Personnage.Personnage.personnage -> Monstre.Monstre.monstre -> string

		val phrase_nuit : Personnage.Personnage.personnage -> string

		val phrase_perte_objet : int -> Objet.Objet.contenu -> string

		val phrase_evenement_monstre : Monstre.Monstre.monstre -> string

		val phrase_level_up : int -> string

		val phrase_demande_action : unit->string

		val phrase_exp_obj_up : int -> Monstre.Monstre.loot -> string

		val afficher_action : unit -> string

		val demander_action : unit -> string

		val afficher_reaction : unit -> string

		val demander_reaction : unit -> string

		val debut_partie : unit -> string

		val demander_nom : unit -> string

		val afficher_genre : string -> string

		val demander_genre : unit -> string

		val afficher_classe : Personnage.Personnage.genre -> string
		
		val demander_classe : unit -> string

		val afficher_perso_change_arme : Personnage.Personnage.personnage -> string

		val demande_perso_change_arme : Personnage.Personnage.personnage -> string


	end;;

		
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
	let bien_ecrire_contenu : Personnage.Personnage.personnage -> int-> Objet.Objet.contenu -> string = fun perso qte ctn->
		let laqte = string_of_int qte in
		match ctn with
		| Eponge when qte>1 -> laqte^" eponges"
		| Eponge-> laqte^" eponge"
		| Piece when qte>1 -> laqte^" pieces"
		| Piece -> laqte^" piece"
		| Poulet when qte >1 -> laqte^" poulets"
		| Poulet -> laqte^" poulet"
		| Potion_Precision -> laqte^" potion de precision"
		| Potion_Puissance -> laqte^" potion de puissance"
		| Arme a when (Equipement.Equipement.creer_arme a)=perso.armequipe-> let b = Equipement.Equipement.creer_arme a in (Equipement.Equipement.afficher_arme b )^" {specialite : "^(Equipement.Equipement.get_spe b)^"}"^" *equipe*"
		| Arme a -> let b = Equipement.Equipement.creer_arme a in (Equipement.Equipement.afficher_arme b )^" {specialite : "^(Equipement.Equipement.get_spe b)^"}"
	
	(** Fonction permettant de convertir un nom de monstre en string
	@param m : le monstre dont on veut convertir le nom
	@return le string du nom du monstre*)
	let string_monstre : Monstre.Monstre.monstre -> string = fun m -> match m.monstre with
    |Golem -> "golem"
    |Sanglier -> "sanglier"
    |Moustiques n-> "nuee de moustiques"
    
	(** Fonction permettant d'afficher le contenu du sac
	@param sac : le sac dont on veut afficher le contenu
	@return le string de l'affichage du sac*)
	let afficher_contenu_sac : Personnage.Personnage.personnage -> Objet.Objet.sac->string = fun perso sac ->
		let rec aux : Objet.Objet.sac -> string -> string = fun sac affichage ->
		match sac with
		| [] -> affichage
		| Objet.Objet.{quantite = qte ; obj = _}::tail when qte<=0 -> aux tail affichage
		| Objet.Objet.{quantite = qte ; obj = ctn}::[] -> affichage^(bien_ecrire_contenu perso qte ctn)
		| Objet.Objet.{quantite = qte ; obj = ctn }::tail -> aux tail (affichage^(bien_ecrire_contenu perso qte ctn)^"\n")
		in aux sac ""

		(** Fonction permettant d'avoir la longueur de l'affichage de l'identit�
	@param perso : le personnage dont on souhaite avoir la longueur de l'affichage d'identit�
	@return le integer de la longueur de l'affichage de l'identit�*)
	let longueur_affichage : unit ->int = fun () -> 70
		
	(** Fonction permettant de cr�er une ligne de nbfois d'espaces
	@param nbfois : le nombre d'espaces qu'on souhaite avoir
	@return : le string de la ligne d'espaces*)
	let ligne_espace: int -> string = fun nbfois ->
		let rec aux = fun nbfois ligne ->
		if nbfois<=0 then ligne
		else aux (nbfois-1) (ligne^" ")
		in aux nbfois ""
		
	(** Fonction permettant d'afficher la ligne de l'identit� du personnage
    @param perso : le personnage dont on souhaite afficher l'identit�
    @return le string de l'affichage de l'identit�*)
    let afficher_identite : Personnage.Personnage.personnage->string = fun perso ->
			let nom = "| "^(perso.nom)^" |" in
			let lgnom = String.length nom in
			let fin = (genrer_classe perso)^" niveau "^(string_of_int perso.lvl)^" |" in
			let lgfin = String.length fin in 
			let esp = ligne_espace (longueur_affichage () - lgnom - lgfin) in
			nom^esp^fin

	(** Fonction permettant d'afficher la ligne des Points de vie du personnage
	@param perso : le personnage dont on souhaite afficher la ligne de points de vie
	@return : le string de l'affichage de la ligne des points de vie du personnage*)
	let afficher_pv : Personnage.Personnage.personnage->string = fun perso ->
		let lg = longueur_affichage () in
		let stringpv= "| Points de vie |" in
		let stringnbpv=(string_of_float perso.stats_base.stats.pv)^"/"^string_of_float (perso.stats_base.pv_max+.(Equipement.Equipement.get_stats perso.armequipe).pv)^"|" in
		let nbespaces = lg-(String.length (stringpv) + String.length (stringnbpv)) in
		stringpv^ligne_espace nbespaces^stringnbpv

	(** Fonction permettant d'afficher la ligne de l'exp�rience du personnage
	@param perso : le personnage dont on souhaite afficher de l'exp�rience
	@return : le string de l'affichage de la ligne de l'exp�rience du personnage*)	
	let afficher_exp : Personnage.Personnage.personnage->string = fun perso ->
		let lg = longueur_affichage () in
		let lgstringpv=String.length "| Points de vie |" in
		let lgstringexp= String.length "| Experience |" in
		let nbespacesexp= lgstringpv-lgstringexp in
		let stringexp="| Experience"^ligne_espace nbespacesexp^" |" in
		let stringnbexp=(string_of_int perso.exp)^" |" in
		let nbespacesnbexp = lg-(String.length (stringexp) + String.length (stringnbexp)) in
		stringexp^(ligne_espace nbespacesnbexp)^stringnbexp
	
	let bien_ecrire_spe : Personnage.Personnage.personnage -> string -> string = fun perso spe ->
		let lg = longueur_affichage () in
		match spe with
		| "Aucune" -> let a = "|    Specialite : Aucune" in let lga = String.length a in let l = ligne_espace (lg-lga-1) in a^l^"|"
		| "Attaque" -> let a = "|    Specialite : Attaque" in let lga = String.length a in let l = ligne_espace (lg-lga-1) in a^l^"|"
		| "Points de vie" -> let a = "|    Specialite : Points de vie" in let lga = String.length a in let l = ligne_espace (lg-lga-1) in a^l^"|"
		| "Precision" -> let a = "|    Specialite : Precision" in let lga = String.length a in let l = ligne_espace (lg-lga-1) in a^l^"|"
		| "Legendaire" -> let a = "|    Specialite : Legendaire" in let lga = String.length a in let l = ligne_espace (lg-lga-1) in a^l^"|"
		| "Defense" -> let a = "|    Specialite : Defense" in let lga = String.length a in let l = ligne_espace (lg-lga-1) in a^l^"|"
		|_ -> ""


	(** Fonction permettant d'afficher les lignes du sac pour l'affichage de l'état du personnage
	@param : perso : le personnage dont on veut l'affichage du sac
	@return : le string de l'affichage de les lignes du sac pour l'affichage de l'état du personnage*)
	let afficher_sac_pas_complet : Personnage.Personnage.personnage ->string = fun perso ->
		let lesac= perso.sac in
		let lg = longueur_affichage () in 
		let rec aux : Objet.Objet.sac -> string -> string = fun sac affichage->
			match sac with
		| [] -> affichage
		| Objet.Objet.{quantite= qte ; obj = ctn}::tail when qte<=0 -> aux tail affichage
		| Objet.Objet.{quantite= qte ; obj = ctn}::tail ->
			let bec="|  "^(bien_ecrire_contenu perso qte ctn) in let lg2= String.length bec in aux tail (affichage^"\n"^bec^(ligne_espace (lg-lg2-1))^"|")
		in aux lesac "" 
	
	let bool_afficher_sac_ou_pas : Personnage.Personnage.personnage -> bool = fun perso ->
		let str = afficher_sac_pas_complet perso in
		if str = "" then false
		else true 

	(** Fonction permettant d'afficher la ligne du sac pour l'affichage de l'état du personnage
	@param perso : le personnage dont on veut afficher l'état
	@return : le string de la ligne du sac pour l'affichage de l'état du personnage*)
	let afficher_partie_sac : Personnage.Personnage.personnage ->string = fun perso ->
		let lg = longueur_affichage () in 
		let sac = "| Sac" in
		let lgsac=String.length sac in
		sac^(ligne_espace (lg-1-lgsac))^"|"

	(** Fonction permettant d'afficher la partie sac pour l'affichage de l'�tat du personnage
	@param perso : le personnage dont on veut afficher le sac 
	@return : le string de l'affichage de la partie sac pour l'affichage de l'�tat du personnage*)	
	let afficher_sac : Personnage.Personnage.personnage->string = fun perso ->
		if (bool_afficher_sac_ou_pas perso)=false then (afficher_partie_sac perso)
		else (afficher_partie_sac perso)^(afficher_sac_pas_complet perso)

	(**Fonction permettant d'affichage l'�tat du personnage
	@param perso : le personnage dont on veut afficher l'�tat
	@return : le string de l'affichage de l'�tat du personnage*)
	let afficher_personnage : Personnage.Personnage.personnage -> string = fun perso -> 
		let lg = longueur_affichage () in
		let ligne_croix = borner_ligne_croix (ligne_de_tirets lg) in
		ligne_croix^"\n"^(afficher_identite perso)^"\n"^ligne_croix^"\n"^(afficher_pv perso)^"\n"^ligne_croix^"\n"^(afficher_exp perso)^"\n"^ligne_croix^"\n"^(afficher_sac perso)^"\n"^ligne_croix
	

(** Fonction permettant l'affichage de string
@param s : le string que l'on souhaite afficher*)
let aff : string->unit = fun s ->
	print_string(s)

(** Fonction permettant de générer aléatoirement un phrase lorsque le personnage mange
@return la phrase aléatoire*)
let phrase_manger : unit -> string = fun () ->
	let ran = Random.int 3 in
		match ran with 
		|0 -> "\nVous avez mangé un poulet du RU et vous avez regagné 2 pv\n"
		|1 -> "\nLe poulet que vous venez de manger était vraiment bon ! Vous gagnez 2 pv\n"
		|_ -> "\nVous engloutissez le poulet et rotez un coup afin de gagner 2 pv !\n"

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
	| 0 -> "Soudain, vous tombez nez a nez avec "^(un_nom_monstre monstre)^".\n"
	| 1 -> "Tout à coup, "^(un_nom_monstre monstre)^" surgit.\n"
	| 2 -> "D’un seul coup, vous vous retrouver face à face à "^(un_nom_monstre monstre)^".\n"
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
	let pv_abs = abs_float pv in
	let pv_retires = string_of_float pv_abs in
	let lenb=Random.int 3 in
	match lenb with
	| 0 -> "vous touche et vous perdez "^pv_retires^" "^(accord_pluriel_float pv_abs "point")^" de vie.\n"
	| 1 -> "vous blesse et vous retire "^pv_retires^" "^(accord_pluriel_float pv_abs "point")^" de vie.\n"
	| 2 -> "fait mouche et vous ote "^pv_retires^" "^(accord_pluriel_float pv_abs "point")^" de vie.\n"
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
	| Sanglier -> "le Sanglier"

(** Fonction permettant de generer aleatoirement une phrase lorsque le monstre attaque
@param monstre : le monstre qui attaque
@param pv_retires : le nombre de points de vie retires au personnage
@return le string de l'attaque du monstre*)
let phrase_attaque_monstre : Monstre.Monstre.monstre->float->string = fun monstre pv_retires ->
	let lenb=Random.int 3 in
	match lenb with
	| 0 -> (le_nom_monstre monstre)^" tente de vous blesser. "^ (pronom_monstre monstre)^" "^(phrase_tentative_monstre monstre pv_retires)
	| 1 -> (le_nom_monstre monstre)^" vous attaque violemment. "^(pronom_monstre monstre)^" "^(phrase_tentative_monstre monstre pv_retires)
	| 2 -> (le_nom_monstre monstre)^" vous charge brutalement. "^(pronom_monstre monstre)^" "^(phrase_tentative_monstre monstre pv_retires)
	| _ -> ""

(**Fonction permettant de generer aleatoirement la phrase de la tentative du personnage
@param pv : le nombre de points de vie retires au monstre
@return le string de la phrase de la tentative du personnage*)
let phrase_tentative_perso : int->string = fun pv ->
	let lenb=Random.int 3 in
	let pv_retires = string_of_int pv in
	if pv=0 then
		match lenb with
		| 0 -> "\nVous attaquer mais vous manquez votre cible.\n"
		| 1 -> "\nVous attaquer le monstre mais vous le rater.\n"
		| 2 -> "\nVous tenter de blesser le monstre mais il esquive votre attaque.\n"
		| _ -> ""
	else
		match lenb with
		| 0 -> "\nVous attaquer et vous touchez le monstre. Il perd "^pv_retires^" "^(accord_pluriel_int pv "point")^" de vie.\n" 
		| 1 -> "\nVous attaquer et blessez le monstre. Vous lui retirez "^pv_retires^" "^(accord_pluriel_int pv "point")^" de vie.\n" 
		| 2 -> "\nVous tenter de blesser le monstre et faites mouche. Vous lui otez "^pv_retires^" "^(accord_pluriel_int pv "point")^" de vie.\n" 
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
	| 0 -> "\n"^(le_nom_monstre monstre)^" meurt dans d’affreuses (vraiment affreuses) souffrances.\n"
	| 1 -> "\nL’horrible "^(string_monstre monstre)^" tombe a terre en laissant planer un silence de mort.\n"
	| 2 -> "\n“O desespoir ! Vous etes bien trop fort !” hurle "^(le_nom_monstre monstre)^" en s’effondrant au sol...\n"
	| _ -> ""

(** Fonction permettant de generer aletoirement une phrase de mort pour le personnage
@param perso : le personnage mourrant
@param mosntre : le monstre ayant tue le personnage
@return le string de la phrase de la mort du personnage*)
let phrase_mort_personnage : Personnage.Personnage.personnage-> Monstre.Monstre.monstre->string = fun perso monstre->
	let lenb=Random.int 3 in
	match lenb with
	| 0 -> "\nVous tombez au combat aujourd’hui. D’autre loueront vos exploits mais aujourd’hui "^(ce_nom_monstre monstre)^" a entache votre reputation a tout jamais.\n"
	| 1 -> "\nVous etes "^(accord_fem_masc perso ("mort"))^".\n"
	| 2 -> "\nVous mourrez dans d’affreuses souffrances.\n"
	| _ -> ""

(** Fonction permettant de generer une phrase lorsque le personnage meurt durant la nuit
@param perso : le personnage allant mourir
@param monstre : le monstre tuant le personnage
@return le string de la phrase de mort du personnage*)
let phrase_mort_nuit : Personnage.Personnage.personnage-> Monstre.Monstre.monstre->string = fun perso monstre->
	match monstre.monstre with
		| Golem -> "\nVous installez votre campement et tombez rapidement "^(accord_fem_masc perso ("endormi"))^". Pendant votre sommeil, un Golem surgit et vous fracasse le crane. Vous êtes accord_fem_masc mort.\n"
		| Moustiques n -> "\nVous etes "^(accord_fem_masc perso ("fatigue"))^" et decidez de dormir a la belle etoile pret d’un marecage. Cependant, une Nuee de moustique apparait et vous pique inlassablement jusqu’a vous tuer. Vous etes "^(accord_fem_masc perso ("mort"))^".\n"
		| _ -> "\nVos nombreux baillements vous poussent a installer un campement pour dormir e l’oree d’une foret. Vous pensant a l’abri vous vous endormez. Or, un Sanglier vous charge et vous pietine. Vous gemissez de douleur jusqu’a la mort.\n"

(** Fonction permettant de generer aleatoirement une phrase lorsque le personnage dort sans mourir
@param perso : le personnage allant dormir
@return le string de la phrase de repos*)		
let phrase_nuit : Personnage.Personnage.personnage->string = fun perso ->
	let lenb=Random.int 3 in
		match lenb with
		| 0 -> "\nVous installez votre campement et tombez rapidement "^(accord_fem_masc perso "endormi")^". Au petit matin, vous vous réveillez sans encombre et gagnez 4 points vies.\n"
		| 1 -> "\nVous etes "^(accord_fem_masc perso "fatigue")^" et decidez de dormir a la belle etoile pret d’un marecage. La lueur du Soleil levant sur l’eau vous reveille et vous gagnez 4 points de vies.\n"
		| 2 -> "\nVos nombreux baillements vous poussent a installer un campement pour dormir a l’oree d’une foret. A l’aube, la rosée du matin vous reveille et vous gagnez 4 points de vie.\n"
		| _ -> ""


(**Fonction permettant de generer aleatoirement une phrase lorsque le personnage perd un objet
@param nb : la quantite de l'objet perdu
@param ctn : le contenu de l'objet perdu	
@return le string de la phrase de la perte de l'objet*)	
let phrase_perte_objet : Personnage.Personnage.personnage -> int-> Objet.Objet.contenu->string = fun perso nb ctn ->
	let lenb=Random.int 3 in
		match lenb with
		| 0 -> "\nDans votre precipitation a fuir vous perdez "^(string_of_int nb)^(bien_ecrire_contenu perso nb ctn)^".\n"
		| 1 -> "\nVous fuyez rapidement mais egarez "^(string_of_int nb)^(bien_ecrire_contenu perso nb ctn)^".\n"
		| 2 -> "\nEn prenant la fuite vous laissez echapper "^(string_of_int nb)^(bien_ecrire_contenu perso nb ctn)^".\n"
		| _ -> ""

(** Fonction permettant de generer aleatoirement un evenement avec un monstre
@param monstre : le monstre de l'evenement
@return le string de la phrase de l'evenement*)
let phrase_evenement_monstre : Monstre.Monstre.monstre->string = fun monstre ->
	let lenb=Random.int 4 in
		match monstre.monstre with
		| Golem ->
			(match lenb with
			| 0 -> "\nLe sol tremble et un enorme Golem passe au dessus de votre tete sans vous voir.\n"
			| 1 -> "\nAu loin, vous appercevez une tete de Golem au dessus des arbres de la foret.\n"
			| 2 -> "\nVous voyez un Golem qui prend tranquillement un bain de boue dans le marai en chantonnant.\n"
			| 3 -> "\nVous sentez une odeur pestinentielle vennant vers vous : c’est un Golem puant qui flatule devant votre nez.\n"
			| _ -> "")
		| Sanglier ->
			(match lenb with
			| 0 -> "\nVous entendez du bruit a l’oree de la foret et vous apercevez un Sanglier.\n"
			| 1 -> "\nVous voyez un Sanglier qui fouille le sol au loin.\n"
			| 2 -> "\nUn Sanglier cherche des baies en se baladant entre les buissons.\n"
			| 3 -> "\nVous reconnaissez, au loin, la musique “Hakuna Matata” : c’est un Sanglier qui l’interprete gracieusement. Entrainnez par la musique, vous commencez a chantonner a votre tour.\n"
			| _ -> "")
		| Moustiques n ->
			(match lenb with
			| 0 -> "\nVous entendez de “bizzz…bizzzz…bizzzzz” autour de vous : c’est "^(un_nom_monstre monstre)^".\n"
			| 1 -> "\nVous appercevez "^(un_nom_monstre monstre)^" au loin.\n"
			| 2 -> "\nUn bruit assourdissant passe pret de vous : vous reconnaissez "^(un_nom_monstre monstre)^".\n"
			| 3 -> "\nVous voyez "^(un_nom_monstre monstre)^"passant entre les arbres.\n"
			| _ -> "")

(** Fonction permettant de generer aletoirement une phrase de passage de niveau
@param lvl : le niveau qu'a atteint le personnage
@return le strign de la phrase de passage de niveau*)
let phrase_level_up : int->string = fun lvl ->
	let lenb=Random.int 3 in
		match lenb with
		| 0 -> "\nFeliciations ! Vous progressez au niveau "^(string_of_int lvl)^".\n\n"
		| 1 -> "\nBravo ! Vous augmentez au niveau "^(string_of_int lvl)^".\n\n"
		| 2 -> "\nHourra ! Vous passez au niveau "^(string_of_int lvl)^".\n\n"
		| _ -> ""

(** Fonction permettant de donner le string d'un potentiel gain d'objet
@param obj : l'objet gagné par le personnage
@return le string du gain d'objet ou un point sinon*)
let gain_objet : Personnage.Personnage.personnage -> Monstre.Monstre.loot -> string = fun perso obj ->
	match obj with
	| Rien -> "."
	| Objet {quantite=qte;obj=ctn} -> " et "^(bien_ecrire_contenu perso qte ctn)^"."

(** Fonction permettant de generer aleatoirement une phrase de gain de points d'experience et d'objet
@param exp : le nombre de points d'experience gagne par le personnage
@param obj : l'objet porté par le monstre et gagne par le personnage
@return le string du gain de points d'experience et de l'objet gagne*)
let phrase_exp_obj_up : Personnage.Personnage.personnage -> int -> Monstre.Monstre.loot -> string = fun perso exp obj ->
	let lenb=Random.int 3 in
		match lenb with
		| 0 -> "\nVous gagnez "^(string_of_int exp)^" "^(accord_pluriel_int exp "point")^" d'experience"^(gain_objet perso obj)^"\n"
		| 1 -> "\nVous remportez "^(string_of_int exp)^" "^(accord_pluriel_int exp "point")^" d'experience"^(gain_objet perso obj)^"\n"
		| 2 -> "\nVous obtenez "^(string_of_int exp)^" "^(accord_pluriel_int exp "point")^" d'experience"^(gain_objet perso obj)^"\n"
		| _ -> ""

(** Fonction permettant de generer aleatoirement une question pour faire une action
@return le string de la question*)
let phrase_demande_action : unit->string = fun () ->
	let lenb=Random.int 3 in
		match lenb with
		| 0 -> "\n\nQue voulez-vous faire ?\n"
		| 1 -> "\n\nQue faites-vous ?\n"
		| 2 -> "\n\nQuelle action voulez-vous affectuer ?\n"
		| _ -> ""

let afficher_action : unit -> string = fun () -> 
		"(C) Continuer votre chemin \n(D) Dormir \n(M) Manger \n(E) Changer d'equipement \n(V) Visualiser l'etat de votre personnage \n(Q) Quitter l'aventure "

(** Fonction permettant de demander a l'utilisateur un choix d'action
@return le choix de l'utilisateur*)
let rec demander_action : unit -> string = fun () ->
	let () = print_string "\n<?> " in
	let reponse = read_line () in
		if reponse = "C" || reponse = "c" || reponse = "D" || reponse = "d" || reponse = "E" || reponse = "e" || reponse = "M" || reponse = "m" || reponse = "V" || reponse = "v" || reponse = "Q" || reponse = "q"  
			then reponse 
			else let () = print_string "\n Votre choix est invalide ! \n" in demander_action ()


let afficher_reaction : unit -> string = fun () ->
	"(A) Attaquer \n(F) Fuir \n(V) Visualiser l'etat de votre personnage "

(** Fonction permmettant de demander a l'utilisateur un choix de réaction par rapport à un évènement
@return le choix de l'utilisateur*)
let rec demander_reaction : unit -> string = fun () ->
	let () = print_string "\n<?> " in 
	let reponse = read_line () in
		if reponse = "A" || reponse = "a" || reponse = "F" || reponse = "f" || reponse = "V" || reponse = "v" 
			then reponse 
			else let () = print_string "\n Votre choix est invalide ! \n" in demander_reaction()

	(** Fonction qui affiche l'introduction dans l'univers du jeu*)
	let debut_partie : unit -> string = fun () -> "\nBienvenue jeune aventurier(e), vous vous appretez a vous lancer dans une aventure remplis d'embuches.
Parviendrez-vous a terrasser tout les monstres qui se dresseront sur votre chemin pour atteindre le niveau 10 ou allez vous mourrir dans d'atroces souffrances ? \n\n"

(** Fonction permettant de demander a l'utilisateur son nom
@return le nom de l'utilisateur*)
let demander_nom : unit -> string = fun () -> let () = print_string "Tout d'abord quel est votre nom jeune aventurier(e) ?\n<?> " in
    read_line ()

let afficher_genre : string -> string = fun n -> "\nEnchanté "^n^" ètes vous un (H) Homme ou une (F) Femme ?"

(** Fonction permettant de demander a l'utilisateur son genre *)
let rec demander_genre : unit -> string = fun () ->
	let () = print_string "\n<?> " in
    let reponse = read_line() in
        if reponse = "H" || reponse = "F" || reponse = "h" || reponse = "f" then reponse else let () = print_string "\n Votre choix est invalide ! \n" in demander_genre()

let afficher_classe : Personnage.Personnage.genre -> string = fun genre -> 
	if genre = Personnage.Personnage.Homme 
		then "\nVoulez vous etre un : \n(G) Guerrier \n(A) Archer \n(M) Magicien "
		else "\nVoulez vous etre une : \n(G) Guerriere \n(A) Archere \n(M) Magicienne "

(** Fonction permettant de demander a l'utilisateur sa classe
@param genre : le genre du personnage dont on veut demander la classe
@return le choix de classe de l'utilisateur*)
let rec demander_classe :unit -> string = fun () -> 
	let () = print_string "\n<?> " in
    let reponse = read_line () in 
    	if reponse = "G" || reponse = "g" || reponse = "A" || reponse = "a" || reponse = "M" || reponse = "m" 
				then reponse
    		else let () = print_string "\n Votre choix est invalide ! \n" in demander_classe ()

let liste_valeur_arme_guerrier : Equipement.Equipement.type_arme_guerrier -> string list = fun t ->
	match t with
	| Epee_en_bois ->("bois"::"Bois"::[])
	| Epee_de_nuada ->("nuada"::"Nuada"::[])
	| Kusanagi_et_Yata_no_kagami ->("kusanagi"::"Kusanagi"::[])
	| Aegis ->("aegis"::"Aegis"::[])
	| Durandal ->("durandal"::"Durandal"::[])
	| Excalibur ->("excalibur"::"Excalibur"::[])

let liste_valeur_arme_archer : Equipement.Equipement.type_arme_archer -> string list = fun t ->
	match t with
	| Arc_en_bois ->("bois"::"Bois"::[])
	| Arc_Artemis ->("artemis"::"Artemis"::[])
	| Gandiva ->("gandiva"::"Gandiva"::[])
	| Arc_bouclier_immortel ->("bouclier"::"Bouclier"::[])
	| Zephyr ->("zephyr"::"Zephyr"::[])
	| Arc_de_lumiere ->("lumiere"::"Lumiere"::[])

let liste_valeur_arme_magicien : Equipement.Equipement.type_arme_magicien -> string list = fun t ->
	match t with
	| Baton_en_bois ->("bois"::"Bois"::[])
	| Gae_bolga ->("gae"::"Gae"::[])
	| Caducee ->("caducee"::"Caducee"::[])
	| Voile_Ino ->("ino"::"Ino"::[])
	| Gambanteinn ->("gambanteinn"::"Gambanteinn"::[])
	| Baguette_de_sureau ->("sureau"::"Sureau"::[])

(** Fonction qui retourne la liste des valeurs que peut rentrer l'utilisateur lors du changement d'arme
@param p : le personnage
@return une lise de string correspondant aux différents choix*)
let liste_valeur_change_arme : Personnage.Personnage.personnage -> string list = fun p ->
	let rec aux : Objet.Objet.sac -> string list -> string list = fun sac list ->
		match sac with
		| [] -> []
		| Objet.Objet.{quantite = qte; obj = Arme a}::t when qte>0 -> 
			(match a with
			| G typ -> aux t list@(liste_valeur_arme_guerrier typ)
			| A typ -> aux t list@(liste_valeur_arme_archer typ)
			| M typ -> aux t list@(liste_valeur_arme_magicien typ))
		| _::t -> aux t list
	in aux p.sac []

(** Fonction qui demande qu'elle arme le joueur veut équiper
@param p : le personnage
@*)
let afficher_arme_guerrier : Equipement.Equipement.type_arme_guerrier -> string = fun t ->
	match t with
	| Epee_en_bois -> "(Bois) Epee en bois"  
	| Epee_de_nuada -> "(Nuada) Epee de Nuada"
	| Kusanagi_et_Yata_no_kagami -> "(Kusanagi) Kusanagi et Yata no Kagami"
	| Aegis -> "(Aegis) Aegis"
	| Durandal ->"(Durandal) Durandal"
	| Excalibur -> "(Excalibur) Excalibur"

let afficher_arme_archer : Equipement.Equipement.type_arme_archer -> string = fun t ->
	match t with
	| Arc_en_bois ->"(Bois) Arc en bois"
	| Arc_Artemis ->"(Artemis) Arc d'Artemis"
	| Gandiva ->"(Gandiva) Gandiva"
	| Arc_bouclier_immortel ->"(Bouclier) Arc bouclier immortel"
	| Zephyr ->"(Zephyr) Zephyr"
	| Arc_de_lumiere ->"(Lumiere) Arc de lumiere"

let afficher_arme_magicien : Equipement.Equipement.type_arme_magicien -> string = fun t ->
	match t with
	| Baton_en_bois ->"(Bois) Baton en bois"
	| Gae_bolga ->"(Gae) Gae Bolga"
	| Caducee ->"(Caducee) Caducee"
	| Voile_Ino ->"(Ino) Voile d'Ino"
	| Gambanteinn ->"(Gambanteinn) Gambanteinn"
	| Baguette_de_sureau ->"(Sureau) Baguette de sureau"

(** Fonction qui retourne la liste des valeurs que peut rentrer l'utilisateur lors du changement d'arme
@param p : le personnage
@return une lise de string correspondant aux différents choix*)
let afficher_change_arme : Personnage.Personnage.personnage -> string  = fun p ->
	let rec aux : Objet.Objet.sac -> string -> string = fun sac res ->
		match sac with
		| [] -> ""
		| Objet.Objet.{quantite = qte; obj = Arme a}::t when qte>0 -> 
			(match a with
			| G typ -> aux t res^(afficher_arme_guerrier typ)^"\n"
			| A typ -> aux t res^(afficher_arme_archer typ)^"\n"
			| M typ -> aux t res^(afficher_arme_magicien typ)^"\n")
		| _::t -> aux t res
	in aux p.sac ""


	(** Fonction qui demande à l'utilisateur l'arme pour le changement d'arme
	@param p : le personnage
	@return l'affichage de la demande avec toutes les armes possibles*)
	let afficher_perso_change_arme : Personnage.Personnage.personnage -> string = fun p ->
		"\nVeuillez choisir qu'elle arme vous voulez equiper :\n\n"^(afficher_change_arme p)

	(** Fonction qui enregistre le choix du personnage concernant le changement d'arme
	@param p : le personnage
	@return le string de l'arme dont il veut s'équiper*)
	let rec demande_perso_change_arme : Personnage.Personnage.personnage -> string = fun p ->
		let () = print_string "\n<?>" in
		let reponse = read_line() in 
		if List.mem reponse (liste_valeur_change_arme p)
			then reponse
			else let () = print_string "\n Votre choix est invalide ! \n" in demande_perso_change_arme p

	(** Fonction qui demande une action s'il y a un marchand itinérant
	@return le string de la demande*)
	let afficher_action_marchand : unit -> string = fun () ->
	"(C) Continuer votre chemin \n(D) Dormir \n(M) Manger \n(E) Changer d'equipement \n(V) Visualiser l'etat de votre personnage \n(S) Parler au marchand itinerant \n(Q) Quitter l'aventure "

	(** Fonction qui demande au joueur le choix d'action s'il y a un marchand itinérant
	@return le choix de l'utilisateur*)
	let rec demander_action_marchand : unit -> string = fun () ->
		let () = print_string "\n<?> " in
		let reponse = read_line () in
			if reponse = "C" || reponse = "c" || reponse = "D" || reponse = "d" || reponse = "E" || reponse = "e" || reponse = "M" || reponse = "m" || reponse = "V" || reponse = "v" || reponse = "S" || reponse = "s" || reponse = "Q" || reponse = "q"  
				then reponse 
				else let () = print_string "\n Votre choix est invalide ! \n" in demander_action_marchand ()

	(** Fonction qui demande une action s'il y a un village
	@return le string de la demande*)
	let afficher_action_village : unit -> string = fun () ->
	"(C) Continuer votre chemin \n(D) Dormir \n(M) Manger \n(E) Changer d'equipement \n(V) Visualiser l'etat de votre personnage \n(T) Se rendre au village \n(Q) Quitter l'aventure "


	(** Fonction qui demande au joueur le choix d'action s'il y a un village
	@return le choix de l'utilisateur*)
	let rec demander_action_village : unit -> string = fun () ->
		let () = print_string "\n<?> " in
		let reponse = read_line () in
			if reponse = "C" || reponse = "c" || reponse = "D" || reponse = "d" || reponse = "E" || reponse = "e" || reponse = "M" || reponse = "m" || reponse = "V" || reponse = "v" || reponse = "T" || reponse = "t" || reponse = "Q" || reponse = "q"  
				then reponse 
				else let () = print_string "\n Votre choix est invalide ! \n" in demander_action_village ()

	(** Fonction qui demande une action s'il y a un marchand itinérant et un village
	@return le string de la demande*)
	let afficher_action_marchand_village : unit -> string = fun () ->
	"(C) Continuer votre chemin \n(D) Dormir \n(M) Manger \n(E) Changer d'equipement \n(V) Visualiser l'etat de votre personnage \n(S) Parler au marchand itinerant \n(T) Se rendre au village \n(Q) Quitter l'aventure "


	(** Fonction qui demande au joueur le choix d'action s'il y a un marchand itinérant et un village
	@return le choix de l'utilisateur*)
	let rec demander_action_marchand_village : unit -> string = fun () ->
		let () = print_string "\n<?> " in
		let reponse = read_line () in
			if reponse = "C" || reponse = "c" || reponse = "D" || reponse = "d" || reponse = "E" || reponse = "e" || reponse = "M" || reponse = "m" || reponse = "V" || reponse = "v" || reponse = "S" || reponse = "s" || reponse = "T" || reponse = "t" || reponse = "Q" || reponse = "q"  
				then reponse 
				else let () = print_string "\n Votre choix est invalide ! \n" in demander_action_marchand_village ()

	
	(*******************auberge*********************)

let piece_sac : Personnage.Personnage.personnage -> string = fun perso ->
	let piece = Objet.Objet.qte_obj perso.sac Piece in
	"Vous avez "^(string_of_int piece)^" "^(bien_ecrire_contenu perso piece Piece)^" dans votre sac.\n" 

(** Fonction permettant de générer aléatoirement une phrase au hasard lorsque le personnage dort à l'auberge
@return le string de la phrase du personnage dormant*)
let phrase_dormir_aub : unit -> string = fun() ->
		let lenb = Random.int 3 in
		match lenb with 
			| 0 -> "Vous sautez dans le lit, vous endormez comme un bébé et gagnez 10 points de vie"
			| 1 -> "Vous tombez directement dans le lit et sombrez dans un sommeil profond. Vous gagnez 10 points de vie"
			| 2 -> "Vous vous enroulez dans les draps du lit et tombez de sommeil. Vous remportez 10 points de vie."
			|_ -> ""

(** Fonction permettant de générer aléatoirement une question pour demander ce que l'on peut rendre comme service
@return le string de la question*)
let question_service : unit -> string = fun()->
	let lenb = Random.int 3 in
	match lenb with 
		| 0 -> "Que souhaitez-vous ?"
		| 1 -> "Que puis-je faire pour vous ?"
		| 2 -> "Quel service puis-je vous rendre ?"
		|_ -> ""

(** Fonction permettant de générer une phrase d'initialiation pour l'aubergiste aléatoirement 
@return le string de la phrase d'initialisation *)
let phrase_aubergiste : unit-> string = fun() ->
		let lenb = Random.int 3 in
		match lenb with 
			| 0 -> "Bienvenue à vous dans mon auberge ! "^(question_service ())^"\n"
			| 1 -> "Bonjour et bienvenue dans notre auberge ! "^(question_service ())^"\n"
			| 2 -> "Bien le bonjour ! Nous avons des lits et de la nourriture à foison ! "^(question_service ())^"\n"
			|_ -> ""

(** Fonction permettant de demander et vérifier l'action que veut faire l'utilisateur dans l'auberge
@return le string de la réponse de l'utilisateur*)
let demander_action_auberge :  unit -> string = fun () ->
    let () = print_string "\n<?> " in
    let reponse = read_line () in
        if reponse = "A" || reponse = "a" || reponse = "D" || reponse = "d" || reponse = "O" || reponse = "o" || reponse="v" || reponse="V" || reponse ="p" || reponse ="P"
            then reponse 
            else let () = print_string "\n Votre choix est invalide ! \n" in demander_action ()

(** Fonction permettant d'avoir le string de la demande d'action par rapport à l'auberge
@return le string de la demande d'action*)
let afficher_action_aub : Personnage.Personnage.personnage -> string = fun perso ->
    (piece_sac perso)^"(A) Acheter des poulets à 4 Pièces l'unité \n(D) Dormir à l'auberge contre 10 Pièces \n(O) Observer l'auberge \n(V) Visualiser l'état de votre personnage\n (P) Partir \n"

(** Fonction permettant de générer aléatoirement une phrase de description de l'auberge
@return le string de la description de l'auberge *)
let phrase_texte_aub : unit -> string = fun () ->
		let lenb = Random.int 3 in
		match lenb with
		| 0 -> "Les fenêtres rondes et poussiéreuses de l'auberge laissent passer un maigre rayon de lumière donnant sur le comptoir en chène de l'accueil. \n
		En tournant la tête vers le bar, on peut appercevoir les quelques habitués du coin accoudés à celui-ci accumulant les chopes de bières. \n
		L'odeur acre de leur transpiration embaumant la piece et leur nez rougeots trahissent leur ivresse. L'un d'entre eux renverse sa cervoise et ronchonne fortement dans un langage approximatif. \n
		Un pichet encore humide a la main, l'aubergiste me fait un rapide haussement d'épaule comme pour me faire part de son habitude a tout cela. \n
		Il attrape le chiffon troué de son épaule et essuie le bro d'un mouvement certain. Eh bien, que voulez-vous ?"
		| 1 -> " L'accueil est assez chaleureux et chaque voyageur se voit invité à se réchauffer puis a prendre un repas réconfortant tandis qu'à l'extérieur , un vent glaçant continue a souffler. \n
		L'auberge est très propre et spacieuse, la salle publique est actuellement remplie, et les discussions vont bon train. \n
		La soirée se passe normalement et sans incidents majeurs, et vous en profiter pour faire connaissance , par le biais de concours de poésie, de chants ou de démonstration de force avec les autres aventuriers. \n
		 "
		| 2 -> "L'accueil de l'auberge est vide, vous n'entendez pas un bruit, il n'y aucun visiteur.\n
		Soudain, une araignée vous tombe sur le visage et vous apercevez des toiles d'araignées et toutes sortes d'insectes dans la pièce.\n 
		En vous dirigeant vers le bar vous voyez un vieux chat empaillé et la vision de celui ci vous effraie.\n
		Vous vous sentez mal à l'aise dans ce lieu mais c'était l'auberge la plus proche et vous êtes vraiment extenué.\n
		Vous retournez après du patron de l'auberge.\n" 
		|_ -> ""

			(**Fonction permettant de faire une phrase indiquant quelle quantité d'un contenu le personnage possède
		@param ctn : le contenu dont on indique la quantite
		@param qte : la quantite du contenu
		@return le string de la quantité du contenu*)
		let ph_qte_ctn : Personnage.Personnage.personnage -> Objet.Objet.contenu -> int -> string = fun perso ctn qte->
			" ~ Vous avez "^(string_of_int qte)^" "^(bien_ecrire_contenu perso qte ctn)^" dans votre sac ~ \n"

(** Fonction permettant d'avoir le string d'une ligne de stock avec la quantite de ce contenu dans le sac du personnage
@param bool : true si c'est pour achat, false si c'est pour la vente
@param qte : la quantite du contenu de la ligne de stock
@param ctn : le contenu de la ligne de stock
@param prix : le prix du contenu de la ligne de stock
@param perso : le personnage en interaction avec le marchand
@return le string de la ligne de stock avec la quantite de ce contenu dans le sac du personnage*)
		let ligne_stock : bool -> int->  int -> Objet.Objet.contenu -> Personnage.Personnage.personnage ->string = fun bool  qte prix ctn perso ->
			let sac = perso.sac in
			let laqte = string_of_int qte in
			let leprix = string_of_int prix in
			if bool=true then
				laqte^" "^(bien_ecrire_contenu perso qte ctn)^" : "^leprix^" "^(bien_ecrire_contenu perso prix Piece)^" l'unité."^(ph_qte_ctn perso ctn (Objet.Objet.qte_obj sac ctn) )
			else 
				(bien_ecrire_contenu perso 0 ctn)^" : "^leprix^" "^(bien_ecrire_contenu perso prix Piece)^" l'unité."^(ph_qte_ctn perso ctn (Objet.Objet.qte_obj sac ctn) )

	(** Fonction permettant d'avoir le string de l'affichage du stock du marchand
	@param marchand : le marchand dont on veut avoir le stock
	@param perso : le personnage communiquant avec le marchand
	@return le string du stock du marchand*)
		let contenu_stock : Marchand.Marchand.marchand -> Personnage.Personnage.personnage-> string = fun marchand perso->
			let stock = marchand.stock in
			let rec aux = fun stock affichage ->
			match stock with
			| [] -> affichage
			| Objet.Objet.{quantite = qte ; prix = prix ; obj = ctn}::tail when qte<=0 -> aux tail affichage
			| Objet.Objet.{quantite = qte ; prix = prix ; obj = ctn}::[] -> affichage^(ligne_stock true qte prix ctn perso)
			| Objet.Objet.{quantite = qte ; prix = prix ; obj = ctn}::tail -> aux tail (affichage^(ligne_stock true qte prix ctn perso)^"\n")
			in aux stock (piece_sac perso)^"    Voici ce que je propose à la vente...\n"

	(** Fonction permettant d'avoir le string de l'affichage du stock de vente du marchand
	@param marchand : le marchand dont on veut avoir le stock de vente
	@param perso : le personnage communiquant avec le marchand
	@return le string du stock de vente du marchand*)
		let contenu_stock_vente : Marchand.Marchand.marchand->Personnage.Personnage.personnage -> string = fun marchand perso->
				let vente= marchand.stock_vente in
				let rec aux : Objet.Objet.stock_vente -> string -> string = fun vente affichage ->
				match vente with
				| [] -> affichage
				| Objet.Objet.{prix = prix ; obj = ctn}::[] -> affichage^(ligne_stock false 0 prix ctn perso)
				| Objet.Objet.{prix = prix ; obj = ctn}::tail -> aux tail (affichage^(ligne_stock false 0 prix ctn perso)^"\n")
				in aux vente "    Voici ce que je peux vous acheter...\n"

(** Fonction permettant de générer aléatoirement une phrase de présentantation du marchand, avec son stock de vente et son stock
@param marchand : le marchand dont on veut le nom
@return le string de la phrase de présentation*)				
let bjr_marchand : Marchand.Marchand.marchand -> Personnage.Personnage.personnage -> string = fun marchand perso ->
		let nom = marchand.nom in
		let lenb = Random.int 3 in
		match lenb with
		| 0 -> "\"Bien le bonjour ! Je suis "^nom^" !\n"^(contenu_stock_vente marchand perso)^"\"\n"^(piece_sac perso)
		| 1 -> "\"Quel bonheur de voir quelqu'un ici ! Désolé, je ne me suis pas présenté, je suis "^nom^" !\n"^(contenu_stock marchand perso)^"\n"^(contenu_stock_vente marchand perso)^"\"\n"^(piece_sac perso)
		| 2 -> "\"Bonjour je suis "^nom^" ! J'ai hâte de pouvoir faire affaire avec vous !\n"^(contenu_stock marchand perso)^"\n"^(contenu_stock_vente marchand perso)^"\"\n"^(piece_sac perso)
		|_ -> ""
	
(** Fonction permettant de générer aléatoirement une phrase de d'initialisation du marchand
@param marchand : le marchand dont on veut une phrase d'initialisation
@return le string de la phrase d'initialisation*)	
		let phrase_init_marchand : Marchand.Marchand.marchand -> string = fun marchand ->
			let lenb = Random.int 3 in
			match lenb with
			| 0 -> "Vous appercevez un marchand itinérant non loin de vous. \n"
			| 1 -> "\"Eh vous là-bas !\" crie quelqu'un derrière vous. Vous vous retournez et reconnaissez un marchand. \n"
			| 2 -> "Vous attendez une petite voix chantonnant près de vous. C'est un vieux marchand trainant sa marchandise. \n"
			|_ -> ""
		
		(** Fonction permettant de demander et vérifier l'action que veut faire le personnage face au marchand
		@return le string de la reponse *)
		let demander_action_marchand :  unit -> string = fun () ->
				let () = print_string "\n<?> " in
				let reponse = read_line () in
						if reponse = "A" || reponse = "a" || reponse = "s" || reponse = "S" || reponse = "P" || reponse = "p" || reponse = "V" || reponse ="v"
								then reponse 
								else let () = print_string "\n Votre choix est invalide ! \n" in demander_action_marchand ()
		
		(** Fonction permettant d'avoir le string de la demande d'action du personnage face à l'aubergiste*)
		let afficher_action_marchand : unit -> string = fun () ->
				phrase_demande_action()^"\n(A) Acheter \n(S) Vendre \n(V) Visualiser l'état de votre personnage \n(P) Partir"
	
		(** Fonction permettant de d'avoir une liste des reponses possibles de l'utilisateur face au marchand par rapport à un contenu
		@param ctn : le contenu que l'on veut ajouter a la liste des reponses
		@return la liste de la reponse correspondant au contenu*)
		let match_lettres : Objet.Objet.contenu -> string list = fun ctn ->
			match ctn with
			| Eponge -> ["eponge";"Eponge"]
			| Poulet -> ["poulet";"Poulet"]
			| Potion_Precision -> ["precision"; "Precision"]
			| Potion_Puissance -> ["puissance"; "Puissance"]
			| Arme (G (Epee_en_bois)) -> ["bois";"Bois"]
			| Arme (G (Epee_de_nuada)) -> ["nuada"; "Nuada"]
			| Arme (G (Kusanagi_et_Yata_no_kagami)) -> ["Kusanagi"; "kusanagi"]
			| Arme (G (Aegis)) -> ["aegis";"Aegis"]
			| Arme (G (Durandal)) ->["durandal"; "Durandal"]
			| Arme (G (Excalibur)) ->["excalibur";"Excalibur"]
			| Arme (A (Arc_en_bois)) -> ["bois";"Bois"]
			| Arme (A (Arc_Artemis)) -> ["artemis";"Artemis"]
			| Arme (A (Gandiva)) -> ["gandiva";"Gandiva"]
			| Arme (A (Arc_bouclier_immortel)) -> ["bouclier";"Bouclier"]
			| Arme (A (Zephyr)) -> ["zephyr,Zephyr"]
			| Arme (A (Arc_de_lumiere)) -> ["lumiere";"Lumiere"]
			| Arme (M (Baton_en_bois)) -> ["bois";"Bois"]
			| Arme (M (Gae_bolga)) -> ["Gae";"gae"]
			| Arme (M (Caducee)) -> ["Caducee";"caducee"]
			| Arme (M (Voile_Ino)) -> ["ino";"Ino"]
			| Arme (M (Gambanteinn)) -> ["gambanteinn";"Gambanteinn"]
			| Arme (M (Baguette_de_sureau)) -> ["Sureau";"sureau"]
			|_ -> []

		(** Fonction permettant de récupérer la liste complète des réponses possibles par l'utilisateur face au marchand lors d'un achat
		@param marchand : le marchand 
		@return la liste de string des réponses possibles pour l'achat*)
		let les_lettres_achat : Marchand.Marchand.marchand -> string list = fun marchand ->
			let stock = marchand.stock in
			let rec aux : Objet.Objet.stock -> string list -> string list = fun stock liste ->
				match stock with
				| [] -> []
				| Objet.Objet.{quantite = q ; prix = p ; obj = type_objet_marchand}::tail when q<=0 -> aux tail liste
				| Objet.Objet.{quantite = q ; prix = p ; obj = type_objet_marchand}::tail -> aux tail liste@(match_lettres type_objet_marchand)
			in aux stock ["retour";"Retour"]
		
		(** Fonction permettant de récupérer la liste complète des réponses possibles par l'utilisateur face au marchand lors d'une vente
		@param marchand : le marchand 
		@return la liste de string des réponses possibles pour la vente*)
		let les_lettres_vente : Marchand.Marchand.marchand -> string list = fun marchand ->
			let stock = marchand.stock_vente in
			let rec aux : Objet.Objet.stock_vente -> string list -> string list = fun stock liste ->
				match stock with
				| [] -> []
				| Objet.Objet.{prix = p ; obj = type_objet_marchand}::tail -> aux tail liste@(match_lettres type_objet_marchand)	
			in aux stock ["retour";"Retour"]

		(** Fonction permettant de demander et vérifier la réponse de l'utilisateur par rapport a ce qu'il veut acheter
		@param marchand : le marchand auquel le personnage veut acheter quelque chose
		@return le string de la réponse de l'utilisateur/*)
		let rec demander_achat :  Marchand.Marchand.marchand -> string = fun marchand ->
					let () = print_string "\n<?> " in
					let reponse = read_line () in
							if (List.mem reponse (les_lettres_achat marchand))
									then reponse 
									else let () = print_string "\n Votre choix est invalide ! \n" in demander_achat marchand
		
		(** Fonction permettant de demander et vérifier la réponse de l'utilisateur par rapport a ce qu'il veut vendre 
		@param marchand : le marchand auquel le personnage veut vendre quelque chose
		@return le string de la réponse de l'utilisateur/*)
		let rec demander_vente :  Marchand.Marchand.marchand -> string = fun marchand ->
			let () = print_string "\n<?> " in
			let reponse = read_line () in
					if (List.mem reponse (les_lettres_vente marchand))
							then reponse 
							else let () = print_string "\n Votre choix est invalide ! \n" in demander_vente marchand
		
		(** Fonction permettant de faire l'affichage d'une ligne sur la quantite d'un contenu que le personnage a dans son sac
		@param perso : le personnage
		@param ctn : le contenu dont on veut la quantite*)
		let match_affichage : Personnage.Personnage.personnage -> Objet.Objet.contenu -> string = fun perso ctn->
			let qte = string_of_int (Objet.Objet.qte_obj perso.sac ctn) in 
			match ctn with
			| Eponge -> "(Eponge) Eponge ~ "^qte^" dans votre sac ~"
			| Poulet -> "(Poulet) Poulet ~ "^qte^" dans votre sac ~"
			| Potion_Precision -> "(Precision) Potion de précision ~ "^qte^" dans votre sac ~"
			| Potion_Puissance -> "(Puissance) Potion de puissance ~ "^qte^" dans votre sac ~"
			| Arme (G (Epee_en_bois)) -> "(Bois) Arme : "^(afficher_arme_guerrier Epee_en_bois)
			| Arme (G (Epee_de_nuada)) -> "(Nuada) Arme : "^(afficher_arme_guerrier Epee_de_nuada)
			| Arme (G (Kusanagi_et_Yata_no_kagami)) -> "(Kusanagi) Arme : "^(afficher_arme_guerrier Kusanagi_et_Yata_no_kagami)
			| Arme (G (Aegis)) ->"(Aegis) Arme : "^(afficher_arme_guerrier Aegis)
			| Arme (G (Durandal)) ->"(Durandal) Arme : "^(afficher_arme_guerrier Durandal)
			| Arme (G (Excalibur)) ->"(Excalibur) Arme : "^(afficher_arme_guerrier Excalibur)
			| Arme (A (Arc_en_bois)) -> "(Bois) Arme : "^(afficher_arme_archer Arc_en_bois)
			| Arme (A (Arc_Artemis)) -> "(Artemis) Arme : "^(afficher_arme_archer Arc_Artemis)
			| Arme (A (Gandiva)) -> "(Gandiva) Arme : "^(afficher_arme_archer Gandiva)
			| Arme (A (Arc_bouclier_immortel)) -> "(Bouclier) Arme : "^(afficher_arme_archer Arc_bouclier_immortel)
			| Arme (A (Zephyr)) -> "(Zephyr) Arme : "^(afficher_arme_archer Zephyr)
			| Arme (A (Arc_de_lumiere)) -> "(Lumiere) Arme : "^(afficher_arme_archer Arc_de_lumiere)
			| Arme (M (Baton_en_bois)) -> "(Bois) Arme : "^(afficher_arme_magicien Baton_en_bois)
			| Arme (M (Gae_bolga)) -> "(Gae) Arme : "^(afficher_arme_magicien Gae_bolga)
			| Arme (M (Caducee)) -> "(Caducee) Arme : "^(afficher_arme_magicien Caducee)
			| Arme (M (Voile_Ino)) -> "(Ino) Arme : "^(afficher_arme_magicien Voile_Ino)
			| Arme (M (Gambanteinn)) -> "(Gambanteinn) Arme : "^(afficher_arme_magicien Gambanteinn)
			| Arme (M (Baguette_de_sureau)) -> "(Sureau) Arme : "^(afficher_arme_magicien Baguette_de_sureau)
			|_ -> ""


		(** Fonction permettant d'afficher ce que peut acheter le personnage chez le marchand
		@param perso : le personnage
		@param marchand : le marchand 
		@return le string de ce que peut acheter le personnage chez le marchand*)
		let afficher_acheter : Personnage.Personnage.personnage -> Marchand.Marchand.marchand -> string = fun perso marchand ->
			let stock = marchand.stock in
			let rec aux = fun stock affichage ->
				match stock with
				| [] -> affichage
				| Objet.Objet.{quantite = q ; prix = p ; obj = ctn}::tail when q<=0 -> aux tail affichage
				| Objet.Objet.{quantite = q ; prix = p ; obj = ctn}::[] -> affichage^(match_affichage perso ctn)^"\n"
				| Objet.Objet.{quantite = q ; prix = p ; obj = ctn}::tail -> aux tail (affichage^(match_affichage perso ctn)^"\n")
				in aux stock "    "^(piece_sac perso)^"    Que souhaitez-vous acheter ?\n"

			
		(** Fonction permettant d'afficher ce que peut vendre le personnage au marchand
		@param perso : le personnage
		@param marchand : le marchand 
		@return le string de ce que peut vendre le personnage au marchand*)
		let afficher_vendre : Personnage.Personnage.personnage -> string = fun perso -> 
			let sac = perso.sac in
				let rec aux : Objet.Objet.sac -> string -> string = fun sac affichage ->
				match sac with
				| [] -> affichage
				| Objet.Objet.{quantite = qte ; obj = _}::tail when qte<=0 -> aux tail affichage
				| Objet.Objet.{quantite = qte ; obj = ctn}::[] -> affichage^(match_affichage perso ctn)
				| Objet.Objet.{quantite = qte ; obj = ctn }::tail -> aux tail (affichage^(match_affichage perso ctn)^"\n")
				in aux sac "    Que souhaitez-vous vendre ?\n"
		
		(** Fonction permettant de demander la quantite que le personnage souhaite acheter ou vendre
		@return la quantite que le personnage souhaite acheter*)
		let rec demander_qte : unit -> int = fun () ->
			let () = print_string "\n<?>" in
				try read_int ()
				with Failure _ -> let () = print_string "\nVous devez rentrer un nombre. \n" in demander_qte ()

		(** Fonction permettant d'avoir le string de la question de la quantite a acheter*)
		let afficher_demander_qte_achat : unit -> string = fun() -> "\nCombien en voulez-vous ?\n"

	(** Fonction permettant d'avoir le string de la question de la quantite a vendre*)
	let afficher_demander_qte_vente : unit -> string = fun () -> "\nCombien voulez-vous en vendre ?\n"

	(** Fonction qui dit que l'on n'a pas assez d'argent pour un achat*)
	let afficher_pas_assez_argent : unit -> string = fun() ->
		"\nDésolé mais vous n'avez pas les moyens pour cet achat.\n"

	(** Fonction permettant d'avoir une phrase de confirmation d'achat
	@param perso : le personnage qui a acheté quelque chose
	@param marchand : le marchand auquel le personnage a acheté quelque chose
	@param ctn : le contenu que le personnage a acheté
	@param qte : la quantite d'objet que le personnage a acheté*)
	let phrase_vente_marchand_valide : Personnage.Personnage.personnage -> Marchand.Marchand.marchand -> Objet.Objet.contenu -> int -> string = fun perso marchand ctn qte ->
		let stock = marchand.stock in
		let leprix = Objet.Objet.prix_obj_stock stock ctn in
		let prix = leprix*qte in
		"Vous venez d'acheter "^(string_of_int qte)^" "^(bien_ecrire_contenu perso qte ctn)^" pour "^(string_of_int prix)^" "^(bien_ecrire_contenu perso prix Piece)^"."
	
	(** Fonction permettant d'avoir une phrase de confirmation d'achat
	@param perso : le personnage qui a acheté quelque chose
	@param marchand : le marchand auquel le personnage a acheté quelque chose
	@param ctn : le contenu que le personnage a acheté
	@param qte : la quantite d'objet que le personnage a acheté*)
	let phrase_vente_marabout_valide : Personnage.Personnage.personnage -> Marabout.Marabout.marabout-> Objet.Objet.contenu -> int -> string = fun perso mara ctn qte ->
		let stock = mara.stock_vente in
		let leprix = Objet.Objet.prix_obj_stock_vente stock ctn in
		let prix = leprix*qte in
		"Vous venez d'acheter "^(string_of_int qte)^" "^(bien_ecrire_contenu perso qte ctn)^" pour "^(string_of_int prix)^" "^(bien_ecrire_contenu perso prix Piece)^"."
	
	(** Fonction permettant d'avoir une phrase de confirmation de vente
	@param perso : le personnage qui a vendu quelque chose
	@param marchand : le marchand auquel le personnage a vendu quelque chose
	@param ctn : le contenu que le personnage a vendu
	@param qte : la quantite d'objet que le personnage a vendu*)
	let phrase_achat_valide : Personnage.Personnage.personnage -> Marchand.Marchand.marchand -> Objet.Objet.contenu -> int -> string = fun perso marchand ctn qte ->
		let stock = marchand.stock_vente in
		let leprix = Objet.Objet.prix_obj_stock_vente stock ctn in
		let prix = leprix*qte in
		"Vous venez de vendre "^(string_of_int qte)^" "^(bien_ecrire_contenu perso qte ctn)^" pour "^(string_of_int prix)^" "^(bien_ecrire_contenu perso prix Piece)^"."


(*-----------------marabout-----------------*)

(** Fonction permettant de générer aléatoirement un texte de lecture d'avenir par le marabout
@return le string du texte*)
let phrase_avenir : unit -> string = fun () ->
	let lenb = Random.int 4 in
	match lenb with
	| 0 -> "\"Vous souhaitez donc connaître votre situation future...\n
	Mais, vous savez, moi je ne crois pas qu’il y ait de bonne ou de mauvaise situation. \n
	Moi, si je devais résumer ma vie aujourd’hui avec vous, je dirais que c’est d’abord des rencontres. \n
	Des gens qui m’ont tendu la main, peut-être à un moment où je ne pouvais pas, où j’étais seul chez moi. \n
	Et c’est assez curieux de se dire que les hasards, les rencontres forgent une destinée… \n
	Parce que quand on a le goût de la chose, quand on a le goût de la chose bien faite, le beau geste, parfois on ne trouve pas l’interlocuteur en face je dirais, le miroir qui vous aide à avancer. \n
	Alors ça n’est pas mon cas, comme je disais là, puisque moi au contraire, j’ai pu : et je dis merci à la vie, je lui dis merci, je chante la vie, je danse la vie… je ne suis qu’amour ! \n
	Et finalement, quand beaucoup de gens aujourd’hui me disent « Mais comment fais-tu pour avoir cette humanité ? », \n
	et bien je leur réponds très simplement, je leur dis que c’est ce goût de l’amour, ce goût donc qui m’a poussé aujourd’hui à entreprendre une construction mécanique, mais demain qui sait ? \n
	Peut-être simplement à me mettre au service de la communauté, à faire le don, le don de soi…\""
	|1 -> "\"Eh bien, encore un curieux ! Asseyez-vous donc ! Je suis un peu pressé mais je veux bien lire votre avenir dans...\"\
	* Le marabout regarde autour de soit et attrape un torchon miteux * \n
	\"...ce chiffon ! Ca fera l'affaire ! \n
	* L'homme ferme les yeux et passe ses mains au dessus du morceau de tissu *
	Alors bon... mmh, oui, je vois... je vois... une femme... heu non, en fait on dirait un homme... quoique...\n
	Enfin voilà, il y a quelqu'un, surement une personne que vous connaissez, je ne sais pas moi... \n
	Ah ! Misère, elle vient de disparaitre ! Comme c'est dommage, elle était juste là... Enfin bon, moi, je n'y peux rien...\n
	Bon, eh bien je vous laisse... Et n'oubliez pas de me laisser un pourboire en sortant !\""
	| 2 -> "\"Je n'osais pas vous le dire, mais vous faites bien de me demander cela. Je sens une grande énergie émaner autour de vous !\n
	Les esprits puissants de nos ancêtres sont accrochés à votre personne. Ils vont apporteront soutient et chance tout au long de votre vie !\n
	N'oubliez pas que les étoiles vous guideront, ce sont nos aïeuls qui les ont placées là, dans le ciel.\n
	Votre personne regorge de potentiel, ne vous laissez pas emporter dans le côté obscur..."
	| 3 -> "* Le marabout sort une boule de cristal de son armoire et rentre dans une transe qui ferait peur à plus d'un. Sa voix change brusquement, on dirait un autre homme *\n
	\"Il doit avoir l’engagement le plus profond, l’esprit le plus sérieux. \n
	Celui-ci, depuis très longtemps je l’observe et toute sa vie, il a regardé vers l’avenir, vers l’horizon. \n
	Jamais l’esprit là où il était, hum! A ce qu’il faisait. \n
	La peur est le chemin vers le côté obscur : la peur mène à la colère, la colère mène à la haine, la haine … mène à la souffrance. 
	Un grand guerrier ? Personne par la guerre ne devient grand. \n
	* Le marabout reprend ses esprits et me regarde * \n
	\" Voilà, j'espère que ce qu'il vous aura dit vous guidera... "
	| _ -> ""

	(**Fonction permettant d'afficher le stock du marabout
		@param mara : le marabout
		@param perso : le personnage en interaction avec le marabout
		@return le string du stock du marabout*)
		let mara_contenu_stock_vente : Marabout.Marabout.marabout-> Personnage.Personnage.personnage -> string = fun mara perso ->
			let vente = mara.stock_vente in
			let rec aux : Objet.Objet.stock_vente -> string -> string = fun vente affichage ->
			match vente with
			| [] -> affichage
			| {prix = prix ; obj = ctn}::[] -> affichage^(ligne_stock false 0 prix ctn perso)^"\n"
			| {prix = prix ; obj = ctn}::tail -> aux tail (affichage^(ligne_stock false 0 prix ctn perso)^"\n")
			in aux vente "Faire une prédiction de votre avenir.\n Acheter une potion :\n       Vous ne pouvez achetez une potion pour améliorer votre puissance ou votre précision seulement si vous n'en possédez pas déjà.\n       Celles-ci pourront vous apporter des bonus avant un combat mais attention, ces potions, si elles sont mal préparées, pourront vous faire perdre en efficacité.\n"

	(** Fonction permettant de générer aléatoirement une phrase d'initialisation du marabout
	@param mara : le marabout
	@return le string de la phrase d'initialisation*)
	let phrase_init_marabout : Personnage.Personnage.personnage -> Marabout.Marabout.marabout -> string = fun perso mara ->
		let lenb=Random.int 3 in
		match lenb with
		| 0 -> "Bien le bonjour ! Je suis le marabout "^mara.nom^"."^(question_service ())^"\n"^(mara_contenu_stock_vente mara perso)^"\n"
		| 1 -> "Bienvenue à vous ! Je me présente, je suis le marabout "^mara.nom^"."^(question_service ())^"\n"^(mara_contenu_stock_vente mara perso)^"\n"
		| 2 -> "Bonjour ! Laissez-moi me présenter ! Je suis le marabout "^mara.nom^"."^(question_service ())^"\n"^(mara_contenu_stock_vente mara perso)^"\n"
		|_ -> ""

		(**Fonction permettant de demander une action par rapport au village
		@return la reponse de l'utilisateur sous forme d'un string*)
		let demander_action_village :  unit -> string = fun () ->
			let () = print_string "\n<?> " in
			let reponse = read_line () in
					if reponse = "A" || reponse = "a" || reponse = "m" || reponse = "M" || reponse = "p" || reponse = "P" || reponse = "f" || reponse ="F" || reponse="v" || reponse ="V"
							then reponse 
							else let () = print_string "\n Votre choix est invalide ! \n" in demander_action_village ()

	(** Fonction permettant d'avoir le string de la demande d'action du personnage par rapport au village
	@return le string de la demande *)
	let phrase_village_action : unit -> string = fun () ->
		"Vous entrez dans le village...\nQue voulez-vous faire ?\n\n(A) Aller faire un tour à l'auberge \n(M) Rendre visite au marabout du coin \n(F) Se rendre à la foire pour jouer à des jeux d'argent \n(V) Visualiser l'état de votre personnage\n(P) Partir du village "
			
		(** Fonction permettant d'afficher ce que peut acheter le personnage chez le marabout
		@param perso : le personnage
		@param marabout : le marabout 
		@return le string de ce que peut acheter le personnage chez le marabout*)
		let afficher_acheter_marabout : Personnage.Personnage.personnage -> Marabout.Marabout.marabout -> string = fun perso marabout ->
			let stock = marabout.stock_vente in
			let rec aux : Objet.Objet.stock_vente -> string -> string = fun stock affichage ->
				match stock with
				| [] -> affichage
				| {prix = p ; obj = ctn}::[] -> affichage^(match_affichage perso ctn)
				| {prix = p ; obj = ctn}::tail -> aux tail (affichage^(match_affichage perso ctn)^"\n")
				in let res = aux stock ((piece_sac perso)^"    Que souhaitez-vous acheter/faire ?\n (Prediction) Faire une prédiction de votre avenir pour 5 pièces. \n") in res^"(V) Visualiser l'état de votre personnage\n(P) Partir du village"

		(**Fonction permettant de demander une action par rapport au village
		@return la reponse de l'utilisateur sous forme d'un string*)
		let rec demander_action_marabout :  unit -> string = fun () ->
			let () = print_string "\n<?> " in
			let reponse = read_line () in
					if reponse = "Puissance" || reponse = "puissance" || reponse = "Precision" || reponse = "precision" || reponse = "Prediction" || reponse = "prediction" || reponse = "V" || reponse ="v" || reponse="P" || reponse ="p"
							then reponse 
							else let () = print_string "\n Votre choix est invalide ! \n" in demander_action_marabout ()

			(**Fonction permettant de demander une action par rapport au village
		@return la reponse de l'utilisateur sous forme d'un string*)
		let demander_action_village :  unit -> string = fun () ->
			let () = print_string "\n<?> " in
			let reponse = read_line () in
					if reponse = "A" || reponse = "a" || reponse = "m" || reponse = "M" || reponse = "p" || reponse = "P" || reponse = "f" || reponse ="F" || reponse="v" || reponse ="V"
							then reponse 
							else let () = print_string "\n Votre choix est invalide ! \n" in demander_action_village ()

	(** Fonction permettant de générer aléatoirement une phrase de d'initialisation du village
@return le string de la phrase d'initialisation*)    
let phrase_init_village : unit -> string = fun () ->
	let lenb = Random.int 3 in
	match lenb with
	| 0 -> "Vous appercevez des lumières au haut d'une colline : c'est un Village. \n"
	| 1 -> "Vous entendez des bruits et des cris de forrains derrière une forêt : c'est un Village. \n"
	| 2 -> "Vous croisez un petit panneau vétuste sur le bord d'un chemin : il indique qu'un village se trouve non loin de vous.\n"
	| _ -> ""

	let phrase_potion_bu : string -> int -> string = fun str nb ->
			let lenb = Random.int 2 in
			if str = "acc" then 
					if nb>0 then
					(match lenb with
					| 0 -> "Vous avez pris une Potion de Précision et celle-ci vous apporte "^(string_of_int nb)^" pourcents de précision.\n"
					| 1 -> "Vous buvez votre Potion de Précision. Elle vous fait gagner "^(string_of_int nb)^" pourcents de précision. \n"
					| _ -> "")
					else
					(match lenb with
					| 0 -> "Vous avez pris une Potion de Précision mais le marabout qui l'a préparé s'est trompé de recette. La potion vous retire "^(string_of_int (abs nb))^" pourcents de précision.\n"
					| 1 -> "Vous vous empoissonez avec la Potion ! Celle-ci vous fait perdre "^(string_of_int (abs nb))^" pourcents de précision.\n"
					| _ -> "")
			else 
					if nb>0 then
					(match lenb with
					| 0 -> "Vous avez pris une Potion de Puissance et celle-ci vous apporte "^(string_of_int nb)^" points de puissance.\n"
					| 1 -> "Vous buvez votre Potion de Puissance. Elle vous fait gagner "^(string_of_int nb)^" points de puissance. \n"
					| _ -> "")
					else 
					(match lenb with
					| 0 -> "Vous avez pris une Potion de Puissance mais le marabout qui l'a préparé s'est trompé de recette. La potion vous retire "^(string_of_int (abs nb))^" points de puissance.\n"
					| 1 -> "Vous vous empoissonez avec la Potion ! Celle-ci vous fait perdre "^(string_of_int (abs nb))^" points de puissance.\n"
					| _ -> "")


	end
;;