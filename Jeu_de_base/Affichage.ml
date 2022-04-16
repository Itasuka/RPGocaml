(*----------------------------------------------------------SIGNATURE----------------------------------------------------------*)

module type SIGAFFICHAGE = 
	sig

		(* Fonction permettant l'affichage de string *)
		val aff : string -> unit
		
		(* Fonction permettant d'avoir le string de l'affichage du sac *)
		val afficher_contenu_sac : Objet.Objet.sac -> string
		
		(* Fonction permettant d'avoir le string de l'affichage de l'état du personnage *)
		val afficher_personnage : Personnage.Personnage.personnage -> string

		(* Fonction permettant de générer aléatoirement un phrase lorsque le personnage mange *)
		val phrase_manger : unit -> string

		(* Fonction permettant de générer aléatoirement une phrase pour malheureuse_rencontre *)
		val phrase_intialisation_surprise : Monstre.Monstre.monstre -> string

		(* Fonction permettant de générer aléatoirement une phrase lorsque le monstre attaque *)
		val phrase_attaque_monstre : Monstre.Monstre.monstre -> float -> string

		(* Fonction permettant de générer aléatoirement la phrase de la tentative du personnage *)
		val phrase_tentative_perso : int->string

		(* Fonction permettant de générer aléatoirement une phrase de mort de monstre *)
		val phrase_mort_monstre : Monstre.Monstre.monstre->string

		(* Fonction permettant de générer aléatoirement une phrase de mort pour le personnage *)
		val phrase_mort_personnage : Personnage.Personnage.personnage -> Monstre.Monstre.monstre -> string

		(* Fonction permettant de générer une phrase lorsque le personnage meurt durant la nuit *)
		val phrase_mort_nuit : Personnage.Personnage.personnage -> Monstre.Monstre.monstre -> string

		(* Fonction permettant de générer aléatoirement une phrase lorsque le personnage dort sans mourir *)
		val phrase_nuit : Personnage.Personnage.personnage -> string

		(*Fonction permettant de générer aléatoirement une phrase lorsque le personnage perd un objet *)
		val phrase_perte_objet : int -> Objet.Objet.contenu -> string

		(* Fonction permettant de générer aléatoirement un événement avec un monstre *)
		val phrase_evenement_monstre : Monstre.Monstre.monstre -> string

		(* Fonction permettant de générer aléatoirement une phrase de passage de niveau *)
		val phrase_level_up : int -> string

		(* Fonction permettant de générer aléatoirement une question pour faire une action *)
		val phrase_demande_action : unit->string

		(* Fonction permettant de générer aléatoirement une phrase de gain de points d'expérience et d'objet *)
		val phrase_exp_obj_up : int -> Monstre.Monstre.loot -> string

		(* Fonction permettant d'avoir la phrase de la demande d'action *)
		val afficher_action : unit -> string

		(* Fonction permettant de demander à l'utilisateur un choix d'action *)
		val demander_action : unit -> string

		(* Fonction permettant d'avoir la phrase de la demande de réaction *)
		val afficher_reaction : unit -> string

		(* Fonction permmettant de demander à l'utilisateur un choix de réaction par rapport à un évènement *)
		val demander_reaction : unit -> string

		(* Fonction qui affiche l'introduction dans l'univers du jeu*)
		val debut_partie : unit -> string

		(* Fonction permettant de demander à l'utilisateur son nom*)
		val demander_nom : unit -> string

		(* Fonction permettant d'avoir la phrase de demande de genre de l'utilisateur*)
		val afficher_genre : string -> string

		(*Fonction permettant de demander à l'utilisateur son genre *)
		val demander_genre : unit -> string

		(* Fonction permettant d'avoir la demande à l'utilisateur la classe qu'il souhaite*)
		val afficher_classe : Personnage.Personnage.genre -> string
		
		(* Fonction permettant de demander à l'utilisateur sa classe *)
		val demander_classe : unit -> string

	end;;

(*------------------------------------------------------------MODULE----------------------------------------------------------------*)
		
module Affichage : SIGAFFICHAGE =
	struct
		
	(** Fonction permettant l'affichage de string
	@author Noemie L
	@param s : le string que l'on souhaite afficher*)
	let aff : string->unit = fun s ->
		print_string(s)
	
	(*----------------------------------------------------------AFFICHAGE----------------------------------------------------------------*)
	(*-------------------------------------------------------------ETAT------------------------------------------------------------------*)
	(*----------------------------------------------------------PERSONNAGE---------------------------------------------------------------*)

	(** Fonction produisant une ligne de tirets de la longueur souhaitée
	@author Noemie L
	@param nbfois : le nombre de tirets que l'on veut
	@return la ligne de nbfois tirets*)
	let ligne_de_tirets : int -> string = fun nbfois ->
		let nombrefois = nbfois-2 in 
		let rec aux = fun nb ligne ->
		if nb<=0 then ligne
		else aux (nb-1) (ligne^"-")
		in aux nombrefois ""

	(** Fonction produisant mettant des croix aux bornes d'un string
	@author Noemie L
	@param ligne : le string que l'on veut borner
	@return le string borné de tirets*)
	let borner_ligne_croix : string -> string = fun ligne ->
		"+"^ligne^"+"

	(** Fonction permettant de genrer la classe du personnage
	@author Noemie L
	@param perso : le personnage dont on souhaite genrer la classe
	@return le string de la classe genrée*)
	let genrer_classe : Personnage.Personnage.personnage -> string = fun perso ->
		let classe=perso.classe in
		match classe with 
		| Guerrier when perso.genre = Femme -> "Guerriere"
		| Guerrier -> "Guerrier"
		| Archer when perso.genre = Femme -> "Archere"
		| Archer  -> "Archer"
		| Magicien when perso.genre = Femme -> "Magicienne"
		| Magicien -> "Magicien"

	(** Fonction permettant de mettre au pluriel si besoin les différents contenus du sac et de mettre une quantité devant
	@author Noemie L
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
	@author Noémie L
	@param m : le monstre dont on veut convertir le nom
	@return le string du nom du monstre*)
	let string_monstre : Monstre.Monstre.monstre -> string = fun m -> match m.monstre with
    |Golem -> "golem"
    |Sanglier -> "sanglier"
    |Moustiques n-> "nuée de moustiques"
    
    
	(** Fonction permettant d'avoir le contenu du sac
	@author Noemie L
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
		
	(** Fonction permettant d'avoir la ligne de l'identité du personnage
	@author Noemie L
	@param perso : le personnage dont on souhaite afficher l'identité
	@return le string de l'affichage de l'identité*)
	let afficher_identite : Personnage.Personnage.personnage->string = fun perso ->
		"| "^(perso.nom)^" | "^(genrer_classe perso)^" niveau "^(string_of_int perso.lvl)^" |"

	(** Fonction permettant d'avoir la longueur de l'affichage de l'identité
	@author Noemie L
	@param perso : le personnage dont on souhaite avoir la longueur de l'affichage d'identité
	@return le integer de la longueur de l'affichage de l'identité*)
	let longueur_affichage : Personnage.Personnage.personnage->int = fun perso -> 
		String.length (afficher_identite perso)
		
	(** Fonction permettant de créer une ligne d'espaces d'une longueur souhaitée
	@author Noemie L
	@param nbfois : le nombre d'espaces qu'on souhaite avoir
	@return : le string de la ligne d'espaces*)
	let ligne_espace: int -> string = fun nbfois ->
		let rec aux = fun nbfois ligne ->
		if nbfois<=0 then ligne
		else aux (nbfois-1) (ligne^" ")
		in aux nbfois ""	

	(** Fonction permettant d'avoir la ligne des Points de vie du personnage
	@author Noemie L
	@param perso : le personnage dont on souhaite afficher la ligne de points de vie
	@return : le string de l'affichage de la ligne des points de vie du personnage*)
	let afficher_pv : Personnage.Personnage.personnage->string = fun perso ->
		let lg = longueur_affichage perso in
		let stringpv= "| Points de vie |" in
		let stringnbpv=(string_of_float perso.pv)^" |" in
		let nbespaces = lg-(String.length (stringpv) + String.length (stringnbpv)) in
		stringpv^ligne_espace nbespaces^stringnbpv

	(** Fonction permettant d'avoir la ligne de l'expérience du personnage
	@author Noemie L
	@param perso : le personnage dont on souhaite afficher de l'expérience
	@return : le string de l'affichage de la ligne de l'expérience du personnage*)	
	let afficher_exp : Personnage.Personnage.personnage->string = fun perso ->
		let lg = longueur_affichage perso in
		let lgstringpv=String.length "| Points de vie |" in
		let lgstringexp= String.length "| Experience |" in
		let nbespacesexp= lgstringpv-lgstringexp in
		let stringexp="| Experience"^ligne_espace nbespacesexp^" |" in
		let stringnbexp=(string_of_int perso.exp)^" |" in
		let nbespacesnbexp = lg-(String.length (stringexp) + String.length (stringnbexp)) in
		stringexp^(ligne_espace nbespacesnbexp)^stringnbexp
		
	(** Fonction permettant d'avoir les lignes du sac pour l'affichage de l'état du personnage
	@author Noemie L
	@param : perso : le personnage dont on veut l'affichage du sac
	@return : le string de l'affichage de les lignes du sac pour l'affichage de l'état du personnage*)
	let afficher_sac_pas_complet : Personnage.Personnage.personnage ->string = fun perso ->
		let lesac= perso.sac in
		let lg = longueur_affichage perso in 
		let rec aux : Objet.Objet.sac -> string -> string = fun sac affichage->
		match sac with
		| [] -> affichage
		| Objet.Objet.{quantite= qte ; obj = ctn}::tail when qte<=0 -> aux tail affichage
		| Objet.Objet.{quantite= qte ; obj = ctn}::tail ->
			let bec="|  "^(bien_ecrire_contenu qte ctn) in let lg2= String.length bec in aux tail (affichage^"\n"^bec^(ligne_espace (lg-lg2-1))^"|")
		in aux lesac "" 
	
	(** Fonction permettant de savoir si le personnage possède quelque chose dans son sac
	@author Noemie L
	@param perso : le personnage dont on veut savoir si il possède quelque chose dans son sac
	@return true si il possède quelque chose false sinon*)
	let bool_afficher_sac_ou_pas : Personnage.Personnage.personnage -> bool = fun perso ->
			let str = afficher_sac_pas_complet perso in
			if str = "" then false
			else true 

	(** Fonction permettant d'avoir la ligne du sac pour l'affichage de l'état du personnage
	@author Noemie L
	@param perso : le personnage dont on veut afficher l'état
	@return : le string de la ligne du sac pour l'affichage de l'état du personnage*)
	let afficher_partie_sac : Personnage.Personnage.personnage ->string = fun perso ->
		let lg = longueur_affichage perso in 
		let sac = "| Sac" in
		let lgsac=String.length sac in
		sac^(ligne_espace (lg-1-lgsac))^"|"

	(** Fonction permettant d'avoir la partie sac pour l'affichage de l'état du personnage
	@author Noemie L
	@param perso : le personnage dont on veut afficher le sac 
	@return : le string de l'affichage de la partie sac pour l'affichage de l'état du personnage*)	
	let afficher_sac : Personnage.Personnage.personnage->string = fun perso ->
		if (bool_afficher_sac_ou_pas perso)=false then (afficher_partie_sac perso)
		else (afficher_partie_sac perso)^(afficher_sac_pas_complet perso)

	(**Fonction permettant d'avoir l'état du personnage
	@author Noemie L
	@param perso : le personnage dont on veut afficher l'état
	@return : le string de l'affichage de l'état du personnage*)
	let afficher_personnage : Personnage.Personnage.personnage -> string = fun perso -> 
		let lg = longueur_affichage perso in
		let ligne_croix = borner_ligne_croix (ligne_de_tirets lg) in
		ligne_croix^"\n"^(afficher_identite perso)^"\n"^ligne_croix^"\n"^(afficher_pv perso)^"\n"^ligne_croix^"\n"^(afficher_exp perso)^"\n"^ligne_croix^"\n"^(afficher_sac perso)^"\n"^ligne_croix
	
  (*-----------------------------------------------------------ACCORDS-----------------------------------------------------------------*)
	(*---------------------------------------------------------DETERMINANTS--------------------------------------------------------------*)

(** Fonction permettant de mettre un article indefini devant le type du monstre
@author Noemie L
@param monstre : le monstre pour lequel on veut l'article indefini
@return : le string de l'article indefini avec le type du monstre*)
let un_nom_monstre : Monstre.Monstre.monstre->string = fun monstre ->
	let m = monstre.monstre in
	match m with
	| Golem -> "un Golem"
	| Moustiques n when n=1-> "une Nuée de "^(string_of_int n)^" moustique"
	| Moustiques n -> "une Nuée de "^(string_of_int n)^" moustiques"
	| Sanglier -> "un Sanglier"

(** Fonction permettant de mettre un article defini devant le type du monstre*
@author Noemie L
@param monstre : le monstre pour lequel on veut l'article defini
@return : le string de l'article defini avec le type du monstre*)
let le_nom_monstre : Monstre.Monstre.monstre->string = fun monstre ->
	let m = monstre.monstre in
	match m with
	| Golem -> "le Golem"
	| Moustiques n when n=1-> "la Nuée de "^(string_of_int n)^" moustique"
	| Moustiques n -> "la Nuée de "^(string_of_int n)^" moustiques"
	| Sanglier -> "le Sanglier"

(** Fonction permettant de mettre un article demonstratif devant le type de monstre
@author Noemie L
@param monstre : le monstre pour lequel on veut l'article demonstratif
@return le string de l'article demonstratif avec le type du monstre*)
let ce_nom_monstre : Monstre.Monstre.monstre->string = fun monstre ->
	let m = monstre.monstre in
	match m with
	| Golem -> "ce Golem"
	| Moustiques n when n=1-> "cette Nuee de "^(string_of_int n)^" moustique"
	| Moustiques n -> "cett Nuee de "^(string_of_int n)^" moustiques"
	| Sanglier -> "cet Sanglier"

(** Fonction permettant d'accorder au féminin et masculin un mot selon le genre du personnage
@author Noemie L
@param perso : le personnage dont on veut genrer le mot
@param mot : le mot que l'on souhaite genrer
@return le string du mot genrer*)
let accord_fem_masc : Personnage.Personnage.personnage -> string -> string = fun perso mot -> match perso.genre with
    |Homme -> mot
    |Femme -> mot^"e"

(**Fonction permettant d'avoir le pronom personnel correspondant au monstre
@author Noemie L
@param monstre : le monstre dont on souhaite avoir le pronom personnel
@return le string du pronom personnel*)
let pronom_monstre : Monstre.Monstre.monstre -> string = fun monstre -> 	match monstre.monstre with
    |Golem -> "Il"
    |Sanglier -> "Il"
    |Moustiques n -> "Elle"

(**Fonction permettant de mettre au pluriel un mot si besoin (selon un int)
@author Noemie L
@param nb : le nombre (int) permettant de savoir si il y a plusieurs fois le mot
@param mot : le mot que l'on souhaite mettre au pluriel ou pas*)
let accord_pluriel_int : int->string->string = fun nb mot ->
	match nb with
	| n when n>1 -> mot^"s"
	| _ -> mot

(**Fonction permettant de mettre au pluriel un mot si besoin (selon un float)
@author Noemie L
@param nb : le nombre (float) permettant de savoir si il y a plusieurs fois le mot
@param mot : le mot que l'on souhaite mettre au pluriel ou pas*)
let accord_pluriel_float : float->string->string = fun nb mot ->
	match nb with
	| n when n>1. -> mot^"s"
	| _ -> mot

(*------------------------------------------------------------PHRASES-----------------------------------------------------------------*)
(*----------------------------------------------------------ALEATOIRES----------------------------------------------------------------*)
(*--------------------------------------------------------------JEU-------------------------------------------------------------------*)

(** Fonction permettant de donner aléatoirement une phrase pour malheureuse_rencontre
@author Noemie L
@param monstre : le monstre de la malheureuse rencontre
@return le string de la phrase pour la malheureuse rencontre*)
let phrase_intialisation_surprise : Monstre.Monstre.monstre->string = fun monstre ->
	let lenb=Random.int 3 in
	match lenb with
	| 0 -> "Soudain, vous tombez nez à nez avec "^(un_nom_monstre monstre)^".\n"
	| 1 -> "Tout à coup, "^(un_nom_monstre monstre)^" surgit.\n"
	| 2 -> "D’un seul coup, vous vous retrouver face à face à "^(un_nom_monstre monstre)^".\n"
	| _ -> ""

(** Fonction permettant de générer aléatoirement un phrase lorsque le personnage mange
@author Noemie L
@return le string de la phrase aléatoire*)
let phrase_manger : unit -> string = fun () ->
	let ran = Random.int 3 in
		match ran with 
		|0 -> "\nVous avez mangé un poulet du RU et vous regagnez 2 points de vie.\n"
		|1 -> "\nLe poulet que vous venez de manger était vraiment bon ! Vous gagnez 2 points de vie.\n"
		|_ -> "\nVous engloutissez le poulet et rôtez un coup afin de gagner 2 points de vie !\n"

(** Fonction permettant de générer aléatoirement une phrase de tentative d'attaque de monstre avec le nombre de points de vie retirés au personnage
@author Noemie L
@param monstre : le monstre qui va faire l'attaque
@param pv : le nombre de points de vie retirés au personnage
@return le string de la tentative du monstre avec le nombre de points de vie retirés au personnage*)
let phrase_tentative_monstre : Monstre.Monstre.monstre->float->string = fun monstre pv ->
	let pv_abs = abs_float pv in
	let pv_retires = string_of_float pv_abs in
	let lenb=Random.int 3 in
	match lenb with
	| 0 -> "vous touche et vous perdez "^pv_retires^" "^(accord_pluriel_float pv_abs "point")^" de vie.\n"
	| 1 -> "vous blesse et vous retire "^pv_retires^" "^(accord_pluriel_float pv_abs "point")^" de vie.\n"
	| 2 -> "fait mouche et vous ote "^pv_retires^" "^(accord_pluriel_float pv_abs "point")^" de vie.\n"
	| _ -> ""

(** Fonction permettant de générer aléatoirement une phrase lorsque le monstre attaque
@author Noemie L
@param monstre : le monstre qui attaque
@param pv_retires : le nombre de points de vie retirés au personnage
@return le string de l'attaque du monstre*)
let phrase_attaque_monstre : Monstre.Monstre.monstre->float->string = fun monstre pv_retires ->
	let lenb=Random.int 3 in
	match lenb with
	| 0 -> (le_nom_monstre monstre)^" tente de vous blesser. "^ (pronom_monstre monstre)^" "^(phrase_tentative_monstre monstre pv_retires)
	| 1 -> (le_nom_monstre monstre)^" vous attaque violemment. "^(pronom_monstre monstre)^" "^(phrase_tentative_monstre monstre pv_retires)
	| 2 -> (le_nom_monstre monstre)^" vous charge brutalement. "^(pronom_monstre monstre)^" "^(phrase_tentative_monstre monstre pv_retires)
	| _ -> ""

(**Fonction permettant de générer aléatoirement la phrase de la tentative du personnage
@author Noemie L
@param pv : le nombre de points de vie retirés au monstre
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

(** Fonction permettant de générer aléatoirement une phrase de mort de monstre
@author Noemie L
@param monstre : le monstre mourrant
@return le string de la phrase de la mort du monstre*)
let phrase_mort_monstre : Monstre.Monstre.monstre->string = fun monstre ->
	let lenb=Random.int 3 in
	match lenb with
	| 0 -> "\n"^(le_nom_monstre monstre)^" meurt dans d’affreuses (vraiment affreuses) souffrances.\n"
	| 1 -> "\nL’horrible "^(string_monstre monstre)^" tombe à terre en laissant planer un silence de mort.\n"
	| 2 -> "\n“O désespoir ! Vous êtes bien trop fort !” hurle "^(le_nom_monstre monstre)^" en s’effondrant au sol...\n"
	| _ -> ""

(** Fonction permettant de générer aléatoirement une phrase de mort pour le personnage
@author Noemie L
@param perso : le personnage mourrant
@param mosntre : le monstre ayant tué le personnage
@return le string de la phrase de la mort du personnage*)
let phrase_mort_personnage : Personnage.Personnage.personnage-> Monstre.Monstre.monstre->string = fun perso monstre->
	let lenb=Random.int 3 in
	match lenb with
	| 0 -> "\nVous tombez au combat. D’autre loueront vos exploits mais aujourd’hui "^(ce_nom_monstre monstre)^" a entaché votre réputation à tout jamais.\n"
	| 1 -> "\nVous êtes "^(accord_fem_masc perso ("mort"))^".\n"
	| 2 -> "\nVous mourrez dans d’affreuses souffrances.\n"
	| _ -> ""

(** Fonction permettant de générer une phrase lorsque le personnage meurt durant la nuit
@author Noemie L
@param perso : le personnage allant mourir
@param monstre : le monstre tuant le personnage
@return le string de la phrase de mort du personnage*)
let phrase_mort_nuit : Personnage.Personnage.personnage-> Monstre.Monstre.monstre->string = fun perso monstre->
	match monstre.monstre with
		| Golem -> "\nVous installez votre campement et tombez rapidement "^(accord_fem_masc perso ("endormi"))^". Pendant votre sommeil, un Golem surgit et vous fracasse le crane. Vous êtes "^(accord_fem_masc perso ("mort"))^".\n"
		| Moustiques n -> "\nVous êtes "^(accord_fem_masc perso ("fatigué"))^" et décidez de dormir à la belle étoile prêt d’un marecage. Cependant, une Nuée de moustique apparait et vous pique inlassablement jusqu’à vous tuer. Vous êtes "^(accord_fem_masc perso ("mort"))^".\n"
		| _ -> "\nVos nombreux baillements vous poussent à installer un campement pour dormir à l’orée d’une forêt. Vous pensant à l’abri vous vous endormez. Or, un Sanglier vous charge et vous piétine. Vous gémissez de douleur jusqu’à la mort.\n"

(** Fonction permettant de générer aléatoirement une phrase lorsque le personnage dort sans mourir
@author Noemie L
@param perso : le personnage allant dormir
@return le string de la phrase de repos*)		
let phrase_nuit : Personnage.Personnage.personnage->string = fun perso ->
	let lenb=Random.int 3 in
		match lenb with
		| 0 -> "\nVous installez votre campement et tombez rapidement "^(accord_fem_masc perso "endormi")^". Au petit matin, vous vous réveillez sans encombre et gagnez 4 points vies.\n"
		| 1 -> "\nVous etes "^(accord_fem_masc perso "fatigue")^" et decidez de dormir a la belle etoile pret d’un marecage. La lueur du Soleil levant sur l’eau vous reveille et vous gagnez 4 points de vies.\n"
		| 2 -> "\nVos nombreux baillements vous poussent a installer un campement pour dormir a l’oree d’une foret. A l’aube, la rosée du matin vous reveille et vous gagnez 4 points de vie.\n"
		| _ -> ""

(**Fonction permettant de générer aléatoirement une phrase lorsque le personnage perd un objet
@author Noemie L
@param nb : la quantité de l'objet perdu
@param ctn : le contenu de l'objet perdu	
@return le string de la phrase de la perte de l'objet*)	
let phrase_perte_objet : int-> Objet.Objet.contenu->string = fun nb ctn ->
	let lenb=Random.int 3 in
		match lenb with
		| 0 -> "\nDans votre précipitation à fuir vous perdez "^(string_of_int nb)^(bien_ecrire_contenu nb ctn)^".\n"
		| 1 -> "\nVous fuyez rapidement mais égarez "^(string_of_int nb)^(bien_ecrire_contenu nb ctn)^".\n"
		| 2 -> "\nEn prenant la fuite vous laissez échapper "^(string_of_int nb)^(bien_ecrire_contenu nb ctn)^".\n"
		| _ -> ""

(** Fonction permettant de générer aléatoirement un événement avec un monstre
@author Noemie L
@param monstre : le monstre de l'événement
@return le string de la phrase de l'événement*)
let phrase_evenement_monstre : Monstre.Monstre.monstre->string = fun monstre ->
	let lenb=Random.int 4 in
		match monstre.monstre with
		| Golem ->
			(match lenb with
			| 0 -> "\nLe sol tremble et un énorme Golem passe au dessus de votre tête sans vous voir.\n"
			| 1 -> "\nAu loin, vous appercevez une tête de Golem au dessus des arbres de la forêt.\n"
			| 2 -> "\nVous voyez un Golem qui prend tranquillement un bain de boue dans le marai en chantonnant.\n"
			| 3 -> "\nVous sentez une odeur pestinentielle vennant vers vous : c’est un Golem puant qui flatule devant votre nez.\n"
			| _ -> "")
		| Sanglier ->
			(match lenb with
			| 0 -> "\nVous entendez du bruit a l’orée de la forêt et vous apercevez un Sanglier.\n"
			| 1 -> "\nVous voyez un Sanglier qui fouille le sol au loin.\n"
			| 2 -> "\nUn Sanglier cherche des baies en se baladant entre les buissons.\n"
			| 3 -> "\nVous reconnaissez, au loin, la musique “Hakuna Matata” : c’est un Sanglier qui l’interprète gracieusement. Vous commencez à chantonner à votre tour.\n"
			| _ -> "")
		| Moustiques n ->
			(match lenb with
			| 0 -> "\nVous entendez des “bizzz…bizzzz…bizzzzz” autour de vous : c’est "^(un_nom_monstre monstre)^".\n"
			| 1 -> "\nVous appercevez "^(un_nom_monstre monstre)^" au loin.\n"
			| 2 -> "\nUn bruit assourdissant passe prêt de vous : vous reconnaissez "^(un_nom_monstre monstre)^".\n"
			| 3 -> "\nVous voyez "^(un_nom_monstre monstre)^" passant entre les arbres.\n"
			| _ -> "")

(** Fonction permettant de générer aléatoirement une phrase de passage de niveau
@author Noemie L
@param lvl : le niveau qu'a atteint le personnage
@return le string de la phrase de passage de niveau*)
let phrase_level_up : int->string = fun lvl ->
	let lenb=Random.int 3 in
		match lenb with
		| 0 -> "\nFéliciations ! Vous progressez au niveau "^(string_of_int lvl)^".\n\n"
		| 1 -> "\nBravo ! Vous augmentez au niveau "^(string_of_int lvl)^".\n\n"
		| 2 -> "\nHourra ! Vous passez au niveau "^(string_of_int lvl)^".\n\n"
		| _ -> ""

(** Fonction permettant de donner le string d'un potentiel gain d'objet
@author Noemie L
@param obj : l'objet gagné par le personnage
@return le string du gain d'objet ou un point sinon*)
let gain_objet : Monstre.Monstre.loot -> string = fun obj ->
	match obj with
	| Rien -> "."
	| Objet {quantite=qte;obj=ctn} -> " et "^(bien_ecrire_contenu qte ctn)^"."

(** Fonction permettant de générer aléatoirement une phrase de gain de points d'expérience et d'objet
@author Noemie L
@param exp : le nombre de points d'expérience gagnés par le personnage
@param obj : l'objet porté par le monstre et gagné par le personnage
@return le string du gain de points d'expérience et de l'objet gagné*)
let phrase_exp_obj_up : int -> Monstre.Monstre.loot -> string = fun exp obj ->
	let lenb=Random.int 3 in
		match lenb with
		| 0 -> "\nVous gagnez "^(string_of_int exp)^" "^(accord_pluriel_int exp "point")^" d'expérience"^(gain_objet obj)^"\n"
		| 1 -> "\nVous remportez "^(string_of_int exp)^" "^(accord_pluriel_int exp "point")^" d'expérience"^(gain_objet obj)^"\n"
		| 2 -> "\nVous obtenez "^(string_of_int exp)^" "^(accord_pluriel_int exp "point")^" d'expérience"^(gain_objet obj)^"\n"
		| _ -> ""

(** Fonction permettant de générer aléatoirement une question pour faire une action
@author Noemie L
@return le string de la question*)
let phrase_demande_action : unit->string = fun () ->
	let lenb=Random.int 3 in
		match lenb with
		| 0 -> "\n\nQue voulez-vous faire ?\n"
		| 1 -> "\n\nQue faites-vous ?\n"
		| 2 -> "\n\nQuelle action voulez-vous affectuer ?\n"
		| _ -> ""
	
(** Fonction qui affiche l'introduction dans l'univers du jeu
@author Nicolas M*)
let debut_partie : unit -> string = fun () -> "\nBienvenue jeune aventurier(e), vous vous apprêtez à vous lancer dans une aventure remplis d'embuches.
Parviendrez-vous à terrasser tous les monstres qui se dresseront sur votre chemin pour atteindre le niveau 10 ou allez-vous mourrir dans d'atroces souffrances ? \n\n"

(*------------------------------------------------------------DEMANDES-----------------------------------------------------------------*)
(*----------------------------------------------------------UTILISATEUR-----------------------------------------------------------------*)

(** Fonction permettant d'avoir la demande d'action
@author Nicolas S 
@return le string de la demande d'action*)
let afficher_action : unit -> string = fun () -> 
		"(C) Continuer votre chemin \n(D) Dormir \n(M) Manger \n(V) Visualiser l'état de votre personnage \n(Q) Quitter l'aventure "

(** Fonction permettant de demander à l'utilisateur un choix d'action
@author Nicolas S
@return le choix de l'utilisateur*)
let rec demander_action : unit -> string = fun () ->
	let () = print_string "\n<?> " in
	let reponse = read_line () in
		if reponse = "C" || reponse = "c" || reponse = "D" || reponse = "d" || reponse = "M" || reponse = "m" || reponse = "V" || reponse = "v" || reponse = "Q" || reponse = "q"  
			then reponse 
			else let () = print_string "\n Votre choix est invalide ! \n" in demander_action ()

(** Fonction permettant d'avoir la phrase de la demande de réaction
@author Nicolas S
@return le string de la demande de réaction*)
let afficher_reaction : unit -> string = fun () ->
	"(A) Attaquer \n(F) Fuir \n(V) Visualiser l'état de votre personnage "

(** Fonction permmettant de demander à l'utilisateur un choix de réaction par rapport à un évènement
@author Nicolas S
@return le choix de l'utilisateur*)
let rec demander_reaction : unit -> string = fun () ->
	let () = print_string "\n<?> " in 
	let reponse = read_line () in
		if reponse = "A" || reponse = "a" || reponse = "F" || reponse = "f" || reponse = "V" || reponse = "v" 
			then reponse 
			else let () = print_string "\n Votre choix est invalide ! \n" in demander_reaction()

(** Fonction permettant de demander à l'utilisateur son nom
@author Nicolas M
@return le nom de l'utilisateur*)
let demander_nom : unit -> string = fun () -> let () = print_string "Tout d'abord quel est votre nom, jeune aventurier(e) ?\n<?> " in
    read_line ()

(** Fonction permettant d'avoir la phrase de demande de genre de l'utilisateur
@author Nicolas M
@return le string de la demande*)
let afficher_genre : string -> string = fun n -> "\nEnchanté "^n^" ! Etes vous un (H) Homme ou une (F) Femme ?"

(** Fonction permettant de demander à l'utilisateur son genre 
@author Nicolas M
@return le string du genre de la personne*)
let rec demander_genre : unit -> string = fun () ->
	let () = print_string "\n<?> " in
    let reponse = read_line() in
        if reponse = "H" || reponse = "F" || reponse = "h" || reponse = "f" then reponse else let () = print_string "\n Votre choix est invalide ! \n" in demander_genre()

(** Fonction permettant d'avoir la demande à l'utilisateur la classe qu'il souhaite
@author Nicolas M
@return le string de la demande*)
let afficher_classe : Personnage.Personnage.genre -> string = fun genre -> 
	if genre = Personnage.Personnage.Homme 
		then "\nVoulez-vous être un : \n(G) Guerrier \n(A) Archer \n(M) Magicien "
		else "\nVoulez-vous être une : \n(G) Guerriere \n(A) Archere \n(M) Magicienne "

(** Fonction permettant de demander à l'utilisateur sa classe
@author Nicolas M
@param genre : le genre du personnage dont on veut demander la classe
@return le choix de classe de l'utilisateur*)
let rec demander_classe :unit -> string = fun () -> 
	let () = print_string "\n<?> " in
    let reponse = read_line () in 
    	if reponse = "G" || reponse = "g" || reponse = "A" || reponse = "a" || reponse = "M" || reponse = "m" 
				then reponse
    		else let () = print_string "\n Votre choix est invalide ! \n" in demander_classe ()


	end
;;