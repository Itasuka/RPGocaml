module type SIGAFFICHAGE = 
	sig
		val genrer_classe : Personnage.personnage -> string
		
		val string_monstre : Monstre.monstre -> string
		
		val bien_ecrire_contenu : int -> Objet.contenu -> string
		
		val afficher_contenu_sac : Objet.sac -> string
		
		val afficher_personnage : Personnage.personnage -> string
	end;;
		
		
module Affichage : SIGAFFICHAGE =
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
	let genrer_classe : Personnage.personnage -> string = fun perso ->
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
	let bien_ecrire_contenu : int-> Objet.contenu -> string = fun qte ctn->
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
	let string_monstre : Monstre.monstre -> string = fun m -> match m.monstre with
    |Golem -> "golem"
    |Sanglier -> "sanglier"
    |Moustiques n-> "nu�e de moustiques"
    
    
	(** Fonction permettant d'afficher le contenu du sac
	@param sac : le sac dont on veut afficher le contenu
	@return le string de l'affichage du sac*)
	let afficher_contenu_sac : Objet.sac->string = fun sac ->
		let rec aux = fun sac affichage ->
		match sac with
		| [] -> affichage
		| {Objet.quantite = qte ; obj = _}::tail when qte<=0 -> aux tail affichage
		| {quantite = qte ; obj = ctn}::[] -> affichage^(bien_ecrire_contenu qte ctn)
		| {quantite = qte ; obj = ctn }::tail -> aux tail (affichage^(bien_ecrire_contenu qte ctn)^"\n")
		in aux sac ""
		
	(** Fonction permettant d'afficher la ligne de l'identit� du personnage
	@param perso : le personnage dont on souhaite afficher l'identit�
	@return le string de l'affichage de l'identit�*)
	let afficher_identite : Personnage.personnage->string = fun perso ->
		"| "^(perso.nom)^" | "^(genrer_classe perso)^" niveau "^(string_of_int perso.lvl)^" |"

	(** Fonction permettant d'avoir la longueur de l'affichage de l'identit�
	@param perso : le personnage dont on souhaite avoir la longueur de l'affichage d'identit�
	@return le integer de la longueur de l'affichage de l'identit�*)
	let longueur_affichage : Personnage.personnage->int = fun perso -> 
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
	let afficher_pv : Personnage.personnage->string = fun perso ->
		let lg = longueur_affichage perso in
		let stringpv= "| Points de vie |" in
		let stringnbpv=(string_of_float perso.pv)^" |" in
		let nbespaces = lg-(String.length (stringpv) + String.length (stringnbpv)) in
		stringpv^ligne_espace nbespaces^stringnbpv

	(** Fonction permettant d'afficher la ligne de l'exp�rience du personnage
	@param perso : le personnage dont on souhaite afficher de l'exp�rience
	@return : le string de l'affichage de la ligne de l'exp�rience du personnage*)	
	let afficher_exp : Personnage.personnage->string = fun perso ->
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
	let afficher_sac_pas_complet : Personnage.personnage ->string = fun perso ->
		let lesac= perso.sac in
		let lg = longueur_affichage perso in 
		let rec aux = fun sac affichage->
		match sac with
		[] -> affichage
		| {Objet.quantite= qte ; obj = ctn}::tail when qte<=0 -> aux tail affichage
		| {quantite= qte ; obj = ctn}::[] -> 
		 let bec="|  "^(bien_ecrire_contenu qte ctn) in let lg2= String.length bec in affichage^bec^(ligne_espace (lg-lg2-1))^"|"
		| {quantite = qte ; obj = ctn }::o::tail when o.quantite>0 -> let bec="|  "^(bien_ecrire_contenu qte ctn) in let lg2= String.length bec in aux (o::tail) (affichage^bec^(ligne_espace (lg-lg2-1))^"|"^"\n")
		| {quantite = qte ; obj = ctn }::tail -> let bec="|  "^(bien_ecrire_contenu qte ctn) in let lg2= String.length bec in aux tail (affichage^bec^(ligne_espace (lg-lg2-1))^"|")
		in aux lesac ""

	(** Fonction permettant d'afficher la ligne du sac pour l'affichage de l'�tat du personnage
	@param perso : le personnage dont on veut afficher l'�tat
	@return : le string de la ligne du sac pour l'affichage de l'�tat du personnage*)
	let afficher_partie_sac : Personnage.personnage ->string = fun perso ->
		let lg = longueur_affichage perso in 
		let sac = "| Sac" in
		let lgsac=String.length sac in
		sac^(ligne_espace (lg-1-lgsac))^"|"

	(** Fonction permettant d'afficher la partie sac pour l'affichage de l'�tat du personnage
	@param perso : le personnage dont on veut afficher le sac 
	@return : le string de l'affichage de la partie sac pour l'affichage de l'�tat du personnage*)	
	let afficher_sac : Personnage.personnage->string = fun perso ->
		(afficher_partie_sac perso)^"\n"^(afficher_sac_pas_complet perso)

	(**Fonction permettant d'affichage l'�tat du personnage
	@param perso : le personnage dont on veut afficher l'�tat
	@return : le string de l'affichage de l'�tat du personnage*)
	let afficher_personnage : Personnage.personnage -> string = fun perso -> 
		let lg = longueur_affichage perso in
		let ligne_croix = borner_ligne_croix (ligne_de_tirets lg) in
		ligne_croix^"\n"^(afficher_identite perso)^"\n"^ligne_croix^"\n"^(afficher_pv perso)^"\n"^ligne_croix^"\n"^(afficher_exp perso)^"\n"^ligne_croix^"\n"^(afficher_sac perso)^"\n"^ligne_croix
	
(** Fonction permettant de demander a l'utilisateur un choix
@return le choix de l'utilisateur*)
let demander_action : unit -> string = fun () ->
	print_string "(C) Continuer votre chemin \n(D) Dormir \n(M) Manger \n(V) Visualiser l'etat de votre personnage \n(Q) Quitter l'aventure \n<?> ";
	read_line ()

(** Fonction permmettant de demander a l'utilisateur un choix
@return le choix de l'utilisateur*)
let demander_reaction : unit -> string = fun () ->
	print_string "(A) Attaquer \n(F) Fuir \n(V) Visualiser l'etat de votre personnage \n<?> ";
	read_line ()

	end
;;


