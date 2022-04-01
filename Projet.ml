Random.self_init();;

module Objet =
	struct
	type contenu = Eponge | Piece | Poulet
	type objet = {quantite : int ; obj : contenu}
	type sac = objet list
	let creer_sac : unit -> sac = 
	fun () -> [{quantite = 0 ; obj = Eponge}
				;{quantite = 0 ; obj = Piece}
				;{quantite = 0 ; obj = Poulet}]
	let rec qte_obj = fun sac obj ->
		match sac with
		|[] -> 0
		|o::t when o.obj=obj -> o.quantite
		|_::t -> qte_obj t obj
	let rec modif_sac : sac -> contenu -> int -> sac = fun sac objet qte ->
		match sac with
		|[] -> []
		|h::t when h.obj=objet -> 
			if h.quantite+qte<=0 
				then {quantite=0; obj=h.obj}::t
				else {quantite=h.quantite+qte;obj=h.obj}::t
		|h::t -> h::(modif_sac sac objet qte)
	end
;;

module Personnage =
	struct
	type genre = Homme | Femme
	type classe = Archer | Guerrier | Magicien
	type personnage = {nom : string ; genre : genre; classe : classe ; lvl : int ; exp : int ; pv : float ; sac : Objet.sac}
	let creer_personnage : string -> genre -> classe -> personnage = fun n g c -> {nom = n ; genre = g ; classe = c ; exp = 0 ; lvl = 1 ; pv = 20. ; sac = Objet.creer_sac()}
	let gainxp : personnage -> int -> int*personnage = fun p xp -> 
		if p.exp+xp>=(2*p.lvl)*10 
			then (1,{nom=p.nom; genre=p.genre; classe=p.classe; lvl=p.lvl+1; exp=p.exp+xp-(2*p.lvl)*10; pv=p.pv; sac=p.sac})
			else (0,{nom=p.nom; genre=p.genre; classe=p.classe; lvl=p.lvl; exp=p.exp+xp; pv=p.pv; sac=p.sac})
	let modif_pv : personnage -> float -> personnage = fun p pv -> 
		if p.pv+.pv>=20. 
			then {nom=p.nom; genre=p.genre; classe=p.classe; lvl=p.lvl; exp=p.exp; pv=20.; sac=p.sac}
			else {nom=p.nom; genre=p.genre; classe=p.classe; lvl=p.lvl; exp=p.exp; pv=p.pv+.pv; sac=p.sac}
	let frapper = fun p ->
		let de = Random.int 100 in
		match p.classe with
		| Archer -> if de<70+p.lvl*5 then 4 else 0
		| Guerrier -> if de<30+p.lvl*5 then 10 else 0
		| Magicien -> if de<50+p.lvl*5 then 5 else 0
	let modif_sac_perso = fun p obj qte -> {nom=p.nom; genre=p.genre; classe=p.classe; lvl=p.lvl; exp=p.exp; pv=p.pv; sac=Objet.modif_sac p.sac obj qte}
	let manger = fun p ->
		if Objet.qte_obj p.sac Poulet > 0
			then (true,modif_pv (modif_sac_perso p Poulet (-1)) 2.)
			else (false,p)
	end
;;

module Monstre =
	struct
	type type_monstre = Golem | Sanglier | Moustiques of int
	type loot = Objet of Objet.objet | Rien
	type monstre = {monstre : type_monstre; loot : loot ; pv : int}
	let vie_monstre : type_monstre -> int = fun monstre ->
		match monstre with
		| Golem -> 25 + (Random.int 6)+1
		| Moustiques n -> 2 + n
		| Sanglier -> 10 + (Random.int 4)+1
	let objet_monstre : type_monstre -> loot = fun m ->
		let i = Random.int 3 in
		match m with
		| Moustiques n -> Rien
		| _ -> match i with
				| 0 -> Objet {quantite=(Random.int 2)+1; obj=Eponge}
				| 1 -> Objet {quantite=(Random.int 7)+1; obj=Piece}
				| _ -> Objet {quantite=1; obj=Poulet}
	let generer_monstre_aleatoire : unit -> monstre = fun () ->
		let i = Random.int 3 in
			match i with
			| 0 -> {monstre=Golem; loot=objet_monstre Golem; pv=vie_monstre Golem}
			| 1 -> let n = Random.int 6 in {monstre=Moustiques (n+1); loot=objet_monstre (Moustiques n); pv=vie_monstre (Moustiques (n+1))}
			| _ -> {monstre=Sanglier; loot=objet_monstre Sanglier; pv=vie_monstre Sanglier}
	let monstre_frapper : Personnage.personnage -> monstre -> Personnage.personnage = fun p m -> 
		match m.monstre with
		|Golem ->  Personnage.modif_pv p (-4.)
		|Sanglier -> Personnage.modif_pv p (-2.)
		|Moustiques n-> Personnage.modif_pv p ((-1.)/.2.*.float_of_int n)
	end
;;

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
		
	end
;;