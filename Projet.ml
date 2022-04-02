Random.self_init();;

module Objet =
	struct
	type contenu = Eponge | Piece | Poulet
	type objet = {quantite : int ; obj : contenu}
	type sac = objet list
	(** Fonction qui initialise un sac vide
	@return un sac avec tout les objets disponibles avec une quantité 0*)
	let creer_sac : unit -> sac = 
		fun () -> [{quantite = 0 ; obj = Eponge}
					 ;{quantite = 0 ; obj = Piece}
					 ;{quantite = 0 ; obj = Poulet
	(** Fonction renvoie le nombre d'occurrence de obj dans sac
	@param sac : le sac dans lequel on va compter le nombre d'occurence d'obj
	@param obj : l'obj dont on veut connaitre le nombre
	@return le nombre d'occurrence de obj dans sac*)
	let rec qte_obj : sac -> objet -> int = fun sac obj ->
		match sac with
		|[] -> 0
		|o::t when o.obj=obj -> o.quantite
		|_::t -> qte_obj t obj
	exception Pas_assez_Objet;; 
	(** Fonction qui modifie la quantité d'un objet dans un sac
	@param sac : le sac qui va être modifié
	@param objet : objet dont la quantité va être modifié
	@param qte : quantité à rajouter
	@return le sac modifier 
	@raise Pas_assez_Objet si on enlève une quantité supérieur à celle de départ*)
	let rec modif_sac : sac -> contenu -> int -> sac = fun sac objet qte ->
		match sac with
		|[] -> []
		|h::t when h.obj=objet -> 
			if h.quantite+qte<=0 
				then raise Pas_assez_Objet
				else {quantite=h.quantite+qte;obj=h.obj}::t
		|h::t -> h::(modif_sac sac objet qte)
	end
;;

module Personnage =
	struct
	type genre = Homme | Femme
	type classe = Archer | Guerrier | Magicien
	type personnage = {nom : string ; genre : genre; classe : classe ; lvl : int ; exp : int ; pv : float ; sac : Objet.sac}
	(** Fonction qui initialise un personnage niveau 1 avec 20 points de vie et un sac vide
	@param n : le nom du personnage
	@param g : le genre du personnage
	@param c : la classe du personnage
	@return un personnage niveau 1 avec 20 points de vie et un sac vide*)
	let creer_personnage : string -> genre -> classe -> personnage = fun n g c -> {nom = n ; genre = g ; classe = c ; exp = 0 ; lvl = 1 ; pv = 20. ; sac = Objet.creer_sac()}
	(** Fonction permettant l'usage de ^ avec des int
	@param a : un nombre
	@param b : un nombre
	@return a^b*)
	let rec pow : int -> int -> int = fun a b -> if b > 0 then a * (pow) a (b-1) else 1
	(** Fonction qui permet de faire passer des niveau à un personnage
	@param p : le personnage qui va recevoir de l'experience
	@param xp : le montant d'expérience
	@return le personnage avec l'expérience ajouté en fonction des niveaux*)
	let gain_xp : personnage -> int -> int*personnage = fun p xp -> 
		let nvxp = p.exp+xp in
			let rec aux = fun lvlgain perso ->
				let xppourlvlup = (pow 2 perso.lvl)*10 in
					if nvxp-xppourlvlup>=0 
					then aux (lvlgain+1) {nom=perso.nom; genre=perso.genre; classe=perso.classe; exp=nvxp-xppourlvlup; lvl=perso.lvl+1; pv=perso.pv; sac=perso.sac}
					else (lvlgain,{nom=perso.nom; genre=perso.genre; classe=perso.classe; exp=perso.exp; lvl=perso.lvl; pv=perso.pv; sac=perso.sac})
			in aux 0 p
	(** Fonction qui permet de modifier les pv d'un personnage
	@param p : le personnage à modifier
	@param pv : le montant de pv ajouter
	@return le personnage avec les pv modifiés*)
	let modif_pv : personnage -> float -> personnage = fun p pv -> 
		if p.pv+.pv>=20. 
			then {nom=p.nom; genre=p.genre; classe=p.classe; lvl=p.lvl; exp=p.exp; pv=20.; sac=p.sac}
			else {nom=p.nom; genre=p.genre; classe=p.classe; lvl=p.lvl; exp=p.exp; pv=p.pv+.pv; sac=p.sac}
	(** Fonction qui renvoie les dégats infligé par un personnage en fonction de sa classe ou 0 s'il rate
	@param p : le personnage
	@return les dégats infligés*)
	let frapper : personnage -> int = fun p ->
		let de = Random.int 100 in
		match p.classe with
		| Archer -> if de<70+p.lvl*5 then 4 else 0
		| Guerrier -> if de<30+p.lvl*5 then 10 else 0
		| Magicien -> if de<50+p.lvl*5 then 5 else 0
	(** Fonction qui permet de modifier le sac du personnage
	@param p : le personnage
	@param obj : l'objet à modifier
	@param qte : la quantité à rajouter
	@return le personnage avec son sac modifié*)
	let modif_sac_perso : personnage -> Objet.objet -> int -> personnage = fun p obj qte -> {nom=p.nom; genre=p.genre; classe=p.classe; lvl=p.lvl; exp=p.exp; pv=p.pv; sac=Objet.modif_sac p.sac obj qte}
	(** Fonction qui soigne de 4 pv le personnage et consomme 1 poulet si le personnage en possède
	@param p : le personnage
	@return true et le personnage qui à mangé ou false et le personnage qui n'a pas mangé *)
	let manger : personnage -> bool*personnage = fun p ->
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
	(** Fonction qui fait diminuer les pv d'un monstre
	@param m : le monstre
	@param pv : les pv enlevé
	@return le monstre avec les pv modifiés*)
	let modif_pv : monstre -> int -> monstre = fun m pv -> 
		{monstre = m.monstre; loot = m.loot; pv = m.pv-pv}
	(** Fonction qui génère un nombre de points de vie aléatoire en fonction du type de monstre
	@param monstre : le type de monstre
	@return les pv du monstre*)
	let vie_monstre : type_monstre -> int = fun monstre ->
		match monstre with
		| Golem -> 25 + (Random.int 6)+1
		| Moustiques n -> 2 + n
		| Sanglier -> 10 + (Random.int 4)+1
	(** Fonction qui génère un objet aléatoire pour un monstre en fonction de son type
	@param m : le type de monstre
	@return l'objet tenu par le monstre s'il en a un*)
	let objet_monstre : type_monstre -> loot = fun m ->
		let i = Random.int 3 in
		match m with
		| Moustiques n -> Rien
		| _ -> match i with
				| 0 -> Objet {quantite=(Random.int 2)+1; obj=Eponge}
				| 1 -> Objet {quantite=(Random.int 7)+1; obj=Piece}
				| _ -> Objet {quantite=1; obj=Poulet}
	(** Fonction qui génère un monstre aléatoire avec ses points de vie et son objet aléatoire
	@return un monstre aléatoire avec ses points de vie et son objet aléatoire*)
	let generer_monstre_aleatoire : unit -> monstre = fun () ->
		let i = Random.int 3 in
			match i with
			| 0 -> {monstre=Golem; loot=objet_monstre Golem; pv=vie_monstre Golem}
			| 1 -> let n = Random.int 6 in {monstre=Moustiques (n+1); loot=objet_monstre (Moustiques n); pv=vie_monstre (Moustiques (n+1))}
			| _ -> {monstre=Sanglier; loot=objet_monstre Sanglier; pv=vie_monstre Sanglier}
	(** Fonction qui inflige des dégats à un personnage en fonction du monstre
	@param p : le personnage qui se fait frapper
	@param m : le monstre qui frappe
	@return le personnage avec des pv en moins*)
	let monstre_frapper : Personnage.personnage -> monstre -> Personnage.personnage = fun p m -> 
		match m.monstre with
		|Golem ->  Personnage.modif_pv p (-4.)
		|Sanglier -> Personnage.modif_pv p (-2.)
		|Moustiques n-> Personnage.modif_pv p ((-1.)/.2.*.float_of_int n)
	end
;;


module Jeu =
	struct
	exception Personnage_mort
	(** Fonction qui fait combattre un personnage et un monstre jusqu'à la mort de l'un d'eu
	@param p : le personnage
	@param m : le monstre
	@return le personnage s'il gagne
	@raise Personnage_mort si le monstre gagne*)
	let rec combattre : Personnage.personnage -> Monstre.monstre -> Personnage.personnage = fun p m ->
		let monstre = Monstre.modif_pv m (Personnage.frapper p) in
			if monstre.pv<=0 then p else 
				let personnage = Monstre.monstre_frapper p monstre in
					if personnage.pv<=0. then raise Personnage_mort else combattre personnage monstre
	(** Fonction qui fait un attaque surprise d'un monstre aléatoire sur un personnage
	@param p : le personnage
	@return le personnage s'il gagne
	@raise Personnage_mort si le monstre gagne*)
	let malheureuse_rencontre : Personnage.personnage -> Personnage.personnage = fun p ->
		let m = Monstre.generer_monstre_aleatoire () in
			(*phrase_surprise m*)
			p
			
	end
;;