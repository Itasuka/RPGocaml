module Monstre =
	struct
	type type_monstre = Golem | Sanglier | Moustiques of int
	type loot = Objet of Objet.Objet.objet | Rien
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
	let monstre_frapper : Personnage.Personnage.personnage -> monstre -> Personnage.Personnage.personnage = fun p m -> 
		match m.monstre with
		|Golem ->  Personnage.Personnage.modif_pv p (-4.)
		|Sanglier -> Personnage.Personnage.modif_pv p (-2.)
		|Moustiques n-> Personnage.Personnage.modif_pv p ((-1.)/.2.*.float_of_int n)
	end
;;