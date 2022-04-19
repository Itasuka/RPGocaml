(*----------------------------------------------------------SIGNATURE----------------------------------------------------------*)

module type MONSTRE =
	sig

	type type_monstre = Golem | Sanglier | Moustiques of int

	type loot = Objet of Objet.Objet.objet | Rien

	type monstre = {monstre : type_monstre; loot : loot ; pv : int}

	(* Fontion qui récupère le loot d'un monstre *)
	val get_loot : monstre -> Objet.Objet.objet

	(*Fonction qui fait diminuer les pv d'un monstre*)
	val modif_pv : monstre -> int -> monstre

	(* Fonction qui génère un nombre de points de vie aléatoire en fonction du type de monstre*)
	val generer_monstre_aleatoire : unit -> monstre

	(* Fonction qui inflige des dégats à un personnage en fonction du monstre*)
	val monstre_frapper : monstre -> float

	(* Fonction qui associe à un type de monstre son xp gagné en combat*)
	val xp_monstre : monstre -> int

end
;;

(*------------------------------------------------------------MODULE----------------------------------------------------------------*)

module Monstre : MONSTRE =
	struct

	(** Type représentant les différents monstres du jeu
	@author Nicolas M*)
	type type_monstre = Golem | Sanglier | Moustiques of int

	(** Type représentant le fait qu'un monstre puisse posséder un objet qui donne au joueur à sa mort ou non
	@author Nicolas S*)
	type loot = Objet of Objet.Objet.objet | Rien

	(** Type représentant un monstre avec son type, son loot et ses points de vie
	@author Nicolas M*)
	type monstre = {monstre : type_monstre; loot : loot ; pv : int}

	(** Fontion qui récupère le loot d'un monstre
	@author Nicolas S
	@param m : le monstre
	@return le loot du monstre*)
	let get_loot : monstre -> Objet.Objet.objet = fun m ->
		match m.loot with
		|Rien -> Objet.Objet.{quantite = 0; obj = Eponge}
		|Objet o -> o

	(** Fonction qui fait diminuer les pv d'un monstre
	@author Nicolas M
	@param m : le monstre
	@param pv : les pv enlevé
	@return le monstre avec les pv modifiés*)
	let modif_pv : monstre -> int -> monstre = fun m pv -> 
		{monstre = m.monstre; loot = m.loot; pv = m.pv-pv}

	(** Fonction qui génère un nombre de points de vie aléatoire en fonction du type de monstre
	@author Nicolas S
	@param monstre : le type de monstre
	@return les pv du monstre*)
	let vie_monstre : type_monstre -> int = fun monstre ->
		match monstre with
		| Golem -> 25 + (Random.int 6)+1
		| Moustiques n -> 2 + n
		| Sanglier -> 10 + (Random.int 4)+1

	(** Fonction qui génère un objet aléatoire pour un monstre en fonction de son type
	@author Nicolas S
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
	@author Nicolas S
	@return un monstre aléatoire avec ses points de vie et son objet aléatoire*)
	let generer_monstre_aleatoire : unit -> monstre = fun () ->
		let i = Random.int 3 in
			match i with
			| 0 -> {monstre=Golem; loot=objet_monstre Golem; pv=vie_monstre Golem}
			| 1 -> let n = Random.int 6 in {monstre=Moustiques (n+1); loot=objet_monstre (Moustiques n); pv=vie_monstre (Moustiques (n+1))}
			| _ -> {monstre=Sanglier; loot=objet_monstre Sanglier; pv=vie_monstre Sanglier}

	(** Fonction qui inflige des dégats à un personnage en fonction du monstre
	@author Nicolas M
	@param p : le personnage qui se fait frapper
	@param m : le monstre qui frappe
	@return le personnage avec des pv en moins*)
	let monstre_frapper : monstre -> float = fun m -> 
		match m.monstre with
		|Golem ->  (-4.)
		|Sanglier -> (-2.)
		|Moustiques n-> ((-1.)/.2.*.float_of_int n)

	(** Fonction qui associe à un type de monstre son xp gagné en combat
	@author Nicolas M
	@param m : le monstre
	@return l'xp gagné en tuant le monstre*)
	let xp_monstre : monstre -> int = fun m ->
		match m.monstre with
		|Golem -> 8
		|Sanglier -> 4
		|Moustiques n -> 2+(1*(n-1))
		

	end
;;