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