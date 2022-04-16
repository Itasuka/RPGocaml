(*----------------------------------------------------------SIGNATURE----------------------------------------------------------*)

module type PERSONNAGE =
	sig

	type genre = Homme | Femme

	type classe = Archer | Guerrier | Magicien

	type personnage = {nom : string ; genre : genre; classe : classe ; lvl : int ; exp : int ; pv : float ; sac : Objet.Objet.sac}

	(* Fonction qui initialise un personnage niveau 1 avec 20 points de vie et un sac vide *)
	val creer_personnage : string -> genre -> classe -> personnage

	(**)
	val gain_xp : personnage -> int -> int*personnage

	val modif_pv : personnage -> float -> personnage

	val frapper : personnage -> int

	val modif_sac_perso : personnage -> Objet.Objet.contenu -> int -> personnage

	val manger : personnage -> bool*personnage

	val peut_perdre_objet : personnage -> bool*Objet.Objet.sac

	val faire_perte_objet : personnage -> Objet.Objet.sac -> int*Objet.Objet.contenu*personnage

end
;;

(*------------------------------------------------------------MODULE----------------------------------------------------------------*)

module Personnage =
	struct
	type genre = Homme | Femme
	type classe = Archer | Guerrier | Magicien
	type personnage = {nom : string ; genre : genre; classe : classe ; lvl : int ; exp : int ; pv : float ; sac : Objet.Objet.sac}
	
	(** Fonction qui initialise un personnage niveau 1 avec 20 points de vie et un sac vide
	@author Nicolas M
	@param n : le nom du personnage
	@param g : le genre du personnage
	@param c : la classe du personnage
	@return un personnage niveau 1 avec 20 points de vie et un sac vide*)
	let creer_personnage : string -> genre -> classe -> personnage = fun n g c -> {nom = n ; genre = g ; classe = c ; exp = 0 ; lvl = 1 ; pv = 20. ; sac = Objet.Objet.creer_sac()}
	
	(** Fonction permettant l'usage de ^ avec des int
	@author Nicolas S
	@param a : un nombre
	@param b : un nombre
	@return a^b*)
	let rec pow : int -> int -> int = fun a b -> if b > 0 then a * (pow) a (b-1) else 1

	(** Fonction qui permet de faire passer des niveau à un personnage
	@author Nicolas S
	@param p : le personnage qui va recevoir de l'experience
	@param xp : le montant d'expérience
	@return le personnage avec l'expérience ajouté en fonction des niveaux*)
	let gain_xp : personnage -> int -> int*personnage = fun p exp -> 
			let rec aux = fun lvlgain perso xp ->
				let nvxp = perso.exp+xp in
				let xppourlvlup = (pow 2 perso.lvl)*10 in
					if nvxp-xppourlvlup>=0 
					then aux (lvlgain+1) {nom=perso.nom; genre=perso.genre; classe=perso.classe; exp=nvxp-xppourlvlup; lvl=perso.lvl+1; pv=perso.pv; sac=perso.sac} 0
					else (lvlgain,{nom=perso.nom; genre=perso.genre; classe=perso.classe; exp=nvxp; lvl=perso.lvl; pv=perso.pv; sac=perso.sac})
			in aux p.lvl p exp

	(** Fonction qui permet de modifier les pv d'un personnage
	@author Nicolas S
	@param p : le personnage à modifier
	@param pv : le montant de pv ajouter
	@return le personnage avec les pv modifiés*)
	let modif_pv : personnage -> float -> personnage = fun p pv -> 
		if p.pv+.pv>=20. 
			then {nom=p.nom; genre=p.genre; classe=p.classe; lvl=p.lvl; exp=p.exp; pv=20.; sac=p.sac}
			else {nom=p.nom; genre=p.genre; classe=p.classe; lvl=p.lvl; exp=p.exp; pv=p.pv+.pv; sac=p.sac}

	(** Fonction qui renvoie les dégats infligé par un personnage en fonction de sa classe ou 0 s'il rate
	@author Nicolas S
	@param p : le personnage
	@return les dégats infligés*)
	let frapper : personnage -> int = fun p ->
		let de = Random.int 100 in
		match p.classe with
		| Archer -> if de<70+p.lvl*5 then 4 else 0
		| Guerrier -> if de<30+p.lvl*5 then 10 else 0
		| Magicien -> if de<50+p.lvl*5 then 5 else 0

	(** Fonction qui permet de modifier le sac du personnage
	@author Nicolas S
	@param p : le personnage
	@param obj : l'objet à modifier
	@param qte : la quantité à rajouter
	@return le personnage avec son sac modifié*)
	let modif_sac_perso : personnage -> Objet.Objet.contenu -> int -> personnage = fun p obj qte -> {nom=p.nom; genre=p.genre; classe=p.classe; lvl=p.lvl; exp=p.exp; pv=p.pv; sac= Objet.Objet.modif_sac p.sac obj qte}
	
	(** Fonction qui soigne de 4 pv le personnage et consomme 1 poulet si le personnage en possède
	@author Noémie L
	@param p : le personnage
	@return true et le personnage qui à mangé ou false et le personnage qui n'a pas mangé *)
	let manger : personnage -> bool*personnage = fun p ->
		if Objet.Objet.qte_obj p.sac Poulet > 0
			then (true,modif_pv (modif_sac_perso p Poulet (-1)) 2.)
			else (false,p)

	(** Fonction permettant de savoir si le personnage à des objets sur lui et de retourner ceux dont la quantite est differente de 0
	@author Noemie L
	@param perso : le personnage dont on veut savoir si il a des objets
	@return un tuple avec un booleen true si le personnage a des objets sur lui, false sinon et un sac avec les objets que possede le personnage*)
	let peut_perdre_objet : personnage -> bool*Objet.Objet.sac = fun perso ->
		let lesac = perso.sac in
		if (Objet.Objet.qte_obj lesac Eponge=0)&&(Objet.Objet.qte_obj lesac Poulet=0)&&(Objet.Objet.qte_obj lesac Piece=0) then (false, lesac)
		else 
		let rec aux = fun lesac lesobj ->
		match lesac with
		| [] -> lesobj
		| Objet.Objet.{quantite=qte;obj=ctn}::tail when qte=0 -> aux tail lesobj
		| Objet.Objet.{quantite=qte;obj=ctn}::tail -> aux tail (Objet.Objet.{quantite=qte;obj=ctn}::lesobj)
		in (true, aux lesac [])

	(** Fonction permettant de faire perdre ou non un objet à un personnage
	@author Noemie L
	@param perso : le personnage dont on veut faire perdre potentiellement un objet
	@param lesac : le sac contenant les objets que peut perdre le personnage
	@return un tuple contenant la quantite de l'objet perdu, le contenu de l'objet perdu et le personnage modifie*)
	let faire_perte_objet : personnage -> Objet.Objet.sac -> int*Objet.Objet.contenu*personnage = fun perso lesac ->
		let lenb=Random.int 10 in
		match lenb with
		| n when n=0 -> 
				let lenb=Random.int (List.length lesac) in 
					let leobj= List.nth lesac lenb in 
						let qte = leobj.quantite in 
							let obj= leobj.obj in 
								let chance = Random.int qte in 
									if chance > 3 then 
										let chance = 3 in
											(chance+1,obj,modif_sac_perso perso obj (-(chance+1)))
									else
										(chance+1,obj,modif_sac_perso perso obj (-(chance+1)))
		| _ -> (0,Eponge,perso)

	
	end
;;