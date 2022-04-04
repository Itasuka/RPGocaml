module Jeu =
	struct
	exception Personnage_mort
	(** Fonction qui fait combattre un personnage et un monstre jusqu'à la mort de l'un d'eu
	@param p : le personnage
	@param m : le monstre
	@return le personnage s'il gagne
	@raise Personnage_mort si le monstre gagne*)
	let rec combattre : Personnage.Personnage.personnage -> Monstre.Monstre.monstre -> Personnage.Personnage.personnage = fun p m ->
		let monstre = Monstre.Monstre.modif_pv m (Personnage.Personnage.frapper p) in
			if monstre.pv<=0 then p else 
				let personnage = Monstre.Monstre.monstre_frapper p monstre in
					if personnage.pv<=0. then raise Personnage_mort else combattre personnage monstre
	(** Fonction qui fait un attaque surprise d'un monstre aléatoire sur un personnage
	@param p : le personnage
	@return le personnage s'il gagne
	@raise Personnage_mort si le monstre gagne*)
	let malheureuse_rencontre : Personnage.Personnage.personnage -> Personnage.Personnage.personnage = fun p ->
		let m = Monstre.Monstre.generer_monstre_aleatoire () in
			(*phrase_surprise m*)
			let perso = Monstre.Monstre.monstre_frapper p m in
			if perso.pv<=0. then raise Personnage_mort else combattre perso m
	
let get_1_3 = fun (a,_,_) -> a
let get_2_3 = fun (_,a,_) -> a
let get_3_3 = fun (_,_,a) -> a

	(** Fonction permettant de d'afficher et de recuperer le nouvel etat du personnage
@param perso : le personnage dont on veut potentiellement faire perdre des objets 
@return le nouvel etat du personnage*)
let perte_objet : Personnage.Personnage.personnage -> Personnage.Personnage.personnage = fun perso ->
	let pos = (Personnage.Personnage.peut_perdre_objet perso) in
	let boolpos = fst(pos) in
	let sacpos = snd(pos) in
	if (boolpos=false) then perso
	else
		let perdu =(Personnage.Personnage.faire_perte_objet perso sacpos) in
			if (get_1_3 perdu = 0) then perso
			else 
				let () = Affichage.Affichage.aff(Affichage.Affichage.phrase_perte_objet (get_1_3 perdu) (get_2_3 perdu)) in
					get_3_3 perdu
	


	end
;;