module type JEU = 
	sig

	val jouer : unit -> unit

end
;;

module Jeu : JEU =
	struct

		(** Fonction permettant de faire gagner 4pv au perso si il reussit a dormir ou de le tuer sinon
	@param perso : le personnage que l'on souhaite faire dormir
	@return un tuple avec true si il a reussit a dormir et false sinon avec en plus l'etat du perso et le monstre genere*)
	let dormir : Personnage.Personnage.personnage->bool*Personnage.Personnage.personnage*Monstre.Monstre.monstre = fun perso ->
		let lenb=Random.int 20 in
		if lenb>1 then let () = Affichage.Affichage.aff(Affichage.Affichage.phrase_nuit perso) in (true,Personnage.Personnage.modif_pv perso (4.),Monstre.Monstre.generer_monstre_aleatoire())
		else (false,perso,Monstre.Monstre.generer_monstre_aleatoire())

		
	(** Fonction qui effectue le déroulement de la victoire du joueurs lors d'un combat
	@param p : le personnage qui a vaincu
	@param m : le monstre qui a été vaincu
	@return le personnage avec l'xp gagné*)
	let victoire : Personnage.Personnage.personnage -> Monstre.Monstre.monstre -> Personnage.Personnage.personnage = fun p m ->
		let () = Affichage.Affichage.aff(Affichage.Affichage.phrase_mort_monstre m) in 
			let xp = Monstre.Monstre.xp_monstre m in
				let obj = (Monstre.Monstre.get_loot m).obj in
					let qte = (Monstre.Monstre.get_loot m).quantite in
				let () = Affichage.Affichage.aff(Affichage.Affichage.phrase_exp_obj_up xp m.loot) in
					let lvl_et_perso = Personnage.Personnage.gain_xp p xp in
						if fst(lvl_et_perso) > 0
							then let () = Affichage.Affichage.aff(Affichage.Affichage.phrase_level_up (fst(lvl_et_perso))) in
								Personnage.Personnage.modif_sac_perso (snd(lvl_et_perso)) obj qte
							else Personnage.Personnage.modif_sac_perso (snd(lvl_et_perso)) obj qte

	
	exception Personnage_mort of string
	(** Fonction qui fait combattre un personnage et un monstre jusqu'à la mort de l'un d'eu
	@param p : le personnage
	@param m : le monstre
	@return le personnage s'il gagne
	@raise Personnage_mort si le monstre gagne*)
	let rec combattre : Personnage.Personnage.personnage -> Monstre.Monstre.monstre -> Personnage.Personnage.personnage = fun p m ->
		let degats_au_monstre = Personnage.Personnage.frapper p in
			let () = Affichage.Affichage.aff(Affichage.Affichage.phrase_tentative_perso degats_au_monstre) in
				let monstre = Monstre.Monstre.modif_pv m (degats_au_monstre) in
					if monstre.pv<=0 then 
						victoire p monstre else 
							let degats_au_perso = Monstre.Monstre.monstre_frapper monstre in
								let () = Affichage.Affichage.aff(Affichage.Affichage.phrase_attaque_monstre monstre degats_au_perso) in
									let personnage = Personnage.Personnage.modif_pv p degats_au_perso in
										if personnage.pv<=0. then raise (Personnage_mort (Affichage.Affichage.phrase_mort_personnage personnage monstre))
										else combattre personnage monstre
										
	(** Fonction qui fait un attaque surprise d'un monstre aléatoire sur un personnage
	@param p : le personnage
	@return le personnage s'il gagne
	@raise Personnage_mort si le monstre gagne*)
	let malheureuse_rencontre : Personnage.Personnage.personnage -> Personnage.Personnage.personnage = fun p ->
		let m = Monstre.Monstre.generer_monstre_aleatoire () in
			let () = Affichage.Affichage.aff(Affichage.Affichage.phrase_intialisation_surprise m) in
			let degats_au_perso = Monstre.Monstre.monstre_frapper m in
			let perso = Personnage.Personnage.modif_pv p degats_au_perso in
			if perso.pv<=0. then raise (Personnage_mort (Affichage.Affichage.phrase_mort_personnage perso m)) else combattre perso m
	
let get_1_3 : 'a*'b*'c -> 'a = fun (a,_,_) -> a
let get_2_3 : 'a*'b*'c -> 'b = fun (_,a,_) -> a
let get_3_3 : 'a*'b*'c -> 'c = fun (_,_,a) -> a

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
	
	let creer_partie : unit -> Personnage.Personnage.personnage = fun () ->
		let () = Affichage.Affichage.aff(Affichage.Affichage.debut_partie()) in
			let nom = Affichage.Affichage.demander_nom() in
			 	let () = Affichage.Affichage.aff(Affichage.Affichage.afficher_genre nom) in
					let genre_string = Affichage.Affichage.demander_genre () in
						if genre_string = "H" || genre_string = "h" 
							then let genre = Personnage.Personnage.Homme in
								let () = Affichage.Affichage.aff(Affichage.Affichage.afficher_classe genre) in
									let classe_string = Affichage.Affichage.demander_classe () in
									if classe_string = "G" || classe_string = "g"
										then let classe = Personnage.Personnage.Guerrier in
											Personnage.Personnage.creer_personnage nom genre classe
										else 
											if classe_string = "A" || classe_string = "a"
												then let classe = Personnage.Personnage.Archer in
													Personnage.Personnage.creer_personnage nom genre classe
												else let classe = Personnage.Personnage.Magicien in
													Personnage.Personnage.creer_personnage nom genre classe
						else let genre = Personnage.Personnage.Femme in
							let () = Affichage.Affichage.aff(Affichage.Affichage.afficher_classe genre) in
								let classe_string = Affichage.Affichage.demander_classe () in
									if classe_string = "G" || classe_string = "g"
										then let classe = Personnage.Personnage.Guerrier in
											Personnage.Personnage.creer_personnage nom genre classe
										else 
											if classe_string = "A" || classe_string = "a"
												then let classe = Personnage.Personnage.Archer in
													Personnage.Personnage.creer_personnage nom genre classe
												else let classe = Personnage.Personnage.Magicien in
													Personnage.Personnage.creer_personnage nom genre classe

	exception Partie_gagnee
	exception Quitter_adventure
	let rec action_reaction : int -> Personnage.Personnage.personnage -> unit = fun typ p ->
		if p.lvl >= 10 then raise Partie_gagnee else 
		if typ = 0 then
			let () = Affichage.Affichage.aff(Affichage.Affichage.phrase_demande_action()) in
				let () = Affichage.Affichage.aff(Affichage.Affichage.afficher_action()) in
					let action = Affichage.Affichage.demander_action ()  in
						if action = "C" || action = "c"
							then action_reaction 1 p
							else 
								if action = "D" || action = "d"
									then let dormi = dormir p in
										if get_1_3 dormi =  true
											then let perso = get_2_3 dormi in action_reaction 0 perso
											else raise (Personnage_mort (Affichage.Affichage.phrase_mort_nuit (get_2_3 dormi) (get_3_3 dormi)))
									else
										if action = "M" || action = "m"
											then let mange = Personnage.Personnage.manger p in
												if fst(mange)=true
													then let () = Affichage.Affichage.aff(Affichage.Affichage.phrase_manger()) in let perso = snd(mange) in action_reaction 0 perso
													else let () = print_string "Vous n'avez pas de poulets à manger...\n\n" in action_reaction 0 p
											else
												if action = "V" || action = "v"
													then let () = Affichage.Affichage.aff(Affichage.Affichage.afficher_personnage p) in action_reaction 0 p
													else
														if action = "Q" || action = "q"
															then raise Quitter_adventure
															else let () = print_string "\n Votre choix est invalide ! \n" in action_reaction 0 p
			else
				let m = Monstre.Monstre.generer_monstre_aleatoire() in
					let () = Affichage.Affichage.aff(Affichage.Affichage.phrase_evenement_monstre m) in
						let () = Affichage.Affichage.aff(Affichage.Affichage.phrase_demande_action ()) in
							let () = Affichage.Affichage.aff(Affichage.Affichage.afficher_reaction()) in
								let reaction = Affichage.Affichage.demander_reaction () in
									if reaction = "A" || reaction = "a"
										then let perso = combattre p m in action_reaction 0 perso
										else
											if reaction = "F" || reaction = "f"
												then let perso = perte_objet p in let ran = Random.int 100 in
																																		match ran with
																																		|n when n < 14 -> let personnage = malheureuse_rencontre perso in action_reaction 0 personnage
																																		|_ -> action_reaction 0 p
												else
													if reaction = "V" || reaction = "v"
														then let () = Affichage.Affichage.aff(Affichage.Affichage.afficher_personnage p) in action_reaction 1 p
														else let () = print_string "\n Votre choix est invalide ! \n" in action_reaction 1 p
	
	let jouer : unit -> unit = fun () ->
		let p = creer_partie () in
			try
				action_reaction 0 p
			with 
			| Personnage_mort s -> print_string s
			| Quitter_adventure -> print_string "Vous avez quitté l'aventure ! Nous espérons vous revoir bientôt pour une nouvelle aventure !"
			| Partie_gagnee -> print_string "Bravo vous avez gagné la partie ! Soyez-en fier car rare sont les aventurier à triompher dans ce monde."


	end
;;


Jeu.jouer();;