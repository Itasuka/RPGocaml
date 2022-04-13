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
				let () = Affichage.Affichage.aff(Affichage.Affichage.phrase_exp_obj_up p xp m.loot) in
					let lvl_et_perso = Personnage.Personnage.gain_xp p xp in
						if fst(lvl_et_perso) > p.lvl
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
										if personnage.stats_base.stats.pv<=0. then raise (Personnage_mort (Affichage.Affichage.phrase_mort_personnage personnage monstre))
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
			if perso.stats_base.stats.pv<=0. then raise (Personnage_mort (Affichage.Affichage.phrase_mort_personnage perso m)) else combattre perso m
	
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
				let () = Affichage.Affichage.aff(Affichage.Affichage.phrase_perte_objet perso (get_1_3 perdu) (get_2_3 perdu)) in
					get_3_3 perdu
	
	(** Fonction qui effectue la création de personnage
	@return le nouveau personnage selon les choix de l'utilisateur*)
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

	(** Fonction qui gère les actions chez l'aubergiste
	@param p : le personnage
	@return le personnage quand il quitte l'auberge*)
	let rec action_auberge : Personnage.Personnage.personnage -> Personnage.Personnage.personnage = fun p ->
		let () = Affichage.Affichage.aff((Affichage.Affichage.phrase_aubergiste())^(Affichage.Affichage.afficher_action_aub p)) in
			let choix = Affichage.Affichage.demander_action_auberge() in
				if choix = "A" || choix = "a" then
					let () = Affichage.Affichage.aff(Affichage.Affichage.afficher_demander_qte_achat()) in
						let reponse = Affichage.Affichage.demander_qte() in
							try
								let perso = Marchand.Marchand.peut_acheter_aub p reponse in action_auberge perso
							with
							| Marchand.Marchand.Pas_Assez_DArgent -> let () = Affichage.Affichage.aff(Affichage.Affichage.afficher_pas_assez_argent()) in action_auberge p
				else if choix = "D" || choix = "d" then
					let () = Affichage.Affichage.aff(Affichage.Affichage.phrase_dormir_aub()) in
						let perso = Personnage.Personnage.modif_pv p 10. in action_auberge perso
				else if choix = "O" || choix = "o" then
					let () = Affichage.Affichage.aff(Affichage.Affichage.phrase_texte_aub()) in action_auberge p
				else if choix = "V" || choix = "v" then
					let () = Affichage.Affichage.aff(Affichage.Affichage.afficher_personnage p) in action_auberge p
				else p

	(** Fonction qui gère les actions chez le marabout
	@param p : le personnage
	@return le personnage quand il part de chez le marabout*)
	let rec action_marabout : Personnage.Personnage.personnage -> Personnage.Personnage.personnage = fun p ->
		let mara = Marabout.Marabout.creer_marabout() in
			let () = Affichage.Affichage.aff((Affichage.Affichage.phrase_init_marabout p mara)^(Affichage.Affichage.afficher_acheter_marabout p mara)) in
				let choix = Affichage.Affichage.demander_action_marabout() in
					if choix = "Puissance" || choix = "puissance" then
						try
							let perso = Marabout.Marabout.peut_acheter_marabout p mara Potion_Puissance in action_marabout perso
						with
						| Marabout.Marabout.Pas_Plus_Dune_Potion -> let () = Affichage.Affichage.aff("Vous ne pouvez pas posséder plus d'un exemplaire par type de potion") in action_marabout p
						| Marchand.Marchand.Pas_Assez_DArgent -> let () = Affichage.Affichage.aff(Affichage.Affichage.afficher_pas_assez_argent()) in action_marabout p
					else if choix = "Precision" || choix = "precision" then
						try
							let perso = Marabout.Marabout.peut_acheter_marabout p mara Potion_Precision in action_marabout perso
						with
						| Marabout.Marabout.Pas_Plus_Dune_Potion -> let () = Affichage.Affichage.aff("Vous ne pouvez pas posséder plus d'un exemplaire par type de potion") in action_marabout p
						| Marchand.Marchand.Pas_Assez_DArgent -> let () = Affichage.Affichage.aff(Affichage.Affichage.afficher_pas_assez_argent()) in action_marabout p
					else if choix = "Prediction" || choix = "prediction" then
						try
							let perso = Marchand.Marchand.achat_perte_piece p 1 5 in let () = Affichage.Affichage.aff(Affichage.Affichage.phrase_avenir()) in action_marabout perso
						with
						| Objet.Objet.Pas_assez_Objet -> let () = Affichage.Affichage.aff(Affichage.Affichage.afficher_pas_assez_argent()) in action_marabout p
					else if choix = "V" || choix = "v" then
						let () = Affichage.Affichage.aff(Affichage.Affichage.afficher_personnage p) in action_marabout p
					else p



	(** Fonction qui prends en comptes les action qu'un joueur peut faire dans le village
	@param p : le personnage
	@return le personnage après qu'il ait quitté le village*)
	let rec action_village : Personnage.Personnage.personnage -> Personnage.Personnage.personnage = fun p ->
		let () = Affichage.Affichage.aff(Affichage.Affichage.phrase_village_action()) in
			let action = Affichage.Affichage.demander_action_village() in 
				if action = "A" || action = "a" then let perso = action_auberge p in action_village perso
				else if action = "M" || action = "m" then let perso = action_marabout p in action_village perso
				else if action = "F" || action = "f" then p (*foire*)
				else if action = "V" || action = "v" then
					let () = Affichage.Affichage.aff(Affichage.Affichage.afficher_personnage p) in action_village p
				else p

	(** Fonction qui définit les taux d'apparition des villages/marchand/les deux
	@return un int correspondant au type de rencontre (typ) dans action_reaction*)
	let type_rencontre : unit -> int = fun () ->
		let chance = Random.int 100 in
		match chance with
		| n when n<10 -> 2
		| n when n<25 -> 3
		| n when n<30 -> 4
		|_ -> 0


	exception Partie_gagnee
	exception Quitter_adventure
	(** Fonction qui effectue le déroulement de la partie
	@param typ : le type de rencontre (0 pour action et 1 pour une réaction)
	@param p : le personnage de l'utilisateur
	@param monstre : le monstre que le joueur peut potentiellement rencontrer en combat
	@raise Partie_gagnee quand le joueur atteint le niveau 10
	@raise Quitter_adventure quand le joueur choisit de quitter l'aventure*)
	let rec action_reaction : int -> Personnage.Personnage.personnage -> Monstre.Monstre.monstre -> unit = fun typ p monstre->
		if p.lvl >= 10 then raise Partie_gagnee else 
		if typ = 0 then
			let () = Affichage.Affichage.aff(Affichage.Affichage.phrase_demande_action()) in
				let () = Affichage.Affichage.aff(Affichage.Affichage.afficher_action()) in
					let action = Affichage.Affichage.demander_action ()  in
						if action = "C" || action = "c"
							then action_reaction 1 p monstre
							else 
								if action = "D" || action = "d"
									then let dormi = dormir p in
										if get_1_3 dormi =  true
											then let perso = get_2_3 dormi in action_reaction 0 perso monstre
											else raise (Personnage_mort (Affichage.Affichage.phrase_mort_nuit (get_2_3 dormi) (get_3_3 dormi)))
									else
										if action = "E" || action = "e"
											then let () = Affichage.Affichage.aff(Affichage.Affichage.afficher_perso_change_arme p) in 
												let choix = Affichage.Affichage.demande_perso_change_arme p in
													let perso = Personnage.Personnage.change_arme p choix in
														action_reaction 0 perso monstre
									else
										if action = "M" || action = "m"
											then let mange = Personnage.Personnage.manger p in
												if fst(mange)=true
													then let () = Affichage.Affichage.aff(Affichage.Affichage.phrase_manger()) in let perso = snd(mange) in action_reaction 0 perso monstre
													else let () = print_string "\nVous n'avez pas de poulets à manger..." in action_reaction 0 p monstre
											else
												if action = "V" || action = "v"
													then let () = Affichage.Affichage.aff(Affichage.Affichage.afficher_personnage p) in action_reaction 0 p monstre
													else
														if action = "Q" || action = "q"
															then raise Quitter_adventure
															else let () = print_string "\n Votre choix est invalide ! \n" in action_reaction 0 p monstre
			else if typ = 1 then
				let m = Monstre.Monstre.generer_monstre_aleatoire() in
					let () = Affichage.Affichage.aff(Affichage.Affichage.phrase_evenement_monstre m) in
						let () = Affichage.Affichage.aff(Affichage.Affichage.phrase_demande_action ()) in
							let () = Affichage.Affichage.aff(Affichage.Affichage.afficher_reaction()) in
								let reaction = Affichage.Affichage.demander_reaction () in
									if reaction = "A" || reaction = "a"
										then let perso = combattre p m in action_reaction (type_rencontre()) perso monstre
										else
											if reaction = "F" || reaction = "f"
												then let perso = perte_objet p in let ran = Random.int 100 in
																																		match ran with
																																		|n when n < 14 -> let personnage = malheureuse_rencontre perso in action_reaction (type_rencontre()) personnage monstre
																																		|_ -> action_reaction (type_rencontre()) p monstre
												else
													if reaction = "V" || reaction = "v"
														then let () = Affichage.Affichage.aff(Affichage.Affichage.afficher_personnage p) in action_reaction 1 p m
														else let () = print_string "\n Votre choix est invalide ! \n" in action_reaction 1 p m
				else if typ = 3 then print_string"marchand"(*marchand*)
				else if typ = 4 then 
					let () = Affichage.Affichage.aff(Affichage.Affichage.phrase_init_village()) in
						let action = Affichage.Affichage.demander_action_village() in
							if (*village*)
				else print_string"marchand + village"(*marchand+village*)
	
	(** Fonction qui creer la partie et effectue le déroulement de la partie
	@catch Personnage_mort et affiche le message de mort
	@catch Quitter_adventure et affiche un message d'au revoir au personnage
	@catch Partie_gagnee et affiche un message pour félicité le personnage d'avoir fini le jeu*)
	let jouer : unit -> unit = fun () ->
		let p = creer_partie () in
			try
				action_reaction 0 p (Monstre.Monstre.generer_monstre_aleatoire())
			with 
			| Personnage_mort s -> print_string s
			| Quitter_adventure -> print_string "\nVous avez quitté l'aventure ! Nous espérons vous revoir bientôt pour une nouvelle aventure !\n"
			| Partie_gagnee -> print_string "\nBravo vous avez gagné la partie ! Soyez-en fier car rare sont les aventurier à triompher dans ce monde.\n"


	end
;;