module type PERSONNAGE =
	sig

	type genre = Homme | Femme

	type classe = Archer | Guerrier | Magicien

	type stats_pers = {stats : Equipement.Equipement.stats; pv_max : float}

	type personnage = {nom : string ; genre : genre; classe : classe ; lvl : int ; exp : int ; stats_base : stats_pers; sac : Objet.Objet.sac; armequipe : Equipement.Equipement.arme; statBonusPot : Equipement.Equipement.stats}

	val creer_personnage : string -> genre -> classe -> personnage

	val gain_xp : personnage -> int -> int*personnage

	val modif_pv : personnage -> float -> personnage

	val frapper : personnage -> int

	val modif_sac_perso : personnage -> Objet.Objet.contenu -> int -> personnage

	val manger : personnage -> bool*personnage

	val peut_perdre_objet : personnage -> bool*Objet.Objet.sac

	val faire_perte_objet : personnage -> Objet.Objet.sac -> int*Objet.Objet.contenu*personnage

	val change_arme : personnage -> string -> personnage

	val modif_stats_pot : personnage -> string -> int -> personnage

	val reset_stats_pot : personnage -> personnage

	val generer_arme_marchand : personnage -> Objet.Objet.objet_marchand

	val  bonus : personnage -> Objet.Objet.contenu -> int*personnage

end
;;

module Personnage : PERSONNAGE =
	struct
	type genre = Homme | Femme
	type classe = Archer | Guerrier | Magicien
	type stats_pers = {stats : Equipement.Equipement.stats; pv_max : float}
	type personnage = {nom : string ; genre : genre; classe : classe ; lvl : int ; exp : int ; stats_base : stats_pers; sac : Objet.Objet.sac; armequipe : Equipement.Equipement.arme; statBonusPot : Equipement.Equipement.stats}
	let stats_base_G : unit -> stats_pers = fun () -> Equipement.Equipement.{stats={pv = 20.; atk = 10; def = 0; acc = 30};pv_max=20.}
	let stats_base_A : unit -> stats_pers = fun () -> Equipement.Equipement.{stats={pv = 20.; atk = 4; def = 0; acc = 70};pv_max=20.}
	let stats_base_M : unit -> stats_pers = fun () -> Equipement.Equipement.{stats={pv = 20.; atk = 5; def = 0; acc = 50};pv_max=20.}
	(** Fonction qui initialise un personnage niveau 1 avec 20 points de vie et un sac vide
	@param n : le nom du personnage
	@param g : le genre du personnage
	@param c : la classe du personnage
	@return un personnage niveau 1 avec 20 points de vie et un sac vide*)
	let creer_personnage : string -> genre -> classe -> personnage = fun n g c -> if c=Guerrier then {nom = n ; genre = g ; classe = c ; exp = 0 ; lvl = 1 ; stats_base = stats_base_G() ; sac = (Objet.Objet.modif_sac (Objet.Objet.creer_sac()) (Arme (G Epee_en_bois)) 1); armequipe = G (Equipement.Equipement.creer_arme_guerrier Equipement.Equipement.Epee_en_bois); statBonusPot = Equipement.Equipement.stats_base()}
	else if c=Archer then {nom = n ; genre = g ; classe = c ; exp = 0 ; lvl = 1 ; stats_base = stats_base_A() ; sac = (Objet.Objet.modif_sac (Objet.Objet.creer_sac()) (Arme (A Arc_en_bois)) 1); armequipe = A (Equipement.Equipement.creer_arme_archer Equipement.Equipement.Arc_en_bois); statBonusPot = Equipement.Equipement.stats_base()}
	else {nom = n ; genre = g ; classe = c ; exp = 0 ; lvl = 1 ; stats_base = stats_base_M() ; sac = (Objet.Objet.modif_sac (Objet.Objet.creer_sac()) (Arme (M Baton_en_bois)) 1); armequipe = M (Equipement.Equipement.creer_arme_magicien Equipement.Equipement.Baton_en_bois); statBonusPot = Equipement.Equipement.stats_base()}
	(** Fonction permettant l'usage de ^ avec des int
	@param a : un nombre
	@param b : un nombre
	@return a^b*)
	let rec pow : int -> int -> int = fun a b -> if b > 0 then a * (pow) a (b-1) else 1
	(** Fonction qui permet de faire passer des niveau à un personnage
	@param p : le personnage qui va recevoir de l'experience
	@param xp : le montant d'expérience
	@return le personnage avec l'expérience ajouté en fonction des niveaux*)
	let gain_xp : personnage -> int -> int*personnage = fun p exp -> 
			let rec aux = fun lvlgain perso xp ->
				let nvxp = perso.exp+xp in
				let xppourlvlup = (pow 2 perso.lvl)*10 in
					if nvxp-xppourlvlup>=0 
					then aux (lvlgain+1) {nom=perso.nom; genre=perso.genre; classe=perso.classe; exp=nvxp-xppourlvlup; lvl=perso.lvl+1; stats_base = {stats = Equipement.Equipement.{pv = perso.stats_base.stats.pv+.2.; atk = perso.stats_base.stats.atk; def = perso.stats_base.stats.def; acc = perso.stats_base.stats.acc}; pv_max = perso.stats_base.pv_max+.2.}; sac=perso.sac; armequipe = perso.armequipe; statBonusPot = perso.statBonusPot} 0
					else (lvlgain,{nom=perso.nom; genre=perso.genre; classe=perso.classe; exp=nvxp; lvl=perso.lvl; stats_base = perso.stats_base; sac=perso.sac; armequipe = perso.armequipe; statBonusPot = perso.statBonusPot})
			in aux p.lvl p exp
	(** Fonction qui permet de modifier les pv d'un personnage
	@param p : le personnage à modifier
	@param pv : le montant de pv ajouter
	@return le personnage avec les pv modifiés*)
	let modif_pv : personnage -> float -> personnage = fun p pv -> 
		if p.stats_base.stats.pv+.pv>=(p.stats_base.pv_max)+.(Equipement.Equipement.get_stats p.armequipe).pv
			then {nom=p.nom; genre=p.genre; classe=p.classe; lvl=p.lvl; exp=p.exp; stats_base = {stats={pv = (p.stats_base.pv_max)+.(Equipement.Equipement.get_stats p.armequipe).pv; atk = p.stats_base.stats.atk; def = p.stats_base.stats.def; acc = p.stats_base.stats.acc};pv_max = p.stats_base.pv_max}; sac=p.sac; armequipe = p.armequipe; statBonusPot = p.statBonusPot}
			else {nom=p.nom; genre=p.genre; classe=p.classe; lvl=p.lvl; exp=p.exp; stats_base = {stats={pv = (p.stats_base.stats.pv)+.pv; atk = p.stats_base.stats.atk; def = p.stats_base.stats.def; acc = p.stats_base.stats.acc};pv_max = p.stats_base.pv_max}; sac=p.sac; armequipe = p.armequipe; statBonusPot = p.statBonusPot}
	(** Fonction qui renvoie les dégats infligé par un personnage en fonction de sa classe ou 0 s'il rate
	@param p : le personnage
	@return les dégats infligés*)
	let frapper : personnage -> int = fun p ->
		let de = Random.int 100 in
			if de<p.stats_base.stats.acc+p.statBonusPot.acc+p.lvl*5 then p.stats_base.stats.atk+(Equipement.Equipement.get_stats p.armequipe).atk+p.statBonusPot.atk else 0
	(** Fonction qui permet de modifier le sac du personnage
	@param p : le personnage
	@param obj : l'objet à modifier
	@param qte : la quantité à rajouter
	@return le personnage avec son sac modifié*)
	let modif_sac_perso : personnage -> Objet.Objet.contenu -> int -> personnage = fun p obj qte -> {nom=p.nom; genre=p.genre; classe=p.classe; lvl=p.lvl; exp=p.exp; stats_base = p.stats_base; sac= Objet.Objet.modif_sac p.sac obj qte; armequipe = p.armequipe; statBonusPot = p.statBonusPot}

	(** Fonction qui soigne de 4 pv le personnage et consomme 1 poulet si le personnage en possède
	@author Noémie L
	@param p : le personnage
	@return true et le personnage qui à mangé ou false et le personnage qui n'a pas mangé *)
	let manger : personnage -> bool*personnage = fun p ->
		if Objet.Objet.qte_obj p.sac Poulet > 0
			then (true,modif_pv (modif_sac_perso p Poulet (-1)) 2.)
			else (false,p)

	(** Fonction permettant de savoir si le personnage a des objets sur lui et de retourner ceux dont la quantite est differente de 0
@param perso : le personnage dont on veut savoir si il a des objets
@return un tuple avec un booleen true si le personnage a des objets sur lui, false sinon et un sac avec les objets que possede le personnage*)
let peut_perdre_objet : personnage -> bool*Objet.Objet.sac = fun perso ->
	let lesac = perso.sac in
	if (Objet.Objet.qte_obj lesac Eponge=0)&&(Objet.Objet.qte_obj lesac Poulet=0)&&(Objet.Objet.qte_obj lesac Piece=0)&&((Objet.Objet.qte_obj lesac Potion_Precision)=0)&&((Objet.Objet.qte_obj lesac Potion_Puissance)=0) then (false, lesac)
	else 
	let rec aux : Objet.Objet.sac -> Objet.Objet.sac -> Objet.Objet.sac = fun lesac lesobj ->
	match lesac with
	| [] -> lesobj
	| Objet.Objet.{quantite=_;obj=Arme _}::tail -> aux tail lesobj
	| Objet.Objet.{quantite=qte;obj=ctn}::tail when qte=0 -> aux tail lesobj
	| Objet.Objet.{quantite=qte;obj=ctn}::tail -> aux tail (Objet.Objet.{quantite=qte;obj=ctn}::lesobj)
	in (true, aux lesac [])

(** Fonction permettant de faire perdre ou non un objet a un personnage
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

	(** Fonction qui convertit le choix de l'utiliateur (en string) en arme
	@param p : le personnage
	@param t : le string du choix de l'utilisateur
	@return l'arme correspondant au choix*)
	let convertit_string_en_arme : personnage -> string -> Equipement.Equipement.arme = fun p t ->
		if p.classe = Guerrier then
			match t with
			|s when s="bois"||s="Bois" ->Equipement.Equipement.creer_arme (G Epee_en_bois)
			|s when s="nuada"||s="Nuada" ->Equipement.Equipement.creer_arme (G Epee_de_nuada)
			|s when s="kusanagi"||s="Kusanagi" ->Equipement.Equipement.creer_arme (G Kusanagi_et_Yata_no_kagami)
			|s when s="aegis"||s="Aegis" ->Equipement.Equipement.creer_arme (G Aegis)
			|s when s="durandal"||s="Durandal" ->Equipement.Equipement.creer_arme (G Durandal)
			|s when s="excalibur"||s="Excalibur" ->Equipement.Equipement.creer_arme (G Excalibur)
			|_ -> Equipement.Equipement.creer_arme (G Epee_en_bois)
			else if p.classe = Archer then
				match t with
				|s when s="bois"||s="Bois" ->Equipement.Equipement.creer_arme (A Arc_en_bois)
				|s when s="artemis"||s="Artemis" ->Equipement.Equipement.creer_arme (A Arc_Artemis)
				|s when s="gandiva"||s="Gandiva" ->Equipement.Equipement.creer_arme (A Gandiva)
				|s when s="bouclier"||s="Bouclier" ->Equipement.Equipement.creer_arme (A Arc_bouclier_immortel)
				|s when s="zephyr"||s="Zephyr" ->Equipement.Equipement.creer_arme (A Zephyr)
				|s when s="lumiere"||s="Lumiere" ->Equipement.Equipement.creer_arme (A Arc_de_lumiere)
				|_ -> Equipement.Equipement.creer_arme (A Arc_en_bois)
				else
					match t with
					|s when s="bois"||s="Bois" ->Equipement.Equipement.creer_arme (M Baton_en_bois)
					|s when s="gae"||s="Gae" ->Equipement.Equipement.creer_arme (M Gae_bolga)
					|s when s="caducee"||s="Caducee" ->Equipement.Equipement.creer_arme (M Caducee)
					|s when s="ino"||s="Ino" ->Equipement.Equipement.creer_arme (M Voile_Ino)
					|s when s="gambanteinn"||s="Gambanteinn" ->Equipement.Equipement.creer_arme (M Gambanteinn)
					|s when s="sureau"||s="Sureau" ->Equipement.Equipement.creer_arme (M Baguette_de_sureau)
					|_ -> Equipement.Equipement.creer_arme (M Baton_en_bois)

	(** Fonction permettant de changer l'arme du joueur
	@param p : le personnage
	@param t : le type de l'arme en string
	@return le personnage qui a changé d'arme*)
	let change_arme : personnage -> string -> personnage = fun p t ->
		{nom=p.nom;genre=p.genre;classe=p.classe;lvl=p.lvl;exp=p.exp;stats_base={stats={pv=min p.stats_base.stats.pv (p.stats_base.pv_max+.(Equipement.Equipement.get_stats(convertit_string_en_arme p t)).pv);atk=p.stats_base.stats.atk;def=p.stats_base.stats.def;acc=p.stats_base.stats.acc};pv_max=p.stats_base.pv_max};sac=p.sac;armequipe=(convertit_string_en_arme p t);statBonusPot=p.statBonusPot}

	(** Fonction qui donne les bonus/malus d'une potion au personnage
	@param p : le personnage
	@param s : la stats en string à augmenter/diminuer
	@param i : la valeur du bonus/malus
	@return le personnage modifié*)
	let modif_stats_pot : personnage -> string -> int -> personnage = fun p s i ->
		if s = "atk" then
			{nom=p.nom;genre=p.genre;classe=p.classe;lvl=p.lvl;exp=p.exp;stats_base=p.stats_base;sac=p.sac;armequipe=p.armequipe;statBonusPot=Equipement.Equipement.{pv=0.;atk=i;def=0;acc=0}}
		else
			{nom=p.nom;genre=p.genre;classe=p.classe;lvl=p.lvl;exp=p.exp;stats_base=p.stats_base;sac=p.sac;armequipe=p.armequipe;statBonusPot=Equipement.Equipement.{pv=0.;atk=p.statBonusPot.atk;def=0;acc=i}}


	(** Fonction qui permet de reset les stats bonus des potions
	@param p : le personnage
	@return le personnage modifié*)
	let reset_stats_pot : personnage -> personnage = fun p ->
		{nom=p.nom;genre=p.genre;classe=p.classe;lvl=p.lvl;exp=p.exp;stats_base=p.stats_base;sac=p.sac;armequipe=p.armequipe;statBonusPot=Equipement.Equipement.stats_base()}

	(** Fonction qui génère une arme que le joueur ne possède pas pour que le marchand lui la propose en vente
	@param p : le personnage
	@return l'objet marchand que le marchand proposera*)
	let generer_arme_marchand : personnage -> Objet.Objet.objet_marchand = fun p ->
		if p.classe = Guerrier then
			let rec aux : Objet.Objet.sac -> Objet.Objet.contenu list ->  Objet.Objet.contenu list = fun s res ->
				match s with
				|[] -> res
				|Objet.Objet.{quantite=qte;obj=(Arme (G a))}::t when qte=0 && a<>Excalibur -> aux t ((Arme (G a))::res)
				|_::t -> aux t res
			in let obj_obtenable = aux p.sac [] in
			if obj_obtenable = [] 
				then if Objet.Objet.qte_obj p.sac (Arme (G Excalibur)) = 0 
					then Objet.Objet.{quantite = 1; prix = 777; obj = (Arme (G Excalibur))}
					else Objet.Objet.{quantite = 0; prix = 0; obj = (Arme (G Epee_en_bois))}
				else
					let chance = Random.int (List.length obj_obtenable) in Objet.Objet.{quantite = 1; prix = 250; obj = (List.nth obj_obtenable chance)}
		else if p.classe = Archer then
			let rec aux : Objet.Objet.sac -> Objet.Objet.contenu list ->  Objet.Objet.contenu list = fun s res ->
				match s with
				|[] -> res
				|Objet.Objet.{quantite=qte;obj=(Arme (A a))}::t when qte=0 && a<>Arc_de_lumiere -> aux t ((Arme (A a))::res)
				|_::t -> aux t res
			in let obj_obtenable = aux p.sac [] in
			if obj_obtenable = [] 
				then if Objet.Objet.qte_obj p.sac (Arme (A Arc_de_lumiere)) = 0 
					then Objet.Objet.{quantite = 1; prix = 777; obj = (Arme (A Arc_de_lumiere))}
					else Objet.Objet.{quantite = 0; prix = 0; obj = (Arme (A Arc_en_bois))}
				else
					let chance = Random.int (List.length obj_obtenable) in Objet.Objet.{quantite = 1; prix = 250; obj = (List.nth obj_obtenable chance)}
		else
			let rec aux : Objet.Objet.sac -> Objet.Objet.contenu list ->  Objet.Objet.contenu list = fun s res ->
				match s with
				|[] -> res
				|Objet.Objet.{quantite=qte;obj=(Arme (M a))}::t when qte=0 && a<>Baguette_de_sureau -> aux t ((Arme (M a))::res)
				|_::t -> aux t res
			in let obj_obtenable = aux p.sac [] in
				if obj_obtenable = [] 
					then if Objet.Objet.qte_obj p.sac (Arme (M Baguette_de_sureau)) = 0 
						then Objet.Objet.{quantite = 1; prix = 777; obj = (Arme (M Baguette_de_sureau))}
						else Objet.Objet.{quantite = 0; prix = 0; obj = (Arme (M Baton_en_bois))}
					else
						let chance = Random.int (List.length obj_obtenable) in Objet.Objet.{quantite = 1; prix = 250; obj = (List.nth obj_obtenable chance)}


		
    let bonus : personnage -> Objet.Objet.contenu -> int*personnage = fun perso ctn ->
        let lenb = Random.int 100 in
        if ctn=Potion_Precision then 
            (if lenb<5 then (-10,(modif_stats_pot perso "acc" (-10)))
            else (if lenb<25 then (-5,(modif_stats_pot perso "acc" (-5)))
                        else (if lenb<65 then (5,(modif_stats_pot perso "acc" (5)))
                                    else (if lenb<90 then (10,(modif_stats_pot perso "acc" (10)))
                                                else (15,(modif_stats_pot perso "acc" (15)))))))
        else (if lenb<5 then (-2,(modif_stats_pot perso "atk" (-2)))
                else (if lenb<25 then (-1,(modif_stats_pot perso "atk" (-1)))
                            else (if lenb<65 then (1,(modif_stats_pot perso "atk" (1)))
                                        else (if lenb<90 then (2,(modif_stats_pot perso "atk" (2)))
                                                    else (3,(modif_stats_pot perso "atk" (3)))))))
		
	end
;;