Random.self_init();;

module type OBJET =
	sig

	type contenu = Eponge | Piece | Poulet | Arme of Equipement.Equipement.type_arme | Potion_Puissance | Potion_Precision

	type objet = {quantite : int ; obj : contenu}

	type objet_marchand = {quantite : int; prix : int; obj : contenu }

	type stock = objet_marchand list

	type sac = objet list

	type objet_vente = {prix : int ;  obj : contenu}

	type stock_vente = objet_vente list

	val creer_sac : unit -> sac

	val qte_obj : sac -> contenu -> int

	val modif_sac : sac -> contenu -> int -> sac

	val qte_obj_stock : stock -> contenu -> int

	val prix_obj_stock : stock -> contenu-> int

	val prix_obj_stock_vente : stock_vente -> contenu -> int

	val creer_stock_vente_marchand : unit -> stock_vente 

	val creer_stock_marabout : unit -> stock_vente

	val modif_stock : stock -> contenu -> int -> stock

	exception Pas_assez_Objet

end
;;

module Objet : OBJET  =
	struct

	(** Type représentant les objets obtenable dans le jeu
	@author Nicolas M*)
	type contenu = Eponge | Piece | Poulet | Arme of Equipement.Equipement.type_arme | Potion_Puissance | Potion_Precision

	(** type représentant un objet avec sa quantité
	@author Nicolas M*)
	type objet = {quantite : int ; obj : contenu}

	(** Type représentant un objet que possède un marchand par un objet, sa quantité et son prix
	@author Noemie L*)
	type objet_marchand = {quantite : int; prix : int; obj : contenu }

	(** Type représentant le stock d'un marchand
	@author Noemie L*)
	type stock = objet_marchand list

	(** Type représentant un objet par son prix et son contenu
	@author Noemie L*)
	type objet_vente = {prix : int ;  obj : contenu}

	(** Type représentant la liste des objet qu'un marchand peut acheter au joueur
	@author Noemie L*)
	type stock_vente = objet_vente list

	(** Type représentant l'inventaire du personnage
	@author Nicolas M*)
	type sac = objet list

	(** Fonction qui initialise un sac vide
	@author Nicolas M
	@return un sac avec tout les objets disponibles avec une quantité 0*)
	let creer_sac : unit -> sac = 
		fun () -> [{quantite = 0 ; obj = Eponge}
					 ;{quantite = 0 ; obj = Piece}
					 ;{quantite = 0 ; obj = Poulet}
					 ;{quantite = 0 ; obj = Potion_Puissance}
					 ;{quantite = 0 ; obj = Potion_Precision}
					 ;{quantite = 0 ; obj = Arme (G Epee_en_bois)}
					 ;{quantite = 0 ; obj = Arme (G Epee_de_nuada)}
					 ;{quantite = 0 ; obj = Arme (G Kusanagi_et_Yata_no_kagami)}
					 ;{quantite = 0 ; obj = Arme (G Aegis)}
					 ;{quantite = 0 ; obj = Arme (G Durandal)}
					 ;{quantite = 0 ; obj = Arme (G Excalibur)}
					 ;{quantite = 0 ; obj = Arme (A Arc_en_bois)}
					 ;{quantite = 0 ; obj = Arme (A Arc_Artemis)}
					 ;{quantite = 0 ; obj = Arme (A Gandiva)}
					 ;{quantite = 0 ; obj = Arme (A Arc_bouclier_immortel)}
					 ;{quantite = 0 ; obj = Arme (A Zephyr)}
					 ;{quantite = 0 ; obj = Arme (A Arc_de_lumiere)}
					 ;{quantite = 0 ; obj = Arme (M Baton_en_bois)}
					 ;{quantite = 0 ; obj = Arme (M Gae_bolga)}
					 ;{quantite = 0 ; obj = Arme (M Caducee)}
					 ;{quantite = 0 ; obj = Arme (M Voile_Ino)}
					 ;{quantite = 0 ; obj = Arme (M Gambanteinn)}
					 ;{quantite = 0 ; obj = Arme (M Baguette_de_sureau)}]
					 
	(** Fonction renvoie le nombre d'occurrence de obj dans sac
	@author Nicolas S
	@param sac : le sac dans lequel on va compter le nombre d'occurence d'obj
	@param obj : l'obj dont on veut connaitre le nombre
	@return le nombre d'occurrence de obj dans sac*)
	let rec qte_obj : sac -> contenu -> int = fun sac obj ->
		match sac with
		|[] -> 0
		|o::t when o.obj=obj -> o.quantite
		|_::t -> qte_obj t obj

	exception Pas_assez_Objet

	(** Fonction qui modifie la quantité d'un objet dans un sac
	@author Nicolas S
	@param sac : le sac qui va être modifié
	@param objet : objet dont la quantité va être modifié
	@param qte : quantité à rajouter
	@return le sac modifier 
	@raise Pas_assez_Objet si on enlève une quantité supérieur à celle de départ*)
	let rec modif_sac : sac -> contenu -> int -> sac = fun sac objet qte ->
		match sac with
		|[] -> []
		|h::t when h.obj=objet -> 
			if h.quantite+qte<0 
				then raise Pas_assez_Objet
				else {quantite=h.quantite+qte;obj=h.obj}::t
		|h::t -> h::(modif_sac t objet qte)

		(** Fonction permettant de connaitre la quantite d'un contenu dans un stock
		@author Noémie L
		@param stock : le stock du marchand
		@return la quantite du contenu*)
		let rec qte_obj_stock : stock -> contenu -> int = fun stock obj ->
			match stock with
			|[] -> 0 
			|o::t when o.obj=obj -> o.quantite
			|_::t -> qte_obj_stock t obj
		
		(** Fonction permettant de connaitre le prix d'un contenu dans un stock
		@author Noémie L
		@param stock : le stock du marchand
		@return le prix du contenu*)
		let rec prix_obj_stock : stock -> contenu-> int = fun stock obj ->
			match stock with
			|[] -> 0 
			|o::t when o.obj=obj -> o.prix
			|_::t -> prix_obj_stock t obj
		
		(** Fonction permettant de connaitre la quantite d'un contenu dans un stock_vente
		@author Noémie L
		@param stock : le stock_vente du marchand
		@return le quantite du contenu*)
		let rec prix_obj_stock_vente : stock_vente -> contenu -> int = fun stockv obj ->
			match stockv with 
			|[] -> 0 
			|o::t when o.obj=obj -> o.prix
			|_::t -> prix_obj_stock_vente t obj

		(** Fonction permettant d'initialiser un stock de vente pour un marchand
		@author Noémie L
		@param stock_vente : le stock de vente du marchand
		@return un stock de vente pour marchand*)
		let creer_stock_vente_marchand : unit -> stock_vente =
				fun () -> [{prix = ((Random.int 3)+1)  ;  obj = Eponge}
				; {prix= ((Random.int 6)+1); obj= Poulet}]
		
		(** Fonction permettant d'initialiser un stock pour le marabout
		@author Nicolas M
		@param stock_vente : le stock du marchand
		@return le stock du marchand*)
		let creer_stock_marabout : unit -> stock_vente =
			fun () -> [{prix = ((Random.int 4)+5); obj = Potion_Precision}
			;{prix = ((Random.int 4)+5); obj = Potion_Puissance}]
		
		(** Fonction permettant de modifier le stock d'un marchand via une quantité
		@author Noémie L
		@param stck : le stock du marchand
		@param objet : le contenu du stock
		@param qte : la quantite que l'on veut retirer
		@return le nouveau stock modifié *)
		let rec modif_stock : stock -> contenu -> int -> stock = fun stck objet qte ->
				match stck with
					|[] -> []
					|h::t when h.obj=objet -> 
					if h.quantite+qte<0 
						then raise Pas_assez_Objet
						else {quantite=h.quantite+qte;prix=h.prix;obj=h.obj}::t
					|h::t -> h::(modif_stock t objet qte)


	end
;;