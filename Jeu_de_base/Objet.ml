Random.self_init();;

(*----------------------------------------------------------SIGNATURE----------------------------------------------------------*)

module type OBJET =
	sig

	
	type contenu = Eponge | Piece | Poulet

	type objet = {quantite : int ; obj : contenu}
	
	type sac = objet list

	(* Fonction qui initialise un sac vide*)
	val creer_sac : unit -> sac

	(* Fonction renvoie le nombre d'occurrence de obj dans sac *)
	val qte_obj : sac -> contenu -> int

	(* Fonction qui modifie la quantité d'un objet dans un sac *)
	val modif_sac : sac -> contenu -> int -> sac

end
;;

(*------------------------------------------------------------MODULE----------------------------------------------------------------*)

module Objet : OBJET =
	struct

	(** Type représentant les objets obtenable dans le jeu
	@author Nicolas M*)
	type contenu = Eponge | Piece | Poulet

	(** Type représentant les objets avec leur quantité
	@author Nicolas M*)
	type objet = {quantite : int ; obj : contenu}

	(** Type représentant le sac d'un personnage
	@author Nicolas M*)
	type sac = objet list

	(** Fonction qui initialise un sac vide
	@author Nicolas M
	@return un sac avec tout les objets disponibles avec une quantité 0*)
	let creer_sac : unit -> sac = 
		fun () -> [{quantite = 0 ; obj = Eponge}
					 ;{quantite = 0 ; obj = Piece}
					 ;{quantite = 0 ; obj = Poulet}]

	(** Fonction renvoie le nombre d'occurrence de obj dans sac
	@author Nicolas S
	@param sac : le sac dans lequel on va compter le nombre d'occurence d'obj
	@param obj : le contenu dont on veut connaitre le nombre
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
	@param objet : le contenu dont la quantité va être modifié
	@param qte : quantité à rajouter
	@return le sac modifié
	@raise Pas_assez_Objet si on enlève une quantité supérieur à celle de départ*)
	let rec modif_sac : sac -> contenu -> int -> sac = fun sac objet qte ->
		match sac with
		|[] -> []
		|h::t when h.obj=objet -> 
			if h.quantite+qte<0 
				then raise Pas_assez_Objet
				else {quantite=h.quantite+qte;obj=h.obj}::t
		|h::t -> h::(modif_sac t objet qte)

	end
;;