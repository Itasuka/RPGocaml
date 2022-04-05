Random.self_init();;

module type OBJET =
	sig

	type contenu = Eponge | Piece | Poulet

	type objet = {quantite : int ; obj : contenu}

	type sac = objet list

	val creer_sac : unit -> sac

	val qte_obj : sac -> contenu -> int

	val modif_sac : sac -> contenu -> int -> sac

end
;;

module Objet : OBJET =
	struct
	type contenu = Eponge | Piece | Poulet
	type objet = {quantite : int ; obj : contenu}
	type sac = objet list
	(** Fonction qui initialise un sac vide
	@return un sac avec tout les objets disponibles avec une quantité 0*)
	let creer_sac : unit -> sac = 
		fun () -> [{quantite = 0 ; obj = Eponge}
					 ;{quantite = 0 ; obj = Piece}
					 ;{quantite = 0 ; obj = Poulet}]
	(** Fonction renvoie le nombre d'occurrence de obj dans sac
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
	end
;;