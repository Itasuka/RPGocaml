module Marabout = 
  struct
  (** Type représant un marabout par son nom et son stock d'objet
      @author Nicolas M*)
  type marabout = { nom : string; stock_vente : Objet.Objet.stock_vente}

  exception Pas_Plus_Dune_Potion
(** Type donnant un nom aléatoire au marabout
@author Nicolas M
@return le string du nom du marabout*)
  let nom_marabout : unit -> string = fun () ->
    let lenb = Random.int 5 in
    match lenb with
    |0 ->"Dfissel"
    |1 ->"Dha"
    |2 ->"Tchou"
    |3 ->"Tantrin"
    |4 ->"Tondore"
    |_ ->""


  (** Fonction permettant de créer un marabout en lui donnant un nom aléatoire et un stock
      @author Nicolas M
      @return le marabout crée*)
  let creer_marabout : unit -> marabout = fun () ->
    {nom = nom_marabout(); stock_vente = Objet.Objet.creer_stock_marabout()}


  (** Fonction qui permmet de vérifier si le joueur ne possède pas de potion
      @author Nicolas M
      @param perso : le personnage voulant acheter une potion
      @param ctn : l'emplacement des potion dans le sac
      @return false si le personnage a deja une potion true sinon*)
  let achat_potion_possible : Personnage.Personnage.personnage -> Objet.Objet.contenu -> bool = fun perso ctn ->
    let qte_pot = Objet.Objet.qte_obj perso.sac ctn in
    if qte_pot > 0 then false 
    else true 
  (** Fonction qui permet au joueur d'acheter une potion si il a assez d'argent et qu'il n'en as pas deja une
      @author Nicolas M
      @param perso : le personnage voulant acheter une potion
      @param marabout : le marabout qui vends la potion
      @param ctn : la potion vendu par le marabout
      @return l'exception Pas_Assez_DArgent si le joueur a moins de piece que le prix de l'objet sinon le nouvel etat du personnage avec le montant de pieces modifié et la potion ajoutée dans le sac*)
  let peut_acheter_marabout : Personnage.Personnage.personnage -> marabout -> Objet.Objet.contenu -> Personnage.Personnage.personnage = fun perso marabout ctn ->
    let piece_sac = Objet.Objet.qte_obj perso.sac Piece in 
    let stock = marabout.stock_vente in
    let leprix = Objet.Objet.prix_obj_stock_vente stock ctn in
    let prix = leprix*1 in
    let pos=achat_potion_possible perso ctn in
    if piece_sac<prix then raise Marchand.Marchand.Pas_Assez_DArgent
    else (if pos=false then raise Pas_Plus_Dune_Potion
    else let perso1 = Marchand.Marchand.achat_perte_piece perso 1 leprix in
      let perso2 = Marchand.Marchand.achat_ajouter_contenu perso1 ctn 1 in
      (perso2))

  end
;;

