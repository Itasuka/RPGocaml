module Marabout = 
  struct
  type marabout = { nom : string; stock_vente : Objet.Objet.stock_vente}

  exception Pas_Plus_Dune_Potion

  let nom_marabout = fun () ->
    let lenb = Random.int 5 in
    match lenb with
    |0 ->"Dfissel"
    |1 ->"Dha"
    |2 ->"Tchou"
    |3 ->"Tantrin"
    |4 ->"Tondore"
    |_ ->""

  let achat_potion_possible : Personnage.Personnage.personnage -> Objet.Objet.contenu -> bool = fun perso ctn ->
    let qte_pot = Objet.Objet.qte_obj perso.sac ctn in
    if qte_pot > 0 then false 
    else true 

  let peut_acheter_marabout : Personnage.Personnage.personnage -> marabout -> Objet.Objet.contenu -> int -> Personnage.Personnage.personnage = fun perso marabout ctn qte ->
    let piece_sac = Objet.Objet.qte_obj perso.sac Piece in 
    let stock = marabout.stock_vente in
    let leprix = Objet.Objet.prix_obj_stock_vente stock ctn in
    let prix = leprix*qte in
    let pos=achat_potion_possible perso ctn in
    if piece_sac<prix then raise Marchand.Marchand.Pas_Assez_DArgent
    else (if pos=false then raise Pas_Plus_Dune_Potion
    else let perso1 = Marchand.Marchand.achat_perte_piece perso qte leprix in
      let perso2 = Marchand.Marchand.achat_ajouter_contenu perso1 ctn qte in
      (perso2))

  end
;;

