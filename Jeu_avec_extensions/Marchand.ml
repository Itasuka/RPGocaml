Random.self_init();;

module type SIGMARCHAND = 
  sig

end

module Marchand  =
struct

  type marchand = { nom : string; stock : Objet.Objet.stock; stock_vente : Objet.Objet.stock_vente}

  let nom_marchand = fun () ->
    let lenb = Random.int 5 in
    match lenb with
    |0 ->"Ollerobo"
    |1 ->"Pasherissi"
    |2 ->"Leklair"
    |3 ->"Padarnak"
    |4 ->"Karphour"
    |_ -> ""
    
    let creer_stock_marchand : Personnage.Personnage.personnage -> Objet.Objet.stock = fun p ->
			[{quantite = ((Random.int 5)+2) ; prix = ((Random.int 3)+2); obj = Eponge}
							 ;{quantite = ((Random.int 5)+2) ; prix = ((Random.int 4)+2); obj = Poulet}
							 ;(Personnage.Personnage.generer_arme_marchand p)]

    let creer_marchand : Personnage.Personnage.personnage -> marchand = fun perso -> { nom = (nom_marchand ()); stock = (creer_stock_marchand perso); stock_vente = (Objet.Objet.creer_stock_vente_marchand ())}

  let achat_ajouter_contenu : Personnage.Personnage.personnage -> Objet.Objet.contenu -> int -> Personnage.Personnage.personnage= fun perso ctn qte ->
      Personnage.Personnage.modif_sac_perso perso ctn qte
  
  let vente_perte_contenu : Personnage.Personnage.personnage -> Objet.Objet.contenu -> int -> Personnage.Personnage.personnage= fun perso ctn qte ->
      Personnage.Personnage.modif_sac_perso perso ctn (-qte)
  
  let achat_perte_piece : Personnage.Personnage.personnage -> int -> int -> Personnage.Personnage.personnage= fun perso qte prix ->
    Personnage.Personnage.modif_sac_perso perso Piece (-(qte*prix))
  
  let vente_gain_piece : Personnage.Personnage.personnage -> int -> int -> Personnage.Personnage.personnage= fun perso qte prix ->
    Personnage.Personnage.modif_sac_perso perso Piece (qte*prix)
  
  let achat_marchand : marchand -> Objet.Objet.contenu -> int -> marchand = fun marchand ctn qte->
    let stock = marchand.stock in
    let nvstock = Objet.Objet.modif_stock stock ctn (-qte) in
      {nom=marchand.nom;stock=nvstock;stock_vente = marchand.stock_vente}
  

  exception Pas_Assez_DeStock

  exception Pas_Assez_DArgent
    
  let peut_acheter_marchand : Personnage.Personnage.personnage -> marchand -> Objet.Objet.contenu -> int -> Personnage.Personnage.personnage * marchand = fun perso marchand ctn qte ->
    let piece_sac = Objet.Objet.qte_obj perso.sac Piece in 
    let stock = marchand.stock in
    let leprix = Objet.Objet.prix_obj_stock stock ctn in
    let prix = leprix*qte in
    if (Objet.Objet.qte_obj_stock stock ctn < qte) then raise Pas_Assez_DeStock
    else if piece_sac<prix then raise Pas_Assez_DArgent
    else let perso1 = achat_perte_piece perso qte leprix in
      let perso2 = achat_ajouter_contenu perso1 ctn qte in
    ((perso2),(achat_marchand marchand ctn qte))

  let peut_acheter_aub : Personnage.Personnage.personnage -> int -> Personnage.Personnage.personnage = fun perso qte ->
    let piece_sac = Objet.Objet.qte_obj perso.sac Piece in 
    let prix = 4*qte in
    if piece_sac<prix then raise Pas_Assez_DArgent
    else let perso1 = achat_perte_piece perso qte 4 in
      let perso2 = achat_ajouter_contenu perso1 Poulet qte in
    (perso2)


end
;;