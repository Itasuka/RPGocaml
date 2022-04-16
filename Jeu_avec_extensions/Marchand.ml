Random.self_init();;

module type SIGMARCHAND = 
  sig

  exception Pas_Assez_DArgent

  exception Pas_Assez_DeStock

  type marchand = { nom : string; stock : Objet.Objet.stock; stock_vente : Objet.Objet.stock_vente}

  val creer_marchand : Personnage.Personnage.personnage -> marchand

  val peut_acheter_marchand : Personnage.Personnage.personnage -> marchand -> Objet.Objet.contenu -> int -> Personnage.Personnage.personnage * marchand 

  val peut_acheter_aub : Personnage.Personnage.personnage -> int -> Personnage.Personnage.personnage

  val achat_perte_piece : Personnage.Personnage.personnage -> int -> int -> Personnage.Personnage.personnage

  val achat_ajouter_contenu : Personnage.Personnage.personnage -> Objet.Objet.contenu -> int -> Personnage.Personnage.personnage

  val vente_gain_piece : Personnage.Personnage.personnage -> int -> int -> Personnage.Personnage.personnage

  val vente_perte_contenu : Personnage.Personnage.personnage -> Objet.Objet.contenu -> int -> Personnage.Personnage.personnage

end

module Marchand : SIGMARCHAND =
struct

  type marchand = { nom : string; stock : Objet.Objet.stock; stock_vente : Objet.Objet.stock_vente}

  (** Fonction permettant de générer un nom aléatoire au marchand
  @author Noémie L
  @return un nom pour le marchand*)
  let nom_marchand = fun () ->
    let lenb = Random.int 5 in
    match lenb with
    |0 ->"Ollerobo"
    |1 ->"Pasherissi"
    |2 ->"Leklair"
    |3 ->"Padarnak"
    |4 ->"Karphour"
    |_ -> ""

  	(** Fonction permettant de créer un stock pour le marchand 
    @author Noémie L
    @param p : le personnage jouant
    @return un stock de marchand*)
    let creer_stock_marchand : Personnage.Personnage.personnage -> Objet.Objet.stock = fun p ->
			[{quantite = ((Random.int 5)+2) ; prix = ((Random.int 3)+2); obj = Eponge}
							 ;{quantite = ((Random.int 5)+2) ; prix = ((Random.int 4)+2); obj = Poulet}
							 ;(Personnage.Personnage.generer_arme_marchand p)]

    (** Fonction permettant de créer un marchand 
    @author Noémie L
    @param perso : le personnage jouant
    @return un marchand*)
    let creer_marchand : Personnage.Personnage.personnage -> marchand = fun perso -> { nom = (nom_marchand ()); stock = (creer_stock_marchand perso); stock_vente = (Objet.Objet.creer_stock_vente_marchand ())}

  (** Fonction permettant d'ajouter un contenu à un personnage selon une quantite lors d'un achat
  @author Noémie L
  @param perso : le personnage jouant
  @param ctn : le contenu acheté
  @param qte : le quantite ajouté*)
  let achat_ajouter_contenu : Personnage.Personnage.personnage -> Objet.Objet.contenu -> int -> Personnage.Personnage.personnage= fun perso ctn qte ->
      Personnage.Personnage.modif_sac_perso perso ctn qte
  
  (** Fonction permettant de retirer un contenu à un personnage selon une quantite lors d'une vente
  @author Noémie L
  @param perso : le personnage jouant
  @param ctn : le contenu vendu
  @param qte : le quantite retiré*)
  let vente_perte_contenu : Personnage.Personnage.personnage -> Objet.Objet.contenu -> int -> Personnage.Personnage.personnage= fun perso ctn qte ->
      Personnage.Personnage.modif_sac_perso perso ctn (-qte)
  
  (** Fonction permettant de retirer des pièces à un personnage selon une quantite de contenu acheté et son prix
  @author Noémie L
  @param perso : le personnage jouant
  @param qte : le quantite de contenu acheté
  @param prix : le prix du contenu*)
  let achat_perte_piece : Personnage.Personnage.personnage -> int -> int -> Personnage.Personnage.personnage= fun perso qte prix ->
    Personnage.Personnage.modif_sac_perso perso Piece (-(qte*prix))
  
  (** Fonction permettant d'ajouter des pièces à un personnage selon une quantite de contenu vendu et son prix
  @author Noémie L
  @param perso : le personnage jouant
  @param qte : le quantite de contenu vendu
  @param prix : le prix du contenu*)
  let vente_gain_piece : Personnage.Personnage.personnage -> int -> int -> Personnage.Personnage.personnage= fun perso qte prix ->
    Personnage.Personnage.modif_sac_perso perso Piece (qte*prix)
  
  (** Fonction permettant de faire baisser le stock d'un marchand lors d'un achat
  @author Noémie L
  @param marchand : le marchand
  @param le contenu dont on veut baisser la quantité
  @param la quantité retirée*)
  let achat_marchand : marchand -> Objet.Objet.contenu -> int -> marchand = fun marchand ctn qte->
    let stock = marchand.stock in
    let nvstock = Objet.Objet.modif_stock stock ctn (-qte) in
      {nom=marchand.nom;stock=nvstock;stock_vente = marchand.stock_vente}

  exception Pas_Assez_DeStock

  exception Pas_Assez_DArgent
  
  (**Fonction permettant de réaliser un achat d'un personnage chez un marchand
  @author Noémie L
  @param perso : le personnage jouant
  @param marchand : le marchand auquel le personnage achete quelque chose
  @param ctn : le contenu acheté
  @param qte : la quantité de contenu achetée
  @raise Pas_Assez_DeStock quand la quantité demandée est trop élevée
  @raise Pas_Assez_DArgent quand le personnage ne possède pas assez d'argent pour acheter
  @return le personnage modifié et le marchand modifié sous la forme d'un tuple*)
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

  (**Fonction permettant de réaliser un achat de poulet d'un personnage chez l'aubergiste
  @author Noémie L
  @param perso : le personnage jouant
  @param qte : la quantité de poulets achetée
  @raise Pas_Assez_DArgent quand le personnage ne possède pas assez d'argent pour acheter
  @return le personnage modifié*)
  let peut_acheter_aub : Personnage.Personnage.personnage -> int -> Personnage.Personnage.personnage = fun perso qte ->
    let piece_sac = Objet.Objet.qte_obj perso.sac Piece in 
    let prix = 4*qte in
    if piece_sac<prix then raise Pas_Assez_DArgent
    else let perso1 = achat_perte_piece perso qte 4 in
      let perso2 = achat_ajouter_contenu perso1 Poulet qte in
    (perso2)


end
;;