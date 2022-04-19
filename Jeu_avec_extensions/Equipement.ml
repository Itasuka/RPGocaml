module type EQUIPEMENT =
  sig

  type type_arme_guerrier = | Epee_en_bois
                            | Epee_de_nuada
                            | Kusanagi_et_Yata_no_kagami
                            | Aegis
                            | Durandal
                            | Excalibur

  type type_arme_archer = | Arc_en_bois
                          | Arc_Artemis
                          | Gandiva
                          | Arc_bouclier_immortel
                          | Zephyr
                          | Arc_de_lumiere

  type type_arme_magicien = | Baton_en_bois
                            | Gae_bolga
                            | Caducee
                            | Voile_Ino
                            | Gambanteinn
                            | Baguette_de_sureau

  type type_arme = G of type_arme_guerrier | A of type_arme_archer | M of type_arme_magicien

  type stats = {pv : float; atk : int; def : int; acc : int}

  type arme_guerrier = {arme : type_arme_guerrier; stats : stats; specialite : string}

  type arme_archer = {arme : type_arme_archer; stats : stats; specialite : string}

  type arme_magicien = {arme : type_arme_magicien; stats : stats; specialite : string}

  type arme = G of arme_guerrier | A of arme_archer | M of arme_magicien

  val stats_base : unit -> stats

  val stats_atk : unit -> stats

  val stats_pv : unit -> stats

  val stats_def : unit -> stats

  val stats_acc : unit -> stats
  
  val stats_leg : unit -> stats

  val creer_arme_guerrier : type_arme_guerrier -> arme_guerrier

  val creer_arme_archer : type_arme_archer -> arme_archer

  val creer_arme_magicien : type_arme_magicien -> arme_magicien

  val creer_arme : type_arme -> arme

  val get_stats : arme -> stats

  val get_spe : arme -> string

end
;;

module Equipement : EQUIPEMENT = 
  struct

  (** Type représentant les noms des différentes armes d'un guerrier
  @author Nicolas S*)
  type type_arme_guerrier = | Epee_en_bois
                            | Epee_de_nuada
                            | Kusanagi_et_Yata_no_kagami
                            | Aegis
                            | Durandal
                            | Excalibur

  (** Type représentant les noms des différentes armes d'un archer
  @author Nicolas S*)
  type type_arme_archer = | Arc_en_bois
                          | Arc_Artemis
                          | Gandiva
                          | Arc_bouclier_immortel
                          | Zephyr
                          | Arc_de_lumiere

  (** Type représentant les noms des différentes armes d'un magicien
  @author Nicolas S*)  
  type type_arme_magicien = | Baton_en_bois
                            | Gae_bolga
                            | Caducee
                            | Voile_Ino
                            | Gambanteinn
                            | Baguette_de_sureau

  (** Type regroupant les différents type d'arme
  @author Nicolas S*)
  type type_arme = G of type_arme_guerrier | A of type_arme_archer | M of type_arme_magicien

  (** Type définissant les statistiques qui seront attribuée à une arme
  @author Nicolas S*)
  type stats = {pv : float; atk : int; def : int; acc : int}

  (** Type représentant une arme d'un guerrier avec son type, ses stats et sa spécialité
  @author Nicolas S*)
  type arme_guerrier = {arme : type_arme_guerrier; stats : stats; specialite : string}

  (** Type représentant une arme d'un archer avec son type, ses stats et sa spécialité
  @author Nicolas S*)
  type arme_archer = {arme : type_arme_archer; stats : stats; specialite : string}

  (** Type représentant une arme d'un magicien avec son type, ses stats et sa spécialité
  @author Nicolas S*)
  type arme_magicien = {arme : type_arme_magicien; stats : stats; specialite : string}

  (** Type regroupant les différentes armes
  @author Nicolas S*)
  type arme = G of arme_guerrier | A of arme_archer | M of arme_magicien

  (** Fonction qui définit les stats d'une arme de spécialité "Aucune"
  @author Nicolas S
  @return les stats d'une arme de spécialité "Aucune"*)
  let stats_base : unit -> stats = fun () -> {pv = 0.; atk = 0; def = 0; acc = 0}

  (** Fonction qui définit les stats d'une arme de spécialité "Attaque"
  @author Nicolas S
  @return les stats d'une arme de spécialité "Attaque"*)
  let stats_atk : unit -> stats = fun () -> {pv = 0.; atk = 5; def = -2; acc = 5}

  (** Fonction qui définit les stats d'une arme de spécialité "Points de vie"
  @author Nicolas S
  @return les stats d'une arme de spécialité "Points de vie"*)
  let stats_pv : unit -> stats = fun () -> {pv = 20.; atk = 2; def = -2; acc = 5}

  (** Fonction qui définit les stats d'une arme de spécialité "Defense"
  @author Nicolas S
  @return les stats d'une arme de spécialité "Defense"*)
  let stats_def : unit -> stats = fun () -> {pv = 7.; atk = -1; def = 1; acc = -10}

  (** Fonction qui définit les stats d'une arme de spécialité "Precision"
  @author Nicolas S
  @return les stats d'une arme de spécialité "Precision"*)
  let stats_acc : unit -> stats = fun () -> {pv = 0.; atk = -1; def = -1; acc = 20}

  (** Fonction qui définit les stats d'une arme de spécialité "Legendaire"
  @author Nicolas S
  @return les stats d'une arme de spécialité "Legendaire"*)
  let stats_leg : unit -> stats = fun () -> {pv = 20.; atk = 5; def = 1; acc = 20}
  

  (** Fonction qui créer une arme_guerrier complète à partir d'un type_arme_guerrier
  @author Nicolas S
  @param t : le type_arme_guerrier
  @return une arme_guerrier*)
  let creer_arme_guerrier : type_arme_guerrier -> arme_guerrier = fun t ->
    match t with
    | Epee_en_bois -> {arme = Epee_en_bois; stats = stats_base(); specialite = "Aucune"}
    | Epee_de_nuada -> {arme = Epee_de_nuada; stats = stats_atk(); specialite = "Attaque"}
    | Kusanagi_et_Yata_no_kagami -> {arme = Kusanagi_et_Yata_no_kagami; stats = stats_pv(); specialite = "Points de vie"}
    | Aegis -> {arme = Aegis; stats = stats_def(); specialite = "Defense"}
    | Durandal -> {arme = Durandal; stats = stats_acc(); specialite = "Precision"}
    | Excalibur -> {arme = Excalibur; stats = stats_leg(); specialite = "Legendaire"}

  (** Fonction qui créer une arme_archer complète à partir d'un type_arme_archer
  @author Nicolas S
  @param t : le type_arme_archer
  @return une arme_archer*)
  let creer_arme_archer : type_arme_archer -> arme_archer = fun t ->
    match t with
    | Arc_en_bois -> {arme = Arc_en_bois; stats = stats_base(); specialite = "Aucune"}
    | Arc_Artemis -> {arme = Arc_Artemis; stats = stats_atk(); specialite = "Attaque"}
    | Gandiva -> {arme = Gandiva; stats = stats_pv(); specialite = "Points de vie"}
    | Arc_bouclier_immortel -> {arme = Arc_bouclier_immortel; stats = stats_def(); specialite = "Defense"}
    | Zephyr -> {arme = Zephyr; stats = stats_acc(); specialite = "Precision"}
    | Arc_de_lumiere -> {arme = Arc_de_lumiere; stats = stats_leg(); specialite = "Legendaire"}

  (** Fonction qui créer une arme_magicien complète à partir d'un type_arme_magicien
  @author Nicolas S
  @param t : le type_arme_magicien
  @return une arme_magicien*)
  let creer_arme_magicien : type_arme_magicien -> arme_magicien = fun t ->
    match t with
    | Baton_en_bois -> {arme = Baton_en_bois; stats = stats_base(); specialite = "Aucune"}
    | Gae_bolga -> {arme = Gae_bolga; stats = stats_atk(); specialite = "Attaque"}
    | Caducee -> {arme = Caducee; stats = stats_pv(); specialite = "Points de vie"}
    | Voile_Ino -> {arme = Voile_Ino; stats = stats_def(); specialite = "Defense"}
    | Gambanteinn -> {arme = Gambanteinn; stats = stats_acc(); specialite = "Precision"}
    | Baguette_de_sureau -> {arme = Baguette_de_sureau; stats = stats_leg(); specialite = "Legendaire"}

  (** Fonction qui appelle les fonctions de création d'arme spécifique afin d'avoir un type arme
  @author Nicolas S
  @param t : le type_arme
  @return l'arme*)
  let creer_arme : type_arme -> arme = fun t ->
    match t with
    | G a -> G (creer_arme_guerrier a)
    | A a -> A (creer_arme_archer a)
    | M a -> M (creer_arme_magicien a)

  (** Fonction permettant de récupérer les statistiques d'une arme
  @author Nicolas S
  @param a : l'arme
  @return les stats de a*)
  let get_stats : arme -> stats = fun a ->
    match a with
    | G a -> a.stats
    | A a -> a.stats
    | M a -> a.stats

  (** Foncion permettant de récupérer la spécialité d'une arme
  @author Nicolas S
  @param a : l'arme
  @return la spécialité de a*)
  let get_spe : arme -> string = fun a ->
    match a with
    | G a -> a.specialite
    | A a -> a.specialite
    | M a -> a.specialite

end
;;