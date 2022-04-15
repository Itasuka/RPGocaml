module Equipement = 
  struct

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

  let stats_base : unit -> stats = fun () -> {pv = 0.; atk = 0; def = 0; acc = 0}
  let stats_atk : unit -> stats = fun () -> {pv = 0.; atk = 5; def = -2; acc = 5}
  let stats_pv : unit -> stats = fun () -> {pv = 20.; atk = 2; def = -2; acc = 5}
  let stats_def : unit -> stats = fun () -> {pv = 7.; atk = -1; def = 1; acc = -10}
  let stats_acc : unit -> stats = fun () -> {pv = 0.; atk = -1; def = -1; acc = 20}
  let stats_leg : unit -> stats = fun () -> {pv = 20.; atk = 5; def = 1; acc = 20}

  let afficher_arme_guerrier : type_arme_guerrier -> string = fun a ->
    match a with
    | Epee_en_bois -> "Epee en bois"
    | Epee_de_nuada -> "Epee de Nuada"
    | Kusanagi_et_Yata_no_kagami -> "Kusanagi et Yata no Kagami"
    | Aegis -> "Aegis"
    | Durandal -> "Durandal"
    | Excalibur -> "Excalibur"

  let afficher_arme_archer : type_arme_archer -> string = fun a ->
    match a with
    | Arc_en_bois -> "Arc en bois"
    | Arc_Artemis -> "Arc d'Artemis"
    | Gandiva -> "Gandiva"
    | Arc_bouclier_immortel -> "Arc bouclier immortel"
    | Zephyr -> "Zephyr"
    | Arc_de_lumiere -> "Arc de lumiere"

    let afficher_arme_magicien : type_arme_magicien -> string = fun a ->
      match a with
      | Baton_en_bois -> "Baton en bois"
      | Gae_bolga -> "Gae Bolga"
      | Caducee -> "Caducee"
      | Voile_Ino -> "Voile d'Ino"
      | Gambanteinn -> "Gambanteinn"
      | Baguette_de_sureau -> "Baguette de sureau"

  let afficher_arme : arme -> string = fun a ->
    match a with
    | G arme -> afficher_arme_guerrier arme.arme
    | A arme -> afficher_arme_archer arme.arme
    | M arme -> afficher_arme_magicien arme.arme

    let creer_arme_guerrier : type_arme_guerrier -> arme_guerrier = fun t ->
      match t with
      | Epee_en_bois -> {arme = Epee_en_bois; stats = stats_base(); specialite = "Aucune"}
      | Epee_de_nuada -> {arme = Epee_de_nuada; stats = stats_atk(); specialite = "Attaque"}
      | Kusanagi_et_Yata_no_kagami -> {arme = Kusanagi_et_Yata_no_kagami; stats = stats_pv(); specialite = "Points de vie"}
      | Aegis -> {arme = Aegis; stats = stats_def(); specialite = "Defense"}
      | Durandal -> {arme = Durandal; stats = stats_acc(); specialite = "Precision"}
      | Excalibur -> {arme = Excalibur; stats = stats_leg(); specialite = "Legendaire"}

    let creer_arme_archer : type_arme_archer -> arme_archer = fun t ->
      match t with
      | Arc_en_bois -> {arme = Arc_en_bois; stats = stats_base(); specialite = "Aucune"}
      | Arc_Artemis -> {arme = Arc_Artemis; stats = stats_atk(); specialite = "Attaque"}
      | Gandiva -> {arme = Gandiva; stats = stats_pv(); specialite = "Points de vie"}
      | Arc_bouclier_immortel -> {arme = Arc_bouclier_immortel; stats = stats_def(); specialite = "Defense"}
      | Zephyr -> {arme = Zephyr; stats = stats_acc(); specialite = "Precision"}
      | Arc_de_lumiere -> {arme = Arc_de_lumiere; stats = stats_leg(); specialite = "Legendaire"}

    let creer_arme_magicien : type_arme_magicien -> arme_magicien = fun t ->
      match t with
      | Baton_en_bois -> {arme = Baton_en_bois; stats = stats_base(); specialite = "Aucune"}
      | Gae_bolga -> {arme = Gae_bolga; stats = stats_atk(); specialite = "Attaque"}
      | Caducee -> {arme = Caducee; stats = stats_pv(); specialite = "Points de vie"}
      | Voile_Ino -> {arme = Voile_Ino; stats = stats_def(); specialite = "Defense"}
      | Gambanteinn -> {arme = Gambanteinn; stats = stats_acc(); specialite = "Precision"}
      | Baguette_de_sureau -> {arme = Baguette_de_sureau; stats = stats_leg(); specialite = "Legendaire"}

  let creer_arme : type_arme -> arme = fun t ->
    match t with
    | G a -> G (creer_arme_guerrier a)
    | A a -> A (creer_arme_archer a)
    | M a -> M (creer_arme_magicien a)

  let get_stats : arme -> stats = fun a ->
    match a with
    | G a -> a.stats
    | A a -> a.stats
    | M a -> a.stats

  let get_spe : arme -> string = fun a ->
    match a with
    | G a -> a.specialite
    | A a -> a.specialite
    | M a -> a.specialite

end
;;