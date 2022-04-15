module Foire =
  struct

  
		(** Fonction permettant d'avoir le string des règles des jeux
@return le string des règles des jeux*)
let description_jeu : unit -> string = fun () ->
  "\nJEU DE NIM : Ce jeu se joue avec un tas de bâtons formant une ligne . Chaque joueur, à tour de rôle, prends au maximum 3 bâtons dans le tas. Le gagnant est celui qui prends le dernier bâton. Vous pouvez gagner quatre fois votre mise ! 
  \nDE : Pour gagner à ce jeu il suffit d'obtenir un six et vous remporter six fois votre mise !
  \nMORPION : Les joueurs inscrivent tour à tour leur symbole sur une grille . Le premier qui parvient à aligner trois de ses symboles horizontalement, verticalement ou en diagonale gagne la partie. Ce jeu permet de remporter le double de sa mise.\n\n"

(** Fonction permettant d'avoir la phrase d'initialisation de la foire
@return le string de la phrase d'initialisation*)
let phrase_init_foire = fun () -> 
  let lenb= Random.int 3 in
  match lenb with
  | 0 -> "\n\"Bonjour ! N'hésiter à jouer à nos jeux pour remplir vos poches !\"\n"^(description_jeu ())
  | 1 -> "\n\"Bienvenue à la foire ! Venez pariez vos pièces !\"\n"^(description_jeu())
  | _ -> "\n\"Oye oye ! Ici, gains garanties !\"\n"^(description_jeu())

(** Fonction permettant d'avoir la demande de choix de jeu
@return le string de la demande*)
let afficher_jeu : Personnage.Personnage.personnage -> string = fun perso -> (Affichage.Affichage.piece_sac perso)^"A quel jeu voulez vous jouer ? \n(N) Nim \n(D) Dé \n(M) Morpion \n(P) Partir\n"

(** Fonction permettant de demander à l'utilisateur un choix de jeu
@return le choix de l'utilisateur*)
let rec demander_jeu : unit -> string = fun () -> let () = print_string "\n<?>" in 
  let reponse = read_line() in 
    if reponse = "N" || reponse = "n" || reponse = "D" || reponse = "d" || reponse = "M" || reponse = "m" || reponse= "p" || reponse ="P" then reponse else let () = print_string "\n Votre choix est invalide !\n" in demander_jeu ()

(** Fonction permettant d'avoir la demande de choix de mise
@return le string de la demande*) 
let afficher_mise : unit -> string = fun () -> "\nCombien de pièces voulez vous miser ? Entre 1 et 15 pièces.\n"

(** Fonction permettant de demander à l'utilisateur le montant de sa mise
@return le montant misé *)
let rec demander_mise : unit -> int = fun () -> let () = print_string "\n<?>" in 
  try
    let reponse = read_int() in
    if reponse > 0  && reponse <= 15 then reponse else let () = print_string "\n Le montant de la mise est invalide ! Il doit être compris entre 1 et 15 pièces\n" in demander_mise ()
  with
  |Failure _ -> let () = print_string "\nVous devez rentrer un nombre !\n" in demander_mise()

(** Fonction affichant la somme remportée en jouant a jeu d'argent
@param perso le personnage jouant au jeu
@param piece le nombre de piece remportée par le joueur
@return string*)
let phrase_gain : int -> string = fun piece ->
  if piece < 0 then "Vous avez perdu "^(string_of_int((abs piece)))^ " pièces.\n"
  else "Vous avez gagné "^(string_of_int(piece))^" pièces.\n"

(** Fonction permettant de faire un tas de batons
@param nbfois le nombre de batons que l'on souhaite
@return le tas*)
let ligne_de_batons : int -> string = fun nbfois ->
  let nombrefois = nbfois in 
  let rec aux = fun nb ligne ->
  if nb<=0 then ligne
  else aux (nb-1) (ligne^"|")
  in aux nombrefois ""


(** Fonction permettant d'avoir la demande de combien de baton la personne veut retirer
@return le string de la demande*)
let afficher_demander_qte_baton : unit -> string = fun () -> "\nCombien voulez-vous retirer de bâtons ? (Vous devez rentrez un nombre entre 1 et 3)\n"

(** Fonction demandant le nombre de baton que l'utilisateur souhaite retirer
@param tas le tas de la partie en cours
@return le nombre choisi par l'utilisateur*)
let rec demander_baton : int -> int = fun tas ->
  let () = print_string "\n<?>" in
    try let reponse = read_int () in if (reponse<1 || reponse>3 || reponse>tas) then let () = print_string "\nVous devez rentrer un nombre entre 1 et 3 et inférieur à la taille du tas !\n" in demander_baton tas else reponse
      with Failure _ -> let () = print_string "\nVous devez rentrer un nombre. \n" in demander_baton tas

(** Fonction qui genere une phrase de victoire aleatoire
@param piece le nombre de piece gagné par l'utilisateur
@return le string de la phrase*)
  let phrase_victoire = fun piece -> let i = Random.int 3 in match i with
      | 0 -> "Félicitation vous êtes le vainqueur ! " ^ "\n" ^(phrase_gain piece)^"\n"
      | 1 -> "Bien joué vous avez gagné cette partie ! " ^ "\n" ^(phrase_gain piece)^"\n"
      | _ -> "Quel talent ! Vous êtes le vainqueur de cette partie ! " ^ "\n" ^(phrase_gain piece)^"\n"

(** Fonction qui genere une phrase de defaite aleatoire
@param piece le nombre de piece perdu par l'utilisateur
@return le string de la phrase *)   
  let phrase_defaite = fun piece -> let i = Random.int 3 in match i with
      |0 -> "Dommage vous avez perdu ! " ^ "\n" ^(phrase_gain piece)^"\n"
      |1 -> "Malheureusement vous êtes le perdant de cette partie ! " ^ "\n" ^(phrase_gain piece)^"\n"
      |_ -> "C'est une défaite pour vous ! Retentez votre chance ! " ^ "\n" ^(phrase_gain piece)^"\n"

(** Fonction qui genere une phrase d'égalité aleatoire
@return le string de la phrase *)  
let phrase_egalite = fun () -> let i = Random.int 3 in match i with
    |0 -> "Il n'y a pas de vainqueur ! Vous êtes remboursé."
    |1 -> "C'est une égalité ! On vous rends votre mise. "
    |_ -> "Personne n'as gagné cette partie ! Vous récuperez votre mise."


(** Fonction permettant d'avoir la demande de coup de l'utilisateur
@return la demande*)
let affiche_demande_coup_morpion : unit -> string = fun () ->"\nOu voulez vous placer votre jeton ? Entrez la lettre correspondante a la colonne et le numéro de ligne\n "

(** Fonction demandant à l'utilisateur ou il souhaite placer son pion
@return le choix de l'utilisateur*)
let rec demander_coup_morpion : unit -> string = fun () -> let () = print_string "\n<?>" in
  let reponse = read_line() in
    if reponse = "A1" || reponse = "a1" || reponse = "A2" ||reponse = "a2" || reponse = "A3" ||reponse = "a3" || reponse = "B1" ||reponse = "b1" || reponse = "B2" ||reponse = "b2" || reponse = "B3" ||reponse = "b3" || reponse = "C1" ||reponse = "c1" || reponse = "C2" ||reponse = "c2" || reponse = "C3" || reponse = "c3"  
      then reponse 
      else let () = print_string "\n Votre coup est invalide ! \n" in demander_coup_morpion()


(*-------------------------------------------------------JOUER DE---------------------------------------------------------*)

(** Fonction permmettant de jouer une partie de dé
@param perso : le personnage jouant au jeu
@param mise : le nombre de pieces que le joueur souhaite miser
@return le personnage avec le montant de pieces modifié*)
let jouer_de : Personnage.Personnage.personnage -> int -> Personnage.Personnage.personnage = fun perso mise -> 
  let i = (Random.int 6)+1 in
  match i with
  | 6 -> let () = Affichage.Affichage.aff("\nLe dé roule et s'arrête sur 6 ! ") in let () = Affichage.Affichage.aff(phrase_victoire (5*mise)) in Personnage.Personnage.modif_sac_perso perso Piece (5*mise)
  | n -> let () = Affichage.Affichage.aff("\nLe dé roule et s'arrête sur "^string_of_int n^" ! ") in let () = Affichage.Affichage.aff(phrase_defaite (-(mise))) in Personnage.Personnage.modif_sac_perso perso Piece (-(mise))

(*------------------------------------------------------JOUER NIM----------------------------------------------------------*)

type joueur = J1 | Ordi 

  (** Fonction permettant de générer un nombre de batons entre 25 et 34
  @return le nombre de batons*)
  let generer_tas : unit -> int = fun () ->
      let lenb = Random.int 10 in 25+lenb

  (** Fonction permmettant de générer un coup aléatoire pour l'adversaire
  @param tas : le tas de la partie en cours
  @return le nombre de batons a retirer*)    
  let rec adv_coup_nim : int -> int = fun tas ->
      let pioche = (Random.int 3)+1 in
      if pioche > tas then adv_coup_nim tas
      else pioche

  (** Fonction qui teste si la partie est terminée
  @param tas : le tas de la partie en cours
  @return true si la partie est terminée false sinon*)
  let partie_terminee_nim : int -> bool = fun tas ->
      if tas<=0 then true
      else false
  
  (** Fonction permettant de changer de joueur*)
  let changer_joueur : joueur -> joueur = fun j ->
    match j with
    | J1 -> Ordi
    | Ordi -> J1

(** Fonction permettant de jouer une partie de jeu de nim
@param mise le montant que le joueur a decidé de miser*)      
  let jouer_nim : Personnage.Personnage.personnage -> int -> Personnage.Personnage.personnage = fun perso mise ->
      let tas = generer_tas () in
      let () = Affichage.Affichage.aff("\n"^ligne_de_batons tas) in
      let rec aux : Personnage.Personnage.personnage -> int -> joueur -> int -> Personnage.Personnage.personnage = fun perso tas joueur mise ->
          let j = changer_joueur joueur in 
          if j = J1 then let () = Affichage.Affichage.aff(afficher_demander_qte_baton()) in
               let qte = demander_baton tas in
               let () = Affichage.Affichage.aff("\n"^ligne_de_batons tas) in 
               let nvtas= tas-qte in
               if (partie_terminee_nim nvtas) then let () = Affichage.Affichage.aff(phrase_victoire (3*mise)) in Personnage.Personnage.modif_sac_perso perso Piece (3*mise)
               else aux perso nvtas j mise
          else let qte = adv_coup_nim tas in
              let () = print_string ("\nLe forain a retiré "^(string_of_int qte)^" batons.\n") in
               let nvtas= tas-qte in
               if (partie_terminee_nim nvtas) then let () = Affichage.Affichage.aff(phrase_defaite (-mise)) in Personnage.Personnage.modif_sac_perso perso Piece (-mise)
               else aux perso nvtas j mise
      in aux perso tas Ordi mise

(*-----------------------------------------------------------MORPION-----------------------------------------------------------------*)

(* Les types *)
type pion = Croix | Rond | Vide
type grille = G of pion*pion*pion * pion*pion*pion * pion*pion*pion
type coup = A1 | B1 | C1 | A2 | B2 | C2 | A3 | B3 | C3

(** Fonction pour passer du type pion au type char 
@param pion le pion que l'on veut convertir en char
@return le char correspondant au pion*)
let pion_to_char : pion -> string = function
      | Croix -> "X"
      | Rond -> "0"
      | Vide -> " "

(** Fonction permettant d'afficher la grille 
@return la grille de la partie en cours*)
let afficheGrille : grille -> string = fun (G (a1, b1, c1, a2, b2, c2, a3, b3, c3)) ->
  "\n     A   B   C\n   +---+---+---+\n 1 | "^(pion_to_char a1)^" | "^(pion_to_char b1)^" | "^(pion_to_char c1)^" |\n   +---+---+---+\n 2 | "^(pion_to_char a2)^" | "^(pion_to_char b2)^" | "^(pion_to_char c2)^" |\n   +---+---+---+\n 3 | "^(pion_to_char a3)^" | "^(pion_to_char b3)^" | "^(pion_to_char c3)^" |\n   +---+---+---+\n" 
      
(** Fonction qui convertit un string en coup
@param reponse : le coup sous forme de string donnée par l'utilisateur
@return un coup*)   
let string_to_coup : string -> coup = fun reponse ->
  if reponse = "A1" || reponse = "a1" then A1
  else if reponse = "B1" || reponse = "b1" then B1
  else if reponse = "C1" || reponse = "c1" then C1
  else if reponse = "A2" || reponse = "a2" then A2
  else if reponse = "B2" || reponse = "b2" then B2
  else if reponse = "C2" || reponse = "c2" then C2
  else if reponse = "A3" || reponse = "a3" then A3
  else if reponse = "B3" || reponse = "b3" then B3
  else C3;;

(** Fonctions pour changer un pion de la grille 
@param grille la grille de la partie en cours
@param pion la position du pion dans la grille qu'on souhaite modfifier*)
let setA1 : grille ->pion -> grille = fun (G (_, b1, c1, a2, b2, c2, a3, b3, c3)) -> function
    | Croix -> G (Croix, b1, c1, a2, b2, c2, a3, b3, c3)
    | Rond -> G (Rond, b1, c1, a2, b2, c2, a3, b3, c3)
    | Vide -> G (Vide, b1, c1, a2, b2, c2, a3, b3, c3) 

let setB1 : grille ->pion -> grille = fun (G (a1, _, c1, a2, b2, c2, a3, b3, c3)) -> function
      |Croix -> G (a1, Croix, c1, a2, b2, c2, a3, b3, c3)
      | Rond -> G (a1, Rond, c1, a2, b2, c2, a3, b3, c3)
      | Vide -> G(a1, Vide, c1, a2, b2, c2, a3, b3, c3)
    
let setC1 : grille ->pion -> grille = fun (G (a1, b1, _, a2, b2, c2, a3, b3, c3)) -> function
    |Croix -> G (a1, b1, Croix, a2, b2, c2, a3, b3, c3)
    | Rond -> G (a1, b1, Rond, a2, b2, c2, a3, b3, c3)
    | Vide -> G(a1, b1, Vide, a2, b2, c2, a3, b3, c3)

let setA2 : grille ->pion -> grille = fun (G (a1, b1, c1, _, b2, c2, a3, b3, c3)) -> function
    Croix -> G (a1, b1, c1, Croix, b2, c2, a3, b3, c3)
    | Rond -> G (a1, b1, c1, Rond, b2, c2, a3, b3, c3)
    | Vide -> G(a1, b1, c1, Vide, b2, c2, a3, b3, c3)

let setB2 : grille ->pion -> grille = fun (G (a1, b1, c1, a2, _, c2, a3, b3, c3)) -> function
    |Croix -> G (a1, b1, c1, a2, Croix, c2, a3, b3, c3)
    | Rond -> G (a1, b1, c1, a2, Rond, c2, a3, b3, c3)
    | Vide -> G(a1, b1, c1, a2, Vide, c2, a3, b3, c3)

let setC2 : grille ->pion -> grille = fun (G (a1, b1, c1, a2, b2, _, a3, b3, c3)) -> function
    |Croix -> G (a1, b1, c1, a2, b2, Croix, a3, b3, c3)
    | Rond -> G (a1, b1, c1, a2, b2, Rond, a3, b3, c3)
    | Vide -> G(a1, b1, c1, a2, b2, Vide, a3, b3, c3)
  
let setA3 : grille ->pion -> grille = fun (G (a1, b1, c1, a2, b2, c2, _, b3, c3)) -> function
    |Croix -> G (a1, b1, c1, a2, b2, c2, Croix, b3, c3)
    | Rond -> G (a1, b1, c1, a2, b2, c2, Rond, b3, c3)
    | Vide -> G(a1, b1, c1, a2, b2, c2, Vide, b3, c3)
  
let setB3 : grille ->pion -> grille = fun (G (a1, b1, c1, a2, b2, c2, a3, _, c3)) -> function
    |Croix -> G (a1, b1, c1, a2, b2, c2, a3, Croix, c3)
    | Rond -> G (a1, b1, c1, a2, b2, c2, a3, Rond, c3)
    | Vide -> G(a1, b1, c1, a2, b2, c2, a3, Vide, c3)
  
let setC3 : grille ->pion-> grille = fun (G (a1, b1, c1, a2, b2, c2, a3, b3, _)) -> function
    |Croix -> G (a1, b1, c1, a2, b2, c2, a3, b3, Croix)
    | Rond -> G (a1, b1, c1, a2, b2, c2, a3, b3, Rond)
    | Vide -> G(a1, b1, c1, a2, b2, c2, a3, b3, Vide)

(** Fonction qui initialise la grille 
@return une grille vide*)    
let init : unit -> grille = fun () -> G (Vide, Vide, Vide, Vide, Vide, Vide, Vide, Vide, Vide)

(** Fonction qui fait correspondre un nombre selon un pion
@param pion le type de pion qui permet de gagner la partie ou non
@return 2 si c'est une Croix, 1 si c'est un rond, 0 sinon*)  
let partie_terminee_morpion = fun pion -> match pion with
  |Croix -> 2
  |Rond -> 1
  |Vide -> 0

(** Fonction qui verifie si un joueur a gagné 
@param grille la grille dans laqulle on souhaite verifier si il y a une victoire
@return 2 si le joueur a gagné, 1 si c'est l'ordinateur, 0 sinon*)  
let gagne : grille -> int = fun (G (a1, b1, c1, a2, b2, c2, a3, b3, c3)) -> 
  if (a1 = a2 && a2 = a3) then partie_terminee_morpion a3   
  else if (b1 = b2 && b2 = b3) then partie_terminee_morpion b3  (** Victoire verticale*)
  else if (c1 = c2 && c2 = c3) then partie_terminee_morpion c3 
  
  else if (a1 = b1 && b1 = c1) then partie_terminee_morpion c1
  else if (a2 = b2 && b2 = c2) then partie_terminee_morpion c2  (** Victoire horizontale*)
  else if (a3 = b3 && b3 = c3) then partie_terminee_morpion c3

  else if (a1 = b2 && b2 = c3) then partie_terminee_morpion c3  (** Victoire diagonales*)
  else if (a3 = b2 && b2 = c1) then partie_terminee_morpion c1
  else 0


(** Fontion qui permet au joueur de jouer
@param grille : la grille de la partie en cours
@param coup : la case dans lequel le joueur veut mettre son pion
@return la : grille modifiée*)      
let rec joueur : grille -> coup -> int*grille = fun (G (a1, b1, c1, a2, b2, c2, a3, b3, c3)) -> function
    | A1 when a1 = Vide -> let g = setA1 (G (a1, b1, c1, a2, b2, c2, a3, b3, c3)) Croix in Affichage.Affichage.aff(afficheGrille g); (gagne g, g)  
    | B1 when b1 = Vide -> let g = setB1 (G (a1, b1, c1, a2, b2, c2, a3, b3, c3)) Croix in Affichage.Affichage.aff(afficheGrille g); (gagne g, g)   
    | C1 when c1 = Vide -> let g = setC1 (G (a1, b1, c1, a2, b2, c2, a3, b3, c3)) Croix in Affichage.Affichage.aff(afficheGrille g); (gagne g, g)   
    | A2 when a2 = Vide -> let g = setA2 (G (a1, b1, c1, a2, b2, c2, a3, b3, c3)) Croix in Affichage.Affichage.aff(afficheGrille g); (gagne g, g)   
    | B2 when b2 = Vide -> let g = setB2 (G (a1, b1, c1, a2, b2, c2, a3, b3, c3)) Croix in Affichage.Affichage.aff(afficheGrille g); (gagne g, g)   
    | C2 when c2 = Vide -> let g = setC2 (G (a1, b1, c1, a2, b2, c2, a3, b3, c3)) Croix in Affichage.Affichage.aff(afficheGrille g); (gagne g, g)   
    | A3 when a3 = Vide -> let g = setA3 (G (a1, b1, c1, a2, b2, c2, a3, b3, c3)) Croix in Affichage.Affichage.aff(afficheGrille g);(gagne g, g)   
    | B3 when b3 = Vide -> let g = setB3 (G (a1, b1, c1, a2, b2, c2, a3, b3, c3)) Croix in Affichage.Affichage.aff(afficheGrille g);(gagne g, g)   
    | C3 when c3 = Vide -> let g = setC3 (G (a1, b1, c1, a2, b2, c2, a3, b3, c3)) Croix in Affichage.Affichage.aff(afficheGrille g);(gagne g, g)   
    | _ -> let () = Affichage.Affichage.aff("\nCette case n'est pas vide, choisissez une autre case\n") in joueur (G (a1, b1, c1, a2, b2, c2, a3, b3, c3)) (string_to_coup (demander_coup_morpion ())) 

(** Fontion qui donne un coup aléatoire pour l'adversaire
@return un coup*)
let coup_random = fun () -> let i = Random.int 9 in match i with
  |0 -> A1
  |1 -> A2 
  |2 -> A3
  |3 -> B1
  |4 -> B2
  |5 -> B3
  |6 -> C1
  |7 -> C2
  |_ -> C3



(** Fontion qui fait jouer l'ordinateur
@param grille : la grille de la partie en cours
@param coup : la case dans lequel l'adversaire place son pion son pion
@return la grille modifiée*)
  let rec ordi : grille -> coup -> int*grille = fun (G (a1, b1, c1, a2, b2, c2, a3, b3, c3)) -> function
| A1 when a1 = Vide -> let g = setA1 (G (a1, b1, c1, a2, b2, c2, a3, b3, c3)) Rond in Affichage.Affichage.aff(afficheGrille g); (gagne g, g)
| B1 when b1 = Vide -> let g = setB1 (G (a1, b1, c1, a2, b2, c2, a3, b3, c3)) Rond in Affichage.Affichage.aff(afficheGrille g); (gagne g, g)
| C1 when c1 = Vide -> let g = setC1 (G (a1, b1, c1, a2, b2, c2, a3, b3, c3)) Rond in Affichage.Affichage.aff(afficheGrille g); (gagne g, g)
| A2 when a2 = Vide -> let g = setA2 (G (a1, b1, c1, a2, b2, c2, a3, b3, c3)) Rond in Affichage.Affichage.aff(afficheGrille g); (gagne g, g)
| B2 when b2 = Vide -> let g = setB2 (G (a1, b1, c1, a2, b2, c2, a3, b3, c3)) Rond in Affichage.Affichage.aff(afficheGrille g); (gagne g, g)
| C2 when c2 = Vide -> let g = setC2 (G (a1, b1, c1, a2, b2, c2, a3, b3, c3)) Rond in Affichage.Affichage.aff(afficheGrille g); (gagne g, g)
| A3 when a3 = Vide -> let g = setA3 (G (a1, b1, c1, a2, b2, c2, a3, b3, c3)) Rond in Affichage.Affichage.aff(afficheGrille g); (gagne g, g)
| B3 when b3 = Vide -> let g = setB3 (G (a1, b1, c1, a2, b2, c2, a3, b3, c3)) Rond in Affichage.Affichage.aff(afficheGrille g); (gagne g, g)   
| C3 when c3= Vide -> let g = setC3 (G (a1, b1, c1, a2, b2, c2, a3, b3, c3)) Rond in Affichage.Affichage.aff(afficheGrille g); (gagne g, g)
| _ -> ordi (G (a1, b1, c1, a2, b2, c2, a3, b3, c3)) (coup_random ())

(** Fonction permettant de savoir si la grille est remplie
@param grille : la grille de la partie en cours
@return true si la grille est remplie, false sinon*)
let grille_remplie : grille -> bool = fun (G (a1, b1, c1, a2, b2, c2, a3, b3, c3)) ->
  if (a1<>Vide)&&(a2<>Vide)&&(a3<>Vide)&&(b1<>Vide)&&(b2<>Vide)&&(b3<>Vide)&&(c1<>Vide)&&(c2<>Vide)&&(c3<>Vide) then true
  else false

(** Fonction permettant d'initiliaser la grille à vide et de l'afficher en debut de partie
@return la grille vide*)
let morpion = fun g ->
  let g = init() in let () = Affichage.Affichage.aff(afficheGrille g) in g

  (** Fonction permettant de jouer un coup
  @param grille : la grille avec laquel on joue 
  @param j : le joueur jouant
  @param coup : le coup joué*)
let jouer = fun grille j coup -> match j with
  | J1 -> (joueur grille coup)
  | Ordi -> (ordi grille coup) ;;
  
(** Fonction permettant de jouer une partie de morpion
@param mise : le montant que le joueur a decidé de miser
@param perso : le personnage jouant au jeu
@return le personnage avec son sac modifié*)      
let jouer_morpion : Personnage.Personnage.personnage -> int -> Personnage.Personnage.personnage = fun perso mise ->
  let lagrille = morpion () in
  let rec aux : Personnage.Personnage.personnage -> grille -> joueur -> int -> Personnage.Personnage.personnage = fun perso lagrille j mise ->
      let remplie = grille_remplie lagrille in
      if (remplie=false) then
        let j = changer_joueur j in
        if j = J1 then let () = Affichage.Affichage.aff(affiche_demande_coup_morpion()) in
        let str_coup = demander_coup_morpion () in
            let () = Affichage.Affichage.aff(str_coup) in
            let coup = string_to_coup str_coup in
            let res = joueur lagrille coup in
            let nvgrille = snd(res) in
            let fini = fst(res) in
            if (fini=2) then let () = Affichage.Affichage.aff(phrase_victoire (mise)) in Personnage.Personnage.modif_sac_perso perso Piece (mise)
            else (if (fini=1) then let () = Affichage.Affichage.aff(phrase_defaite (-mise)) in Personnage.Personnage.modif_sac_perso perso Piece (-mise)
                  else aux perso nvgrille j mise)
        else let coup = coup_random () in 
            let res= ordi lagrille coup in
            let nvgrille = snd(res) in
            let fini = fst(res) in
            if (fini=2) then let () = Affichage.Affichage.aff(phrase_victoire (mise)) in Personnage.Personnage.modif_sac_perso perso Piece (mise)
            else (if (fini=1) then let () = Affichage.Affichage.aff(phrase_defaite (-mise)) in Personnage.Personnage.modif_sac_perso perso Piece (-mise)
                  else aux perso nvgrille j mise)
      else let () = Affichage.Affichage.aff(phrase_egalite ()) in perso
  in aux perso lagrille Ordi mise

  
  end
;;