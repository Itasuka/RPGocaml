
# <center>PARTAGE DU TRAVAIL

  

  

__Noémie__ :

  

  

Dans la partie sans Extension, j'ai réalisé la fonction permettant de faire perdre un objet au personnage mais aussi la fonction dormir. Je me suis également occupée de tout le Module affichage, c'est à dire l'affichage de l'état du personnage et de toutes les fonctions d'écriture de l'histoire en cours, en dehors des fonctions de demande à l'utilisateur. J'ai décidé de retirer les accents de l'affichage de l'état du personnage car j'ai rencontré des problèmes avec ceux : ils comptaient comme 2 caractères ce qui m'empêchait de faire un affichage propre. J'ai aussi rédigé l'intégralité du mode d'emploi du jeu de base.

  

Dans la partie Extensions, je me suis occupée de l'affichage de l'Auberge et les fonctions de demande liées au village. J'ai aussi réalisé toute la partie Marchand, c'est à dire : le module Marchand, la création de fonctions et types nécessaires à celui-ci dans le module Objet et aussi sa partie affichage. J'ai également effectué toute la réadaptation nécessaire de l'affichage du jeu de base pour que celle-ci soit compatible avec les extensions (armes, potions, réadaptation de l’affichage du personnage etc). J'ai aussi rédigé le mode d'emploi de la partie extension en dehors de la partie "Armes" et "Foire".

  
  
  

  

__Nicolas M__ :

  

Dans la partie sans Extension, j'ai créés les types pour le personnage et les monstres. J'ai aussi fait la fonction qui associe a chaque type de monstre les dégâts que celui ci inflige ainsi qu'une fonction pour afficher le nom du monstre en string. Enfin j'ai crées les fonctions de demande concernant la creation du personnage (nom, genre et classe) et une fonction qui crées un sac pour le personnage.

J'ai effectuée les OcamlDoc de chacunes des fonctions que j'ai créées.

  

Dans la partie Extensions, J'ai réalisé toute la partie Foire, c'est à dire : le module Foire, la création de fonctions et type nécessaires aux différent jeux et la partie affichage de chacun de ses jeux d'argent. Je me suis également occupée du module marabout et des modifications du type objet dans le module Objet pour rajouter les potions vendus par le marabout et aussi les fonctions affichage concernant le marabout.

  

__Nicolas S__ :

Dans la partie sans extensions, je me suis occupé :
* Des fonctions de mofications du personnage (gain de l'expérience, modifications des points de vie et du sac du personnage)
* La fonction manger
* De la fonction calculant les dégats du personnage en prenant en compte ses chances de toucher selon la classe
* Des fonctions qui permettent de créer un monstre aléatoire avec des points de vie et un objet aléatoire (si il en possède un)
* Des fonctions de demandes d'actions
* Des fonction du module jeu (à l'exception de dormir) qui permettent le déroulement de la partie.

Je me suis aussi occupé de la correction des erreurs dans sa globalité ainsi que des ajustements sur l'affichage lors des tests.

Dans la partie avec extensions, je me suis occupé de l'entièreté de la partie équipements/statistiques (à l'exception de l'ajustement de l'affichage prenant en compte les armes), c'est à dire :
* Création des types pour définir 5 armes pour chaques classe, recherche de noms pour les armes afin s'inspirer de la mythologie, de films ou de jeux vidéo marquant
* Modification du personnage pour qu'il possède des statistiques, une arme équipée ainsi que des stats bonus octroyées par les potions
* Modification du sac afin d'initialiser les armes et les potions dedans
* Création d'une fonction permettant de générer une arme aléatoire obtenable auprès du marchand selon la classe et le sac du personnage
* Création de fonctions permettant de modifier ou réinitialiser les stats octroyées par les potions 

Modifications du module Jeu pour prendre en compte les diverses extensions.
Correction des erreurs du jeu dans sa globalité.
  

  

# <center>MODE D'EMPLOI DU JEU

  

  

  
  

  

  
  
  

  
  

  

  

## <center>☆━━━━━━━━━BUT DU JEU━━━━━━━━━☆

  

  

  

Atteindre le niveau 10 avant d'être tué par un monstre.

  

  

  

## <center>☆━━━━━━━━━━━LE JEU━━━━━━━━━━━☆

  

  

  

___Attention !___ Vous entrez dans un monde fantastique rempli de monstres plus terrifiants les uns que les autres...

  

  

  

__☆ Les classes__

  

  

Au début du jeu, vous aurez initialement 20 points de vie et pourrez choisir d'incarner la ___classe___ de votre choix entre ___Archer___, ___Guerrier___ ou ___Magicien___.

  

  

Cependant, chacune possède ses forces et ses faiblesses... L'Archer, très adroit, aura 70% de chance de toucher un ennemi mais n'infligera seulement que 4 points de dégats. Le Guerrier, bien plus puissant, fera perdre 10 points à l'adversaire, mais au vu de son agilité aura seulement 30% de chance de le toucher. Le magicien, misant à la fois force et ingéniosité, ôtera 5 point de vies au monstre et aura 50% de chance que son coup réussisse.

  

  

  

![Personnage de classe Magicien](https://p4.wallpaperbetter.com/wallpaper/498/245/298/fantasy-art-magic-sorcerer-wallpaper-preview.jpg)

  

  

<center> Personnage de classe Magicien

  

  

  

![Personnage de classe Archer](https://p4.wallpaperbetter.com/wallpaper/365/243/671/fantasy-art-elves-warrior-archer-wallpaper-preview.jpg)

  

  

<center> Personnage de classe Archer

  

  

  

![enter image description here](https://cdn.discordapp.com/attachments/938846595812818964/960568260020015174/unknown.png)

  

  

<center> Personnage de classe Guerrier

  

  

  

__☆ Le sac__

  

  

Votre personnage possèdera un ___sac___ tout au long du jeu, qui sera vide initialement, et dont le contenu variera en fonction de vos choix au long de l'aventure : lors d'une fuite face à un monstre, vous pourrez égarer certains objets et à l'inverse, en combattant et vainquant un monstre, vous pourrez gagner ce que celui-ci portait sur lui sauf si ce dernier était une nuée de moustiques car elle ne possède pas d'objets.

  

  

Les différents objets obtenables sont : des ___Pieces___, des ___Eponges___ et des ___Poulets___. A chaque Poulet mangé, vous gagnerez 2 points de vie.

  

  

Vous pouvez également gagner 4 points de vie en dormant, cependant vous n'êtes pas à l'abri qu'un monstre vienne vous tuer durant votre sommeil...

  

  

Vous pourrez visualiser le niveau, les points de vie, l'expérience et le contenu du sac de votre personnage en choisisant "Visualiser l'état de votre personnage".

  

  

  

__☆ Les monstres__

  

  

Face à vous, différents ___monstres___ pourront se présenter : des ___Sanglier___, des ___Golem___ et plus surprenant encore des ___Nuées de Moustiques___ au nombre variable !

  

  

Chacun de ces montres possède un certains nombre de points de vie et une force d'attaque propre à lui. Un Sanglier vous infligera 4 de dégat et possèdera entre 26 et 31 points de vie. Un Golem vous enlevera 2 points de vie et, de son côté, en possèdera entre 11 à 14. Enfin, chaque moustisque d'une Nuée de Moustiques vous fera 1/2 points de dégats.

  

  

Vous pourrez décider de si vous souhaitez combattre ou fuir un monstre, cependant il se peut que vous tombiez fortuitement face à un nouveau monstre et que celui-ci vous attaque sans que vous ayez votre mot à dire.

  

  

A chaque fois que vous vainquerez un monstre, celui-ci vous rapportera des points d'expériences : 8 pour un Golem, 4 pour un Sanglier et 2 pour une Nuee de Moustique. Vous remporterez également l'objet que portait ce monstre, cependant, il se peut que les Nuees de Moustiques n'en possède pas.

  

  

  

![Les Moustiques](https://cdn.discordapp.com/attachments/938846595812818964/960566305365315614/unknown.png)

  

  

<center> Des Moustiques à la recherche de sang

  

  

  

![enter image description here](https://images-wixmp-ed30a86b8c4ca887773594c2.wixmp.com/f/e117094b-c226-4191-bd74-8ec24c208ccc/d2pjakv-da2855ec-c6b7-412a-b56a-dee2985e3603.jpg?token=eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJ1cm46YXBwOjdlMGQxODg5ODIyNjQzNzNhNWYwZDQxNWVhMGQyNmUwIiwiaXNzIjoidXJuOmFwcDo3ZTBkMTg4OTgyMjY0MzczYTVmMGQ0MTVlYTBkMjZlMCIsIm9iaiI6W1t7InBhdGgiOiJcL2ZcL2UxMTcwOTRiLWMyMjYtNDE5MS1iZDc0LThlYzI0YzIwOGNjY1wvZDJwamFrdi1kYTI4NTVlYy1jNmI3LTQxMmEtYjU2YS1kZWUyOTg1ZTM2MDMuanBnIn1dXSwiYXVkIjpbInVybjpzZXJ2aWNlOmZpbGUuZG93bmxvYWQiXX0.ByPTA9YMXOs5Bj19DsguuIeSIXrgv93GroiNPrP-KdI)

  

  

<center> Un Golem en furie

  

  

  

![enter image description here](https://i.pinimg.com/originals/e8/eb/d4/e8ebd40077186c4fa26da90c8d2b286b.jpg)

  

  

<center> Un sanglier sauvage

  

  

  

Faites attention, ___aucune sauvegarde___ ne serra effectuée si vous décidez de quitter le jeu !

  

  

  

<center>A VOUS DE JOUER !

  

  

  

## <center>☆━━━━━━━━━━COMMENT JOUER ━━━━━━━━━━☆

  

  

  

Tout au long du jeu, celui-ci vous demandera d'entrer vos choix via des lettres.

  

  

Par exemple :

  

  

* Que voulez-vous faire ?

  

  

(C\) Continuer votre chemin

  

  

(D) Dormir

  

  

(M) Manger

  

  

(V) Visualiser l’ ́etat de votre personnage

  

  

(Q) Quitter l’aventure

  

  

\<?>

  

  

  

Au niveau du <?>

  

  

→ En entrant C ou c vous continuerez votre chemin à la rencontre d'autres monstres.

  

  

→ En entrant D ou d vous dormirez pour récupérer 4 points de vie ou vous mourirez malencontreusement.

  

  

→ En entrant M ou m vous mangerez un de vos délicieux poulets et gagnerez 2 points de vie.

  

  

→ En entrant V ou v vous visualiserez l'état complet de votre personnage.

  

  

→ En entrant Q ou q vous quitterez l'aventure, mais attention ! Aucune sauvegarde n'est disponible !

  

  

  

Sous un autre exemple :

  

  

* Que faites-vous ?

  

  

(A) Attaquer

  

  

(F) Fuir

  

  

(V) Visualiser l’ ́etat de votre personnage

  

  

\<?>

  

  

  

A niveau du <?>

  

  

→ En entrant A ou a vous attaquerez le monstre que vous venez d'appercevoir.

  

  

→ En entrant F ou f vous fuyerez le monstre que vous venez d'appercevoir.

  

  

→ En entrant V ou v vous visualiserez l'état complet de votre personnage.

  

  

  

Vous devez donc entrer votre choix d'action sous la forme d'une lettre, comme ci-dessous : D ; ou encore comme ceci : d.

  

  

  

## <center>☆━━━━━━━━━━L'EXTENSION━━━━━━━━━━☆

  

  

  

Le but du jeu reste le même cependant divers ajouts de contenu ont été effectués.

  

  

  

### ☆ LE VILLAGE

  

  

Lors de l'aventure vous croiserez plusieurs villages où vous pourrez effectuer différentes activités.

  

  

  

* __L'auberge__

  

  

Ici, vous pourrez dormir en sécurité pour la modique somme de 10 Pieces. L'aubergiste vous proposera également des poulets à 4 Pieces l'unité.

  

  

![enter image description here](https://cdn.discordapp.com/attachments/938846595812818964/963088882650460190/unknown.png)

  

  

<center> L'auberge

  

  

  

* __Le marabout__

  

  

En allant voir le marabout du village, vous serez en capacité de lui acheter des potions qui pourront augmenter votre puissance ou votre précision avant un combat si vous décidez de les consommer. Mais faites attention, certaines d'entre elles vous pourront vous faire plus de mal que de bien...

  

  

![enter image description here](https://cdn.discordapp.com/attachments/938846595812818964/963091468149485618/unknown.png)

  

  

<center> Un marabout

  

  

  

* __La foire__

  

  

En vous rendant à la foire du village, on vous proposera de jouer à des divers jeux d'argents : les dés, le jeu de Nim ou encore le morpion. Misez des Pièces : gagnez et remportez le gros lot, échouez et perdez votre mise !

  

  

  

![enter image description here](https://cdn.discordapp.com/attachments/938846595812818964/963090517380452392/unknown.png)

  

  

<center> Les jeux d'argent

  

__Règles des différents jeux:__

  

__Les dés:__

Pour gagner a ce jeu il suffit d'obtenir un six et vous remporter six fois votre mise

  

__Le jeu de Nim:__

Ce jeu se joue avec un tas de bâtons formant une ligne . Chaque joueur, à tour de rôle, prends au maximum 3 bâtons dans le tas. Le gagnant est celui qui prends le dernier bâton.

  

__Le Morpion:__

Le but est de créer le premier un alignement sur une grille pour cela les joueurs inscrivent tour à tour leur symbole sur cette grille . Le premier qui parvient à aligner trois de ses symboles horizontalement, verticalement ou en diagonale gagne la partie. Ce jeu permet de remporter le double de sa mise.

  

### ☆ LES MARCHANDS ITINERANTS

  

Tout du long du jeu, vous aurez la chance de croiser des marchands itinérants sur votre route !

Vous pourrez leur vendre vos biens, tels que des Poulets, des Eponges ou des Potions, pour gagner des pièces et pourrez également acheter en quantité limité ce qu'ils vous proposeront : Poulets, Eponges, et certaines Armes !

Soyez malin : les prix des articles proposés à la vente et au rachat varient d'u
![enter image description here](https://i.pinimg.com/736x/2f/ef/68/2fef68a9c05fc86fd4d97e48c97feb7c--fantasy-characters-character-ideas.jpg)
<center> Un marchand itinérant

### ☆ LES ARMES

Lors de vos péripéties, vous pourrez avoir la chance de croiser un marchand itinérant qui pourra vous vendre différents objets utiles mais aussi diverses armes selon votre classe.

Chaque classe possède :

* 1 arme de base

* 4 armes ayant chacune une spécialité

* 1 arme légendaire obtenable auprès d'un marchand si vous possédez toute les autres armes

Voici toutes les armes disponible dans le jeu :

| Spécialité | Guerrier | Archer | Magicien |
|:--------------|:-------------------------:|:-------------------:|:--------------------:|
| Aucune | Epée en bois |Arc en bois |Baton en bois |
| Points de vie | Epée de Nuada |Arc d'Artémis |Gae Bolga |
| Attaque | Kusanagi et Yata no kagami|Gandiva |Caducée |
| Défense | Aegis |Arc bouclier immortel|Voile d'Ino |
| Précision | Durandal |Zéphyr |Gambanteinn |
| __*Légendaire*__| __*Excalibur*__ |__*Arc de lumière*__ |__*Baguette de sureau*__|

  

![enter image description here](https://cdn.discordapp.com/attachments/938846595812818964/963816461426651176/unknown.png)
<center> Excalibur

![enter image description here](https://cdn.discordapp.com/attachments/938846595812818964/963814730160537600/unknown.png)
<center> L'arc de lumière

![enter image description here](https://cdn.discordapp.com/attachments/938846595812818964/963816775022153729/unknown.png)
<center> La baguette de sureau

Les armes avec aucunes spécialité sont les armes que vous possèderez au début du jeu. Elles ne confèrent aucun avantages.

Les armes augmentent grandement la statistique de leur spécialité et augmentent ou diminuent les autres statistiques.

Il faut donc bien faire attention lorsque l'on choisi son arme !

Les armes légendaire sont des armes qui surclassent toutes les autres en tout points. Cependant, ce ne sera pas aisé de mettre la main dessus car vous devez posséder toutes les autres armes de votre classe avant.

Voici un tableau récapitulatif des statistiques octroyées par les armes selon leur spécialitée :
| Spécialité/Stats | Points de vie | Attaque | Défense | Précision |
|:-----|:----:|:----:|:----:|:----:|
| Aucune | 0 |0|0 |0|
| Points de vie | 20 |2|-2|5|
| Attaque |0|5|-2|5|
| Défense |7|-1|1|-10|
| Précision |0|-1|-1|20|
| __*Légendaire*__|20|5|1|20