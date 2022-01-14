(* Consigne générale : remplacer les
              raise (TODO "...")
   par votre code. Ne pas changer le nom des fonctions.

  Pré et post-condition : ce fichier compile sans erreur

   Il y a 3 types de questions (dans l'ordre d'apparition) :
   - des questions [**] que vous devriez réussir si vous avez fait et compris le projet sans les extensions
   - des questions [***] qui concernent les extensions facultatives
   - des questions [*] que vous n'avez pas besoin de traiter si vous avec réussi au moins 2
     des questions mieux étoilées

   Il n'est pas nécessaire de traiter toutes les questions à [**] ou [***] pour obtenir
   une note satisfaisante, ni même la note maximale !    
   *)

let prenom: string = "Brice";;
let nom: string = "MONTHE";;
    
exception TODO of string;;
exception PRESQUE of string;;
(*  Vous  pouvez  utilisez  l'exception PRESQUE  si  vous  avez  un
   programme qui  marche presque.  Si  une telle exception  est levée,
   nous irons lire votre code pour voir s'il mérite quelques points.

    ex  :
    let incr x =
       (* x+2  *)
      raise (PRESQUE "mon incr incrémente, mais de 2 au lieu de 1, et je ne trouve pas mon erreur")
    *) 

(***********************************************************************************)
(* Questions projets

   Voici 2 programmes écrit dans le langage loop, pour lequel on peut
   - manipuler 2 variables ("a" et "b")
   - exécuter des instructions en séquence (";")
   - incrémenter une des 2 variables ("I a")
   - remettre la valeur d'une variable à 0 ("R a")
   - boucler entre 0 et 9 fois "L 3 ( ... )" 

   Voici 2 exemples de  programmes écrits dans ce langage
   *)
      
let prog1 = "R a;
R b;
I a; I a;
L 4 (
  I a;
  R a;
  I b;
  L 5 ( I a )
)
";;
let prog2 = "R a;
R b;
I a; I a;
L 9 (
  I a;
  R a;
  I b;
  L 5 ( I a )
)
";;

(*  [**]  Écrivez  une   grammaire  reconnaissant  les  2  programmes
   ci-dessus. Faites le dans  la chaîne (string) grammaire ci-dessous
   *)

let grammaire:string = "
 grammaire récursive à gauche
 
 V::= a | b 
 C::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 
 I::= I;I | L C (I;I) | R V | I' V | Epsilon
 
 grammaire non récursive à gauche
 V::= a | b 
 C::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 
 S::= IL | Epsilon
 L::= ;S | Epsilon
 I::= L C (S) | R V | I' V 
 
 on suppose que l'espace est négligé.
"

(*  Voici un  analyseur  lexical  pour notre  langage.  Notez que  ce
   programme enlève les blancs et les retours à la ligne *)
let lexer : string -> char list = fun s ->
  let blank = function
    | ' ' | '\n' | '\t' -> true
    | _ -> false
  in
  let n = String.length s in
  let rec boucle i =
    if i = n then [] else if blank s.[i] then boucle (i+1) else s.[i] :: boucle (i+1)
  in boucle 0;;
    
(* En utilisant le fichier anacomb.ml qui vous est fourni... *)

#use "anacomb.ml";;

(*  ...    écrivez  un  analyseur  syntaxique   (p_P)  permettant  de
   reconnaître  les programmes  définis  par votre  grammaire, et  de
   construire  un arbre  syntaxique dont  le type  en ocaml  vous est
   fourni ci-dessous *)

type var = char
type linstr =
  | Skip
  | Incr of var
  | Reset of var
  | Seq of linstr * linstr
  | Loop of int * linstr 

let chiffre : (int, char) ranalist =
  let valchiffre : char -> int option = fun c ->
    match c with
    | '0' .. '9' -> Some (Char.code c - Char.code '0')
    |_ -> None
  in terminal_res valchiffre;;

let lettre : (char, char) ranalist =
  let char_lettre : char -> char option = fun c ->
    match c with
    | 'a' .. 'z' -> Some c
    |_ -> None
  in terminal_res char_lettre;;
              
let rec p_P : (linstr, char) ranalist = fun l -> l |>
	( p_I ++> fun i1 -> p_Li ++> fun i2 -> epsilon_res (Seq(i1, i2) ) ) +|
	( epsilon_res (Skip) )
and 
		p_Li : (linstr, char) ranalist = fun l -> l |>
	( terminal ';' -+> p_P ++> fun i -> epsilon_res (Seq(Skip,i))) +|
	( epsilon_res (Skip) )
and 
		p_I : (linstr, char) ranalist = fun l -> l |>
	( terminal 'L' -+> chiffre ++>  fun val_loop -> terminal '(' -+> p_P  ++> fun i1 -> terminal ')' -+> epsilon_res (Loop(val_loop, i1 ))) +|
	( terminal 'R' -+> lettre ++> fun variable -> epsilon_res (Reset(variable))) +|
	( terminal 'I' -+> lettre ++> fun variable -> epsilon_res (Incr(variable)));;
(* rappel : les boucles utilisent des entiers entre 0 et 9 (pas besoin de Horner) *)
     
let test s = p_P (lexer s);;

let _ = test prog1;;
let _ = test prog2;;