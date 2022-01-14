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
 I::= I;I | L.C.(I;I) | R.V | I'.V | Epsilon
 
 grammaire non récursive à gauche
 V::= a | b 
 C::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 
 S::= IL | Epsilon
 L::= ;S | Epsilon
 I::= L.C.(S;S) | R.V | I'.V 
 
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

let x =(list_of_string "3onthe" );;

let test_1 = fun l -> l |> chiffre ++> fun b -> epsilon_res b ;;

let _ = test_1 x;;
              
let rec p_P : (linstr, char) ranalist =
	let p_P1 : (linstr, char) ranalist = fun l ->
		let (info1, i1) = p_I l in
		let (info2, i2) = p_Li i1 in ( Seq(info1, info2), i2 )
	and 
		p_P2: (linstr, char) ranalist = fun l -> ( Skip, l)
	in fun l -> try p_P1 l with Echec -> p_P2 l
and 
		p_Li : (linstr, char) ranalist = 
			let p_Li1 : (linstr,char) ranalist = fun l -> 
				let i1 = terminal ';' l in 
				let (info2, i2) = p_P i1 in ( Seq(Skip,info2), i2 )
			and 
				p_Li2 : (linstr,char) ranalist = fun l -> ( Skip, l)
			in fun l -> try p_Li1 l with Echec -> p_Li2 l
and 
		p_I : (linstr, char) ranalist =
			let p_I1 : (linstr,char) ranalist = fun l -> 
				let i1 = terminal 'L' l in
				let (val_loop, i2) = chiffre i1 in 
				let i3 = terminal '(' i2 in 
				let (info4, i4) = p_P i3 in
				let i5 = terminal ')' i4 in (Loop(val_loop, info4 ), i5 )
			and 
				p_I2: (linstr,char) ranalist = fun l ->
					let i1 = terminal 'R' l in
					let (variable, i2) = lettre i1 in (Reset(variable),i2)
			and 
				p_I3: (linstr,char) ranalist = fun l ->
					let i1 = terminal 'I' l in 
					let (variable, i2) = lettre i1 in (Incr(variable),i2)
			in fun l -> try p_I1 l with Echec -> try p_I2 l with Echec -> p_I3 l;;
(* rappel : les boucles utilisent des entiers entre 0 et 9 (pas besoin de Horner) *)
     
let test s = p_P (lexer s);;

let _ = test prog1;;
let _ = test prog2;;