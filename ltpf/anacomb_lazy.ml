(* Analyse descendante récursive sur une liste avec des combinateurs *)

module type ListT =
  sig
    type 'a t
    val vide : 'a t
    val cons : 'a -> 'a t -> 'a t
    val hd : 'a t -> 'a   (* may raise Failure "hd" *)
    val tl : 'a t -> 'a t (* may raise Failure "tl" *)
    val map : ('a -> 'a) -> 'a t -> 'a t
    val to_list : 'a t -> 'a list
  end

module Liste: ListT =
struct
  type 'a t = 'a list
  let vide = []
  let cons x l = x::l
  let hd = List.hd
  let tl = List.tl
  let map = List.map
  let to_list l = l
end

type 'a lazylist = unit -> 'a contentsll
and 'a contentsll = Nil | Cons of 'a * 'a lazylist

module LazyListe: ListT =
struct
  type 'a t = 'a lazylist
  let vide = fun () -> Nil
  let cons x l () = Cons(x, l)
  let hd l = match l() with Cons (x,_) -> x | Nil -> failwith "hd"
  let tl l = match l() with Cons (_,l) -> l | Nil -> failwith "tl"
  let rec map f l () =
    match l() with
    | Cons (x,l) -> Cons (f x, map f l)
    | Nil -> Nil
  let rec to_list l =
    match l() with
    | Cons (x,l) -> x:: (to_list l)
    | Nil -> []
end

open Liste

let (char_liste_of_string : string -> 'char Liste.t) = fun s ->
  let rec boucle s i n =
    if i = n then vide else cons s.[i] (boucle s (i+1) n)
  in boucle s 0 (String.length s)

let (char_liste_of_file: string -> 'char Liste.t) = fun fn ->
  let ic = open_in fn in
  let rec loop acc =
    try  loop (cons (input_char ic) acc)
    with End_of_file -> acc
  in
  loop vide

let (string_of_list : ('a -> string) -> 'a Liste.t -> string) =
  fun a2s l ->
  String.concat "" (List.map a2s (to_list l))

let (string_of_char_liste : char Liste.t -> string) = string_of_list Char.escaped

let test str =
  assert(str=string_of_char_liste (char_liste_of_string str))
let _ = test "dfgqsdcsdg sdfgsfg sfg sfg"


(* Utilitaire pour les tests *)

(* Le type des fonctions qui épluchent une liste de terminaux *)
type 'term analist = 'term Liste.t -> 'term Liste.t

exception Echec

(* terminal constant *)
let terminal (c : 't) : 't analist = fun l ->
  try if Liste.hd l = c then Liste.tl l else raise Echec
  with Failure _ -> raise Echec

(* terminal conditionnel *)
let terminal_cond (p : 'term -> bool) : 'term analist = fun l ->
  try if p (Liste.hd l) then Liste.tl l else raise Echec
  with Failure _ -> raise Echec

(* non-terminal vide *)
let epsilon : 'term analist = fun l -> l

(* ------------------------------------------------------------ *)
(* Combinateurs d'analyseurs purs *)
(* ------------------------------------------------------------ *)

(* a suivi de b *)
let (-->) (a : 'term analist) (b : 'term analist) : 'term analist =
  fun l -> let l = a l in b l

(* Choix entre a ou b *)
let (-|) (a : 'term analist) (b : 'term analist) : 'term analist =
  fun l -> try a l with Echec -> b l

(* ---------------------------------- *)
(* Grammaire non récursive *)

(*
    S0  ::=  'x'
    S   ::=  '(' S0 ')'  |  'x'
*)

let p_S0 : char analist = terminal 'x'

let p_S : char analist =
    (terminal '('  -->  p_S0  -->  terminal ')')
 -| (terminal 'x')

(* Tests *)

let echec test s = try (let _ = test s in false) with Echec -> true

let test s = to_list(p_S (char_liste_of_string s))

(* ---------------------------------- *)
(* Grammaire récursive *)

(*
    S   ::=  '(' S ')'  |  'x'
*)


(*
   En OCaml, x |> f est une autre notation de f x.
   Le let rec impose l'explicitation d'au moins un argument,
   d'où le démarrage par fun l -> l |>
*)

let rec p_S : char analist = fun l ->  l |>
     (terminal '('  -->  p_S  -->  terminal ')')
  -| (terminal 'x')

(* Variante avec ε
    S   ::=  '(' S ')'  |  ε
*)


let rec p_S : char analist = fun l ->  l |>
     (terminal '('  -->  p_S  -->  terminal ')')
  -| epsilon

(* ------------------------------------------------------------ *)
(* Combinateurs d'analyseurs
   avec calcul supplémentaire, ex. d'un AST *)
(* ------------------------------------------------------------ *)

(* Le type des fonctions qui épluchent une liste et rendent un résultat *)
type ('res, 'term) ranalist = 'term Liste.t -> 'res * 'term Liste.t

(* Un epsilon informatif *)
let epsilon_res (info : 'res) : ('res, 'term) ranalist =
  fun l -> (info, l)

(* Terminal conditionnel avec résultat *)
(* [f] ne retourne pas un booléen mais un résultat optionnel *)
let terminal_res (f : 'term -> 'res option) : ('res, 'term) ranalist =
  fun l ->
  try
    match f (Liste.hd l) with Some y -> y, Liste.tl l | None -> raise Echec
  with
    Failure _ -> raise Echec


(* a sans résultat suivi de b donnant un résultat *)
let ( -+>) (a : 'term analist) (b : ('res, 'term) ranalist) :
      ('res, 'term) ranalist =
  fun l -> let l = a l in b l

(* a rendant un résultat suivi de b rendant un résultat *)
let (++>) (a : ('resa, 'term) ranalist) (b : 'resa -> ('resb, 'term) ranalist) :
      ('resb, 'term) ranalist =
  fun l -> let (x, l) = a l in b x l

(* a rendant un résultat suivi de b sans résultat *)
let (+->) (a : ('res, 'term) ranalist) (b : 'res -> 'term analist) :
      'term analist =
  fun l -> let (x, l) = a l in b x l

(* Choix entre a ou b informatifs *)
let (+|) (a : ('res, 'term) ranalist) (b : ('res, 'term) ranalist) :
      ('res, 'term) ranalist =
  fun l -> try a l with Echec -> b l

(* ---------------------------------- *)
(*
    S   ::=  '(' S ')'  |  'x'
*)

type ast = Fin | Pa of ast

let rec p_S : (ast, char) ranalist = fun l ->  l |>
     (terminal '('  -+>  p_S  ++>  (fun a -> terminal ')'  -+>  epsilon_res (Pa (a))))
  +| (terminal 'x'  -+>  epsilon_res Fin)

(* ---------------------------------- *)
(* Exemple avec récursion mutuelle

  B  ::=  (B)  |  C
  C  ::=  x    |  yC   |  zBC  |  ε

 *)

type boite = Emb of boite | Cont of contenu
and contenu = X | Y of contenu | Z of boite * contenu | Quedalle

let rec p_B : (boite, char) ranalist = fun l ->  l |>
    (terminal '('  -+>  p_B  ++>  fun b -> terminal ')'  -+>  epsilon_res (Emb (b)))
 +| (p_C  ++>  fun c -> epsilon_res (Cont (c)))

and p_C : (contenu, char) ranalist = fun l ->  l |>
    (terminal 'x'  -+>  epsilon_res X)
 +| (terminal 'y'  -+>  p_C  ++>  fun c -> epsilon_res (Y (c)))
 +| (terminal 'z'  -+>  p_B  ++>  fun b -> p_C  ++>  fun c -> epsilon_res (Z (b, c)))
 +| epsilon_res Quedalle

let _ = p_B (char_liste_of_string "((yz(yyx)yx))a")
