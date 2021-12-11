(* 1.1 DEFINITION ET ANALYSE D'UN LANGAGE DE PROGRAMMATION SIMPLE *)

(*type variable = Var of char
type constante = Zero | One
type expression = V of variable | C of constante

type saxiome = Axiome of instruction * liste_inst | Epsilon 
and liste_inst = Plus of saxiome | Epsilon 
and instruction = Seq of variable * expression | IF of expression * saxiome * saxiome | Wh of expression * saxiome | Epsilon

type 'res ana = 'res -> 'res (* un type qui renvoi un resultat sans information supplémentaire *)
type 'res rana = 'res -> 'res * 'res  (* un type qui renvoie un résultat avec résultat supllémentaire *)


exception Echec


(* pour consommer 'x' la variable *)
let p_variable : 'res ana = fun l ->
  match l with
  | Var a -> l
  | _ -> raise Echec

(* pour consommer 0 la constante du Faux *)
let p_zero : 'res ana = fun l ->
  match l with
  |Zero -> l 
  | _ -> raise Echec

(* pour consommer 1 la constante du Vrai *)
let p_one: 'res ana = fun l->
  match l with
  | One -> l
  | _ -> raise Echec
  
(* pour consommer soit 0 ou soit 1 *)
let p_constante: 'res ana = fun l ->
  try p_zero l with
    Echec -> p_one l

(* pour consommer soit 0 soit 1 soit 'x' la variable *)
let p_expression : 'res ana =
	let p_expression1 : 'res ana = fun l -> match l with
		| V x -> V ( p_variable x ) 
		| _ -> raise Echec
	and 
		p_expression2 : 'res ana = fun l -> match l with
		| C cst -> C ( p_constante cst )
		| _ -> raise Echec
	in fun l ->	try p_expression1 l with Echec -> p_expression2 l ;;

(* pour consommer V:=E *)
let p_seq : 'res ana = fun l ->
  match l with  
  | Seq ( var , exp ) -> Seq ( p_variable var, p_expression exp )
  | _ -> raise Echec ;;


(* pour consommer IL | Epsilon *)
let rec p_saxiome : saxiome ana =
	let p_saxiome1: saxiome ana = fun l -> match l with 
		| Axiome ( instr, list_instr )  -> 
			let instr = p_instruction instr in 
			let list_instr = p_liste_inst list_instr in 
			Axiome ( instr, list_instr )
			
		| _ -> raise Echec
   and
		p_saxiome2: saxiome ana = fun l -> l
   in fun l -> try p_saxiome1 l with Echec -> p_saxiome2 l
and 
(* pour consommer ;S | Epsilon *)
p_liste_inst : liste_inst ana =
	let p_liste_instr1 : liste_inst ana = fun l -> match l with
		| Plus saxiome1 -> let saxiome = p_saxiome saxiome1 in Plus saxiome
		| _ -> raise Echec
	and
		p_liste_instr2 : liste_inst ana = fun l -> l
	in fun l -> try p_liste_instr1 l with 
						Echec -> p_liste_instr2 l 
and 

(* pour consommer V:=E | If E I_1 I_2 | Wh E I | Epsilon *)

p_instruction: instruction ana =
	let p_i1 : instruction ana = fun l -> p_seq l 
	and 
		p_i2 : instruction ana = fun l -> match l with
			| IF (expr,saxiome1,saxiome2) -> 
				let exp = p_expression expr in 
				let saxiome1 = p_saxiome saxiome1 in 
				let saxiome2 = p_saxiome saxiome2 in 
				IF (exp,saxiome1,saxiome2)
			| _ -> raise Echec
	and 
		p_i3 : instruction ana = fun l -> match l with
			| Wh (exp,saxiome) -> 
				let exp = p_expression exp in 
				let saxiome = p_saxiome saxiome in 
				Wh (exp, saxiome)
			| _ -> raise Echec
	and 
		p_i4 : instruction ana = fun l -> l
	in fun l -> try p_i1 l with 
						Echec -> try p_i2 l with 
										Echec -> try p_i3 l with 
														Echec -> p_i4 l;;

let a : variable = Var 'a';;
let b : variable = Var 'b';;
let c : variable = Var 'c';;
let zero : constante = Zero;;
let one : constante = One ;;
let i1 : instruction = Seq ( a , C one );;
let i2 : instruction = Seq ( b, C one );;
let i3 : instruction = Seq ( c, C one );;
let i4 : instruction = Seq ( c, C zero );;
let i5 : instruction = Seq ( a, V b );;
let i6 : instruction = Seq ( b, C zero );;
let i7 : instruction = Seq ( c, V a );;

let axiome2 : saxiome = Axiome ( i4, Plus ( Axiome ( i5 , Plus ( Epsilon )  ) ) );;
let axiome3 : saxiome = Axiome ( i6, Plus ( Axiome ( i7, Plus Epsilon ) )  );;
let i8 : instruction = IF ( V c, axiome2, axiome3 );;
let axiome4 : saxiome = Axiome ( i8, Plus Epsilon );;
let i9 : instruction = Wh ( V a, axiome4 );;
let axiome1 : saxiome = Axiome ( i1, Plus ( Axiome ( i2 , Plus ( Axiome ( i3, Plus ( Axiome (i9, Plus Epsilon)  )  ) )  ) )  );;
(*let x = Axiome ( Seq ( Var 'x' , C Zero ), Plus (  Axiome ( Seq( Var 'x' , C Zero ), Plus( Epsilon ) ) ));;*)
p_saxiome axiome1;; *)


(** 2.1 IMPLEMENTATION DE L'ANALYSEUR SIMPLE **)
(* type variable = Var of char
type constante = Zero | One
type expression = V of variable | C of constante

type blanks = SPACE | TAB | NEXTLINE
type stn' = Blanks of blanks * list_blanks' | Epsilon
and list_blanks' = Plusb of stn' | Epsilon

type saxiome = Axiome of stn' * instruction * liste_inst | Epsilon 
and liste_inst = Plus of stn' * saxiome | Epsilon 
and instruction = Seq of variable * expression | IF of expression * saxiome * saxiome | Wh of expression * saxiome | Epsilon

type 'res ana = 'res -> 'res (* un type qui renvoi un resultat sans information supplémentaire *)
type 'res rana = 'res -> 'res * 'res  (* un type qui renvoie un résultat avec résultat supllémentaire *)


exception Echec;;


(* pour consommer 'x' la variable *)
let p_variable : 'res ana = fun l ->
  match l with
  | Var a -> l
  | _ -> raise Echec

(* pour consommer 0 la constante du Faux *)
let p_zero : 'res ana = fun l ->
  match l with
  |Zero -> l 
  | _ -> raise Echec

(* pour consommer 1 la constante du Vrai *)
let p_one: 'res ana = fun l->
  match l with
  | One -> l
  | _ -> raise Echec
  
(* pour consommer soit 0 ou soit 1 *)
let p_constante: 'res ana = fun l ->
  try p_zero l with
    Echec -> p_one l

(* pour consommer soit 0 soit 1 soit 'x' la variable *)
let p_expression : 'res ana =
	let p_expression1 : 'res ana = fun l -> match l with
		| V x -> V ( p_variable x ) 
		| _ -> raise Echec
	and 
		p_expression2 : 'res ana = fun l -> match l with
		| C cst -> C ( p_constante cst )
		| _ -> raise Echec
	in fun l ->	try p_expression1 l with Echec -> p_expression2 l ;;

let p_blanks : blanks ana = 
	let p_NEXTLINE : blanks ana = fun l -> match l with 
		| NEXTLINE -> l
		| _ -> raise Echec
	and 
		p_SPACE : blanks ana = fun l -> match l with
		| SPACE -> l
		| _ -> raise Echec
	and 
		p_TAB : blanks ana = fun l -> match l with
		| TAB -> l
		| _ -> raise Echec
	in fun l -> try p_NEXTLINE l with 
						Echec -> try p_SPACE l with
							Echec -> p_TAB l;;

let rec p_stn' : stn' ana =
	let p_stn1' : stn' ana = fun l -> match l with
		| Blanks (blanks,list_blanks') -> Blanks ( blanks, p_list_blanks' list_blanks' )
		| _ -> raise Echec
	and
		p_stn2' : stn' ana = fun l -> l
	in fun l -> try p_stn1' l with Echec -> p_stn2' l
and 
p_list_blanks' : list_blanks' ana = 
	let p_list_blanks1' : list_blanks' ana = fun l -> match l with
		| Plusb stn' -> Plusb ( p_stn' stn' )
		| _ -> raise Echec
	and 
		p_list_blanks2' : list_blanks' ana = fun l -> l 
	in fun l -> try p_list_blanks1' l with
						Echec -> p_list_blanks2' l;;

(* pour consommer V:=E *)
let p_seq : 'res ana = fun l ->
  match l with  
  | Seq ( var , exp ) -> Seq ( p_variable var, p_expression exp )
  | _ -> raise Echec ;;


(* pour consommer IL | Epsilon *)
let rec p_saxiome : saxiome ana =
	let p_saxiome1: saxiome ana = fun l -> match l with 
		| Axiome ( stn, instr, list_instr )  -> 
			let stn = p_stn' stn in 
			let instr = p_instruction instr in 
			let list_instr = p_liste_inst list_instr in 
			Axiome ( stn, instr, list_instr )
			
		| _ -> raise Echec
   and
		p_saxiome2: saxiome ana = fun l -> l
   in fun l -> try p_saxiome1 l with Echec -> p_saxiome2 l
and 
(* pour consommer ;S | Epsilon *)
p_liste_inst : liste_inst ana =
	let p_liste_instr1 : liste_inst ana = fun l -> match l with
		| Plus ( stn,saxiome1 ) -> let stn = p_stn' stn in let saxiome = p_saxiome saxiome1 in Plus ( stn, saxiome )
		| _ -> raise Echec
	and
		p_liste_instr2 : liste_inst ana = fun l -> l
	in fun l -> try p_liste_instr1 l with 
						Echec -> p_liste_instr2 l 
and

p_instruction: instruction ana = 
	let p_i1 : instruction ana = fun l -> p_seq l 
	and
		p_i2 : instruction ana = fun l -> match l with
			| IF (expr,saxiome1,saxiome2) -> 
				let exp = p_expression expr in 
				let saxiome1 = p_saxiome saxiome1 in 
				let saxiome2 = p_saxiome saxiome2 in 
				IF (exp,saxiome1,saxiome2)
			| _ -> raise Echec
	and 
		p_i3 : instruction ana = fun l -> match l with
			| Wh (exp,saxiome) -> 
				let exp = p_expression exp in 
				let saxiome = p_saxiome saxiome in 
				Wh (exp, saxiome)
			| _ -> raise Echec
	and 
		p_i4 : instruction ana = fun l -> l
	in fun l -> try p_i1 l with 
						Echec -> try p_i2 l with 
										Echec -> try p_i3 l with 
														Echec -> p_i4 l;;


let t_1 : stn' = Blanks( TAB, Epsilon );;
let t_2 : stn' = Blanks( TAB, Plusb( Blanks ( TAB, Epsilon ) ) );;
let n_1 : stn' = Blanks ( NEXTLINE, Epsilon );;
let n_1_t_1 : stn' = Blanks( NEXTLINE, Plusb (  t_1 ) );;
let n_1_t_2 : stn' = Blanks ( NEXTLINE, Plusb ( t_2 ) );;
let a : variable = Var 'a';;
let b : variable = Var 'b';;
let c : variable = Var 'c';;
let zero : constante = Zero;;
let one : constante = One ;;
let i1 : instruction = Seq ( a , C one );;
let i2 : instruction = Seq ( b, C one );;
let i3 : instruction = Seq ( c, C one );;
let i4 : instruction = Seq ( c, C zero );;
let i5 : instruction = Seq ( a, V b );;
let i6 : instruction = Seq ( b, C zero );;
let i7 : instruction = Seq ( c, V a );;
let axiome2 : saxiome = Axiome ( n_1_t_2, i4, Plus ( Epsilon, Axiome ( n_1_t_2, i5 , Plus ( n_1_t_1, Epsilon )  ) ) );;
let axiome3 : saxiome = Axiome ( n_1_t_2 , i6, Plus ( Epsilon, Axiome ( n_1_t_2 , i7, Plus ( n_1_t_1 , Epsilon ) ) )  );;
let i8 : instruction = IF ( V c, axiome2, axiome3 );;
let axiome4 : saxiome = Axiome ( n_1_t_1, i8, Plus ( Epsilon, Epsilon ) );;
let i9 : instruction = Wh ( V a, axiome4 );;
let axiome1 : saxiome = Axiome ( Epsilon, i1, Plus ( Epsilon, Axiome ( n_1, i2 , Plus ( Epsilon, Axiome ( n_1, i3, Plus ( Epsilon, Axiome ( n_1, i9, Plus (n_1, Epsilon)  )  ) )  ) )  ) );;
(*let x = Axiome ( Seq ( Var 'x' , C Zero ), Plus (  Axiome ( Seq( Var 'x' , C Zero ), Plus( Epsilon ) ) ));;*)
p_saxiome axiome1;; *)