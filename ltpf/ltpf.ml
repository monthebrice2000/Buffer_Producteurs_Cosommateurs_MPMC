type variable = Var of char
type constante = Zero | One
type expression = V of variable | C of constante

type saxiome = Axiome of instruction * liste_inst | Epsilon and liste_inst = Plus of saxiome | Epsilon and instruction = Seq of expression * expression | TwoI of saxiome * saxiome | IF of saxiome * saxiome | Wh of expression * saxiome | Epsilon

type ( 'saxiome, 'res ) rianalist = 'saxiome list -> 'res * 'saxiome list

exception Echec

let p_variable : ('saxiome, 'res ) rianalist = fun l ->
  match l with
  | x::l -> x,l
  | _ -> raise Echec;;

let p_zero: ( 'saxiome, 'res ) rianalist = fun l ->
  match l with
  | x::l -> x,l
  | _ -> raise Echec

let p_one: ( 'saxiome, 'res ) rianalist = fun l->
  match l with
  | x::l -> x,l
  | _ -> raise Echec

let p_constante: ( 'saxiome, 'res ) rianalist = fun l ->
  try p_zero l with
    Echec -> p_one l

let p_expression : ( 'saxiome, 'res ) rianalist = fun l ->
	try p_variable l with 
		Echec -> p_constante l

let p_seq : ( 'saxiome, 'res ) rianalist = fun l ->
  match l with
  | x::l -> x,l
  | _ -> raise Echec

let rec p_saxiome : ( 'saxiome, 'res ) rianalist =
	let p_saxiome1: ( 'saxiome, 'res ) rianalist = fun l ->
    let (i1,l) = p_instruction l in
    let (i2,l) = p_liste_inst l in i2,l
   and
		p_saxiome2: ( 'saxiome, 'res ) rianalist = fun l -> Epsilon,l 
   in fun l -> try p_saxiome1 l with Echec -> p_saxiome2 l
and 
p_liste_inst : ( 'saxiome, 'res ) rianalist = 
	let p_liste_instr1 : ( 'saxiome, 'res ) rianalist = fun l ->
		let (s,l) = p_saxiome l in s,l
	and
		p_liste_instr2 : ( 'saxiome, 'res ) rianalist = fun l -> Epsilon, l
	in fun l -> try p_liste_instr1 l with 
						Echec -> p_liste_instr2 l
and 
p_instruction: ( 'saxiome, 'res ) rianalist =
	let p_i2 : ( 'saxiome, 'res ) rianalist = fun l ->
		let i1,l = p_instruction l in let i2,l = p_instruction l in i1,l
	and 
		p_i1 : ( 'saxiome, 'res ) rianalist = fun l ->
			let i1,l = p_seq l in (i1,l)
	in fun l -> try  p_i1 l with 
						Echec -> p_i2 l;;

let var: variable = Var('x')
let va : constante = Zero
let i1 : instruction = Seq( V(var), C(va) )
let saxiome : 'saxiome list = i1::i1::Epsilon::[];;
let test,l = p_saxiome saxiome;;