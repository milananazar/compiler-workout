(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
*)
open GT 
    
(* Simple expressions: syntax and semantics *)
module Expr =
  struct
    
    (* The type for expressions. Note, in regular OCaml there is no "@type..." 
       notation, it came from GT. 
    *)
    @type t =
    (* integer constant *) | Const of int
    (* variable         *) | Var   of string
    (* binary operator  *) | Binop of string * t * t with show

    (* Available binary operators:
        !!                   --- disjunction
        &&                   --- conjunction
        ==, !=, <=, <, >=, > --- comparisons
        +, -                 --- addition, subtraction
        *, /, %              --- multiplication, division, reminder
    *)
                                                            
    (* State: a partial map from variables to integer values. *)
    type state = string -> int 

    (* Empty state: maps every variable into nothing. *)
    let empty = fun x -> failwith (Printf.sprintf "Undefined variable %s" x)

    (* Update: non-destructively "modifies" the state s by binding the variable x 
      to value v and returns the new state.
    *)
    	let update x v s = fun y -> if x = y then v else s y

    	let bool_to_int b = if b then 1 else 0;;

	let int_to_bool i = i != 0;;

	let get_operator operator = match operator with
	| "+" -> ( + )
	| "-" -> ( - )
	| "*" -> ( * )
	| "/" -> ( / )
	| "%" -> ( mod )
	| "<" -> fun left_expression right_expression -> bool_to_int ( ( < ) left_expression right_expression )
	| "<=" -> fun left_expression right_expression -> bool_to_int ( ( <= ) left_expression right_expression )
	| ">"  -> fun left_expression right_expression -> bool_to_int ( ( > ) left_expression right_expression )
	| ">=" -> fun left_expression right_expression -> bool_to_int ( ( >= ) left_expression right_expression )
	| "==" -> fun left_expression right_expression -> bool_to_int ( ( == ) left_expression right_expression )
	| "!=" -> fun left_expression right_expression -> bool_to_int ( ( != ) left_expression right_expression )
	| "&&" -> fun left_expression right_expression -> bool_to_int ( ( && ) ( int_to_bool left_expression ) ( int_to_bool right_expression ) )
	| "!!" -> fun left_expression right_expression -> bool_to_int ( ( || ) ( int_to_bool left_expression ) ( int_to_bool right_expression ) );;

	let rec eval state expression = match expression with
	| Const const -> const
	| Var var -> state var
	| Binop (operator, left_expression, right_expression) -> get_operator operator (eval state left_expression) (eval state right_expression);;
              

  	end
                    
(* Simple statements: syntax and sematics *)
module Stmt =
  struct

    (* The type for statements *)
    @type t =
    (* read into the variable           *) | Read   of string
    (* write the value of an expression *) | Write  of Expr.t
    (* assignment                       *) | Assign of string * Expr.t
    (* composition                      *) | Seq    of t * t with show

    (* The type of configuration: a state, an input stream, an output stream *)
    type config = Expr.state * int list * int list 

    (* Statement evaluator

          val eval : config -> t -> config

       Takes a configuration and a statement, and returns another configuration
    *)
  let rec eval config statement = match (config, statement) with
      | ((state, value::inp, out), Read name) -> (Expr.update name value state, inp, out)
      | ((state, inp, out), Write expr) -> (state, inp, out @ [(Expr.eval state expr)])
      | ((state, inp, out), Assign (name, expr)) -> ((Expr.update name (Expr.eval state expr) state), inp, out)
      | (config, Seq (s1, s2)) -> eval (eval config s1) s2
      | _ -> failwith "Unknown operation"
                                                                                                                 
  end

(* The top-level definitions *)

(* The top-level syntax category is statement *)
type t = Stmt.t    

(* Top-level evaluator

     eval : int list -> t -> int list

   Takes a program and its input stream, and returns the output stream
*)
let eval i p =
  let _, _, o = Stmt.eval (Expr.empty, i, []) p in o
