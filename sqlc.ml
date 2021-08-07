module SM = Map.Make(String)

type ex =
  | Fn of string * ex
  | App of ex * ex
  | Var of string
  | Int of int
  | Error of string

type vl =
  | VInt of int
  | VClosure of en * string * ex
  | VOp of string
  | VOp_Partial of string * vl
  | VError of string

and en = vl SM.t

let rec pp_ex ff = function
  | Fn (x, ex) -> Format.fprintf ff "(fn %s %a)" x pp_ex ex
  | App (ex1, ex2) -> Format.fprintf ff "(%a %a)" pp_ex ex1 pp_ex ex2
  | Var x -> Format.fprintf ff "%s" x
  | Int i -> Format.fprintf ff "%d" i
  | Error msg -> Format.fprintf ff "<error: %s>" msg

let rec pp_vl ff = function
  | VInt i -> Format.pp_print_int ff i
  | VError msg -> Format.fprintf ff "ERROR!! %s" msg
  | VOp op -> Format.fprintf ff "%s" op
  | VOp_Partial (op, fst) -> Format.fprintf ff "(%s %a)" op pp_vl fst
  | VClosure (_, x, ex) -> Format.fprintf ff "<closure: %s %a>" x pp_ex ex

(* Evaluation *)

let rec eval env ex k =
  match ex with
  | Fn (x, ex) ->
    k (VClosure(env, x, ex))
  | App (f, a) ->
    eval env f 
      (function
        | VClosure (env', x, body) ->
          eval env a (function
              | VError _ as err -> k err
              | va ->
                let env' = SM.add x va env' in
                eval env' body k
            )
        | VOp op ->
          eval env a (function
              | VError _ as err -> k err
              | va ->
                k (VOp_Partial (op, va))
            )
        | VOp_Partial (op, fst) ->
          eval env a (function
              | VError _ as err -> k err
              | snd ->
                begin match op, fst, snd with
                  | "+", VInt x, VInt y -> k (VInt (x + y))
                  | "-", VInt x, VInt y -> k (VInt (x - y))
                  | "*", VInt x, VInt y -> k (VInt (x * y))
                  | "/", VInt x, VInt y -> k (VInt (x / y))
                  | "%", VInt x, VInt y -> k (VInt (x mod y))
                  | _ -> k (VError "Type error")
                end
            )

        | _ -> k (VError "Cannot call a non-closure")
      )
  | Var x ->
    begin match SM.find x env with
      | exception Not_found -> k (VError ("Variable '"^x^"' not found"))
      | vl -> k vl
    end
  | Int i ->
    k (VInt i)
  | Error msg ->
    k (VError msg)

let eval env ex =
  eval env ex (fun vl -> vl)

let initial_env =
  [
    "+", VOp "+";
    "-", VOp "-";
    "*", VOp "*";
    "/", VOp "/";
    "%", VOp "%";
  ] |> List.to_seq |> SM.of_seq


(* Lexer *)

let is_whitespace c =
  c == ' ' || c == '\n' || c == '\r' || c == '\t'

let is_ident c =
  c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z' || c = '_' || c = '+' || c = '-' || c = '*' || c = '/' || c == '%'

let is_int c =
  c >= '0' && c <= '9'

type token =
  | Token_FN
  | Token_IDENT of string
  | Token_LPAREN
  | Token_RPAREN
  | Token_INT of int
  | Token_EOF
  | Token_ERROR of string

type lex_state = {
  lex_str: string;
  lex_off: int;
}

let lex_next state =
  {state with lex_off = state.lex_off + 1}

let lex_char state =
  if state.lex_off < String.length state.lex_str then
    state.lex_str.[state.lex_off]
  else
    Char.chr 0

let rec lex_ident buf state k =
  let c = lex_char state in
  if is_ident c then begin
    Buffer.add_char buf c;
    lex_ident buf (lex_next state) k
  end else
    match Buffer.contents buf with
    | "fn" -> k Token_FN state
    | ident -> k (Token_IDENT ident) state

let rec lex_int buf state k =
  let c = lex_char state in
  if is_int c then begin
    Buffer.add_char buf c;
    lex_int buf (lex_next state) k
  end else match (int_of_string (Buffer.contents buf)) with
    | exception Failure _ -> k (Token_ERROR "Invalid integer") state
    | i -> k (Token_INT i) state

let rec lex state k =
  let c = lex_char state in
  if is_whitespace c then
    lex (lex_next state) k
  else if is_ident c then
    lex_ident (Buffer.create 20) state k
  else if is_int c then
    lex_int (Buffer.create 20) state k
  else match c with
    | '(' -> k Token_LPAREN (lex_next state)
    | ')' -> k Token_RPAREN (lex_next state)
    | c when Char.code c = 0 -> k Token_EOF state
    | c -> k (Token_ERROR ("Invalid character: "^(String.make 1 c))) (lex_next state)



(* Parser *)
    
type parse_state = {
  parse_lex: lex_state;
  parse_head: token;
}

let parse_head state =
  state.parse_head

let parse_next state =
  lex state.parse_lex (fun parse_head parse_lex -> {parse_lex; parse_head})
  

let is_base state =
  match parse_head state with
  | Token_INT _ -> true
  | Token_LPAREN -> true
  | Token_IDENT _ -> true
  | _ -> false

let rec parse_fn state k =
  match parse_head state with
  | Token_FN ->
    let state = parse_next state in
    begin match parse_head state with
      | Token_IDENT id ->
        let state = parse_next state in
        parse_ex state (fun body state ->
            k (Fn (id, body)) state
          )
      | _ ->
        k (Error "Expected identifier") state
    end
  | _ ->
    k (Error "Expected fn") state

and parse_base state k =
  match parse_head state with
  | Token_INT i ->
    k (Int i) (parse_next state)
  | Token_LPAREN ->
    let state = parse_next state in
    parse_ex state (fun result state ->
        match parse_head state with
        | Token_RPAREN ->
          k result (parse_next state)
        | _ ->
          k (Error "Expected ')'") state
      )
  | Token_IDENT id ->
    k (Var id) (parse_next state)
  | Token_ERROR msg ->
    k (Error msg) (parse_next state)
  | _ ->
    k (Error "Expected base expression") (parse_next state)

and parse_app last_base state k =
  if is_base state then
    parse_base state (fun base state ->
        match last_base with
        | Some last_base ->
          parse_app (Some (App (last_base, base))) state k
        | None ->
          parse_app (Some base) state k
      )
  else match last_base with
    | None ->
      k (Error "Cannot have empty parens") state
    | Some app ->
      k app state

and parse_ex state k =
  match parse_head state with
  | Token_FN ->
    parse_fn state k
  | _ ->
    parse_app None state k

let parse lex_str =
  let lex_state = {
    lex_str;
    lex_off = 0;
  } in
  lex lex_state (fun head parse_lex ->
      let state = {parse_head = head; parse_lex} in
      parse_ex state (fun ex state ->
          match parse_head state with
          | Token_EOF ->
            ex
          | Token_ERROR msg ->
            Error msg
          | _ ->
            Error "Expected end of file"
        )
    )


(* basic test and main *)

let test1 () =
  let prog = App(Fn("x", Int 3), Int 4) in
  let result = eval initial_env prog in
  assert (result = VInt 3);
  Format.printf "%a@." pp_vl result



let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let buf = Buffer.create n in
  Buffer.add_channel buf ic n;
  close_in ic;
  Buffer.contents buf


let () =
  let contents = load_file Sys.argv.(1) in
  let ast = parse contents in
  Format.printf "%a@." pp_ex ast;
  let result = eval initial_env ast in
  Format.printf "%a@." pp_vl result

