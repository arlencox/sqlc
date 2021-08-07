open Types

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


(* basic test and main *)

let test1 () =
  let prog = App(Fn("x", Int 3), Int 4) in
  let result = eval initial_env prog in
  assert (result = VInt 3);
  Format.printf "%a@." pp_vl result



let () =
  let ic = open_in Sys.argv.(1) in
  let lexbuf = Lexing.from_channel ic in
  let ast = Parse.prog Lex.token lexbuf in
  Format.printf "%a@." pp_ex ast;
  let result = eval initial_env ast in
  Format.printf "%a@." pp_vl result

