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

