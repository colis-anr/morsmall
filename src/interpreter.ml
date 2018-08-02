
open AST.AST

let rec command env = function
  | Simple (_al, _wl) ->
     assert false

  | Async _c ->
     assert false

  | Seq (c1, c2) ->
     command (command env c1) c2

  | And (c1, c2) ->
     let env = command env c1 in
     if Env.is_success env then
       command env c2
     else
       env

  | Or (c1, c2) ->
     let env = command env c1 in
     if Env.is_success env then
       env
     else
       command env c2

  | Not c ->
     let env = command env c in
     Env.negate env

  | Pipe (_c1, _c2) ->
     assert false

  | Subshell _c ->
     assert false

  | For (_n, _wlo, _c) ->
     assert false

  | Case (_w, _cil) ->
     assert false

  | If (_c1, _c2, _c3o) ->
     assert false

  | While (_c1, _c2) ->
     assert false

  | Until (_c1, _c2) ->
     assert false

  | Function (_n, _c) ->
     assert false

  | Redirection (_c, _d, _k, _w) ->
     assert false

  | HereDocument (_c, _d, _w) ->
     assert false
