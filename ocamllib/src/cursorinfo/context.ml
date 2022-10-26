(* Typing context *)

type assumption = Var.t * Type.p_t
type t = assumption list

let empty : t = []

let lookup (ctx : t) (x : Var.t) : Type.p_t option =
  List.fold_left
    (fun found (y, ty) ->
      match found with
      | Some _ -> found
      | None -> if Var.equal x y then Some ty else None)
    None ctx

let extend (ctx : t) ((x, ty) : assumption) : t =
  match lookup ctx x with
  | None -> (x, ty) :: ctx
  | Some _ ->
      List.fold_right
        (fun (y, ty') new_ctx ->
          let ty = if Var.equal x y then ty else ty' in
          (y, ty) :: new_ctx)
        ctx empty

let concat (ctx1 : t) (ctx2 : t) : t =
  List.fold_left (fun ctx (x, ty) -> extend ctx (x, ty)) ctx1 ctx2
