open Sexplib.Std

type ('z, 'a) t = ('a list) * 'z * ('a list) [@@deriving sexp]

let singleton (z: 'z) = ([], z, [])

let rec split_at (n : int) (xs : 'a list) : (('a, 'a) t) option =
    match n, xs with
    | _, [] -> None
    | 0, hd :: tl -> Some (([], hd, tl))
    | _, hd :: tl ->
        begin match split_at (n - 1) tl with
        | None -> None
        | Some (pre, z, suf) -> Some ((hd :: pre, z, suf))
        end

let map_z (f : 'z1 -> 'z2) ((pre, z, suf) : ('z1, 'a) t) : ('z2, 'a) t = (pre, f z, suf)

(* let equal (f_z : 'z -> 'z -> bool) (f_a : 'a -> 'a -> bool) ((pre1, z1, suf1) : ('z, 'a) t) ((pre2, z2, suf2) : ('z, 'a) t) : bool =
    (List.equal f_a pre1 pre2) && (f_z z1 z2) && (List.equal f_a suf1 suf2) *)

let map (f : 'z -> 'a) ((pre, z, suf) : ('z, 'a) t) : 'a list =
    List.concat [
        pre;
        [f z];
        suf;
    ]
    
