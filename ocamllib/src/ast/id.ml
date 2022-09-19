type t = int

let last_id : t ref = ref (-1)

let generate : unit -> t =
 fun _ ->
  last_id := !last_id + 1;
  !last_id

let reset : unit -> unit = fun _ -> last_id := -1
