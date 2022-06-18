(* Functions to convert between lists and BigArrays *)
open Bigarray
open Int32

(* Convert nodes from Bigarray to OCaml list *)
let array1_to_list (arr : (int32, int32_elt, c_layout) Array1.t) : int list =
  let rec array1_to_list_aux (arr : (int32, int32_elt, c_layout) Array1.t)
      (len : int) : int list =
    if len > 0
    then array1_to_list_aux arr (len - 1) @ [ to_int arr.{len - 1} ]
    else []
  in
  array1_to_list_aux arr (Array1.dim arr)

(* Convert OCaml nodes list to Bigarray *)
let list_to_array1 (l : int list) : (int32, int32_elt, c_layout) Array1.t =
  let nodes = Array1.create Int32 c_layout (List.length l) in
  let rec list_to_array1_aux (l : int list)
      (arr : (int32, int32_elt, c_layout) Array1.t) (count : int) : unit =
    match l with
    | [] -> ()
    | hd :: tl ->
        let _ = arr.{count} <- of_int hd in
        list_to_array1_aux tl arr (count + 1)
  in
  let _ = list_to_array1_aux l nodes 0 in
  nodes

(* Convert edges from Bigarray to OCaml list *)
let edge_to_list (arr : (int32, int32_elt, c_layout) Array2.t) :
    (int * int * int) list =
  let rec edge_to_list_aux (arr : (int32, int32_elt, c_layout) Array2.t)
      (len : int) : (int * int * int) list =
    if len > 0
    then
      (to_int arr.{len - 1, 0}, to_int arr.{len - 1, 1}, to_int arr.{len - 1, 2})
      :: edge_to_list_aux arr (len - 1)
    else []
  in
  edge_to_list_aux arr (Array2.dim1 arr)

(* Convert OCaml edges list to Bigarray *)
let list_to_edge (l : (int * int * int) list) :
    (int32, int32_elt, c_layout) Array2.t =
  let edges = Array2.create Int32 c_layout (List.length l) 3 in
  let rec list_to_edge_aux (l : (int * int * int) list)
      (arr : (int32, int32_elt, c_layout) Array2.t) (count : int) : unit =
    match l with
    | [] -> ()
    | (start, stop, index) :: tl ->
        arr.{count, 0} <- of_int start;
        arr.{count, 1} <- of_int stop;
        arr.{count, 2} <- of_int index;
        list_to_edge_aux tl arr (count + 1)
  in
  let _ = list_to_edge_aux l edges 0 in
  edges

(* Convert unit tests from Bigarray to OCaml list *)
let tests_to_list (arr : (int32, int32_elt, c_layout) Array2.t) :
    (int * int) list =
  let rec tests_to_list_aux (arr : (int32, int32_elt, c_layout) Array2.t)
      (len : int) : (int * int) list =
    if len > 0
    then
      (to_int arr.{len - 1, 0}, to_int arr.{len - 1, 1})
      :: tests_to_list_aux arr (len - 1)
    else []
  in
  tests_to_list_aux arr (Array2.dim1 arr)

(* Convert OCaml unit tests to Bigarray *)
let list_to_tests (l : (int * int) list) : (int32, int32_elt, c_layout) Array2.t
    =
  let tests = Array2.create Int32 c_layout (List.length l) 2 in
  let rec list_to_edge_aux (l : (int * int) list)
      (arr : (int32, int32_elt, c_layout) Array2.t) (count : int) : unit =
    match l with
    | [] -> ()
    | (input, output) :: tl ->
        let _ = arr.{count, 0} <- of_int input in
        let _ = arr.{count, 1} <- of_int output in
        list_to_edge_aux tl arr (count + 1)
  in
  let _ = list_to_edge_aux l tests 0 in
  tests
