open Domainslib.Task

let parallel_map pool ?(chunk_size=0) f arr =
  let len = Array.length arr in
  let res = Array.make len (f arr.(0)) in
  parallel_for ~chunk_size ~start:1 ~finish:(len - 1)
  ~body:(fun i ->
    res.(i) <- (f arr.(i))) pool;
  res

let parallel_map_list pool f l =
  let arr = Array.of_list l in
  let res = parallel_map pool f arr in
  Array.to_list res

let get_pool name =
  match lookup_pool name with
  | Some x -> x
  | None -> setup_pool ~name:name ~num_additional_domains:1 () 