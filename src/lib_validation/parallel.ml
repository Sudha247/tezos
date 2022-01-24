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
  match Lwt_domain.lookup_pool name with
  | Some x -> x
  | None -> Lwt_domain.setup_pool ~name 1

let get_pool_d name =
  match Domainslib.Task.lookup_pool name with
  | Some x -> x
  | None -> Domainslib.Task.setup_pool ~name ~num_additional_domains:1 ()