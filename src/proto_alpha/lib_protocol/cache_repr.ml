(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

module Cache_costs = struct
  module S = Saturation_repr

  (* Computed by typing the contract
     "{parameter unit; storage unit; code FAILWITH}"
     and evaluating
     [(8 * Obj.reachable_words (Obj.repr typed_script))]
     where [typed_script] is of type [ex_script] *)
  let minimal_size_of_typed_contract_in_bytes = 688

  let approximate_cardinal bytes =
    S.safe_int (bytes / minimal_size_of_typed_contract_in_bytes)

  let log2 x = S.safe_int (1 + S.numbits x)

  let cache_update_constant = S.safe_int 600

  let cache_update_coeff = S.safe_int 57

  (* Cost of calling [Environment_cache.update]. *)
  let cache_update ~cache_size_in_bytes =
    let approx_card = approximate_cardinal cache_size_in_bytes in
    Gas_limit_repr.atomic_step_cost
      S.(add cache_update_constant (mul cache_update_coeff (log2 approx_card)))

  (* Cost of calling [Environment_cache.find].
     This overapproximates [cache_find] slightly. *)
  let _cache_find = cache_update
end

type index = int

type size = int

type identifier = string

type namespace = string

let compare_namespace = Compare.String.compare

type internal_identifier = {namespace : namespace; id : identifier}

let separator = '@'

let sanitize namespace =
  if String.contains namespace separator then
    invalid_arg
      (Format.asprintf
         "Invalid cache namespace: '%s'. Character %c is forbidden."
         namespace
         separator)
  else namespace

let string_of_internal_identifier {namespace; id} =
  namespace ^ String.make 1 separator ^ id

let internal_identifier_of_string raw =
  match String.split_on_char separator raw with
  | [] -> assert false
  | namespace :: id ->
      (* An identifier may contain [separator], hence we concatenate
         possibly splitted parts of [id]. *)
      {namespace = sanitize namespace; id = String.concat "" id}

let internal_identifier_of_key key =
  let raw = Raw_context.Cache.identifier_of_key key in
  internal_identifier_of_string raw

let key_of_internal_identifier ~cache_index identifier =
  let raw = string_of_internal_identifier identifier in
  Raw_context.Cache.key_of_identifier ~cache_index raw

let make_key =
  let namespaces = ref [] in
  fun ~cache_index ~namespace ->
    let namespace = sanitize namespace in
    if List.mem ~equal:String.equal namespace !namespaces then
      invalid_arg
        (Format.sprintf
           "Cache key namespace %s already exist."
           (namespace :> string))
    else (
      namespaces := namespace :: !namespaces ;
      fun ~id ->
        let identifier = {namespace; id} in
        key_of_internal_identifier ~cache_index identifier)

module NamespaceMap = Map.Make (struct
  type t = namespace

  let compare = compare_namespace
end)

type partial_key_handler =
  Raw_context.t -> string -> Context.Cache.value tzresult Lwt.t

let value_of_key_handlers : partial_key_handler NamespaceMap.t ref =
  ref NamespaceMap.empty

module Admin = struct
  include Raw_context.Cache

  let list_keys context ~cache_index =
    Raw_context.Cache.list_keys context ~cache_index

  let key_rank context key = Raw_context.Cache.key_rank context key

  let value_of_key ctxt key =
    (* [value_of_key] is a maintainance operation: it is typically run
       when a node reboots. For this reason, this operation is not
       carbonated. *)
    let ctxt = Raw_context.set_gas_unlimited ctxt in
    let {namespace; id} = internal_identifier_of_key key in
    match NamespaceMap.find namespace !value_of_key_handlers with
    | Some value_of_key -> value_of_key ctxt id
    | None ->
        failwith
          (Format.sprintf "No handler for key `%s%c%s'" namespace separator id)
end

module type CLIENT = sig
  val cache_index : int

  val namespace : namespace

  type cached_value

  val value_of_identifier :
    Raw_context.t -> identifier -> cached_value tzresult Lwt.t
end

module type INTERFACE = sig
  type cached_value

  val update :
    Raw_context.t ->
    identifier ->
    (cached_value * int) option ->
    Raw_context.t tzresult

  val find : Raw_context.t -> identifier -> cached_value option tzresult Lwt.t

  val list_identifiers : Raw_context.t -> (identifier * int) list

  val identifier_rank : Raw_context.t -> identifier -> int option

  val size : Raw_context.t -> size

  val size_limit : Raw_context.t -> size
end

let register_exn (type cvalue)
    (module C : CLIENT with type cached_value = cvalue) :
    (module INTERFACE with type cached_value = cvalue) =
  if
    Compare.Int.(
      C.cache_index < 0
      || C.cache_index >= List.length Constants_repr.cache_layout)
  then invalid_arg "Cache index is invalid" ;
  let mk = make_key ~cache_index:C.cache_index ~namespace:C.namespace in
  (module struct
    type cached_value = C.cached_value

    type Admin.value += K of cached_value

    let () =
      let voi ctxt i =
        C.value_of_identifier ctxt i >>=? fun v -> return (K v)
      in
      value_of_key_handlers :=
        NamespaceMap.add C.namespace voi !value_of_key_handlers

    let size ctxt =
      Option.value ~default:max_int
      @@ Admin.cache_size ctxt ~cache_index:C.cache_index

    let size_limit ctxt =
      Option.value ~default:max_int
      @@ Admin.cache_size_limit ctxt ~cache_index:C.cache_index

    let update ctxt id v =
      let cache_size_in_bytes = size ctxt in
      Raw_context.consume_gas
        ctxt
        (Cache_costs.cache_update ~cache_size_in_bytes)
      >|? fun ctxt ->
      let v = Option.map (fun (v, size) -> (K v, size)) v in
      Admin.update ctxt (mk ~id) v

    let find ctxt id =
      let cache_size_in_bytes = size ctxt in
      Raw_context.consume_gas
        ctxt
        (Cache_costs.cache_update ~cache_size_in_bytes)
      >>?= fun ctxt ->
      Admin.find ctxt (mk ~id) >>= function
      | None -> return None
      | Some (K v) -> return (Some v)
      | _ ->
          (* This execution path is impossible because all the keys of
             C's namespace (which is unique to C) are constructed with
             [K]. This [assert false] could have been pushed into the
             environment in exchange for extra complexity. The
             argument that justifies this [assert false] seems
             simple enough to keep the current design though. *)
          assert false

    let list_identifiers ctxt =
      Admin.list_keys ctxt ~cache_index:C.cache_index |> function
      | None ->
          (* `cache_index` is valid. *)
          assert false
      | Some list ->
          List.filter_map
            (fun (key, age) ->
              let {namespace; id} = internal_identifier_of_key key in
              if String.equal namespace C.namespace then Some (id, age)
              else None)
            list

    let identifier_rank ctxt id = Admin.key_rank ctxt (mk ~id)
  end)
