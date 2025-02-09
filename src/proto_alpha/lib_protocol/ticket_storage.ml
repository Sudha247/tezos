(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Trili Tech, <contact@trili.tech>                       *)
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

type key_hash = Script_expr_hash.t

type error +=
  | Negative_ticket_balance of {key : Script_expr_hash.t; balance : Z.t}
  | Failed_to_hash_node

let script_expr_hash_of_key_hash key_hash = key_hash

let hash_bytes_cost bytes =
  let module S = Saturation_repr in
  let ( + ) = S.add in
  let v0 = S.safe_int @@ Bytes.length bytes in
  let ( lsr ) = S.shift_right in
  S.safe_int 200 + (v0 + (v0 lsr 2)) |> Gas_limit_repr.atomic_step_cost

let hash_of_node ctxt node =
  Raw_context.consume_gas ctxt (Script_repr.strip_locations_cost node)
  >>? fun ctxt ->
  let node = Micheline.strip_locations node in
  match Data_encoding.Binary.to_bytes_opt Script_repr.expr_encoding node with
  | Some bytes ->
      Raw_context.consume_gas ctxt (hash_bytes_cost bytes) >|? fun ctxt ->
      (Script_expr_hash.hash_bytes [bytes], ctxt)
  | None -> error Failed_to_hash_node

let make_key_hash ctxt ~ticketer ~typ ~contents ~owner =
  hash_of_node ctxt @@ Micheline.Seq (0, [ticketer; typ; contents; owner])

let () =
  let open Data_encoding in
  register_error_kind
    `Permanent
    ~id:"Negative_ticket_balance"
    ~title:"Negative ticket balance"
    ~description:"Attempted to set a negative ticket balance value"
    ~pp:(fun ppf (key, balance) ->
      Format.fprintf
        ppf
        "Attempted to set negative ticket balance value '%a' for key %a."
        Z.pp_print
        balance
        Script_expr_hash.pp
        key)
    (obj2 (req "key" Script_expr_hash.encoding) (req "balance" Data_encoding.z))
    (function
      | Negative_ticket_balance {key; balance} -> Some (key, balance)
      | _ -> None)
    (fun (key, balance) -> Negative_ticket_balance {key; balance}) ;
  register_error_kind
    `Branch
    ~id:"Failed_to_hash_node"
    ~title:"Failed to hash node"
    ~description:"Failed to hash node for a key in the ticket-balance table"
    ~pp:(fun ppf () ->
      Format.fprintf
        ppf
        "Failed to hash node for a key in the ticket-balance table")
    Data_encoding.empty
    (function Failed_to_hash_node -> Some () | _ -> None)
    (fun () -> Failed_to_hash_node)

let get_balance ctxt key =
  Storage.Ticket_balance.Table.find ctxt key >|=? fun (ctxt, res) -> (res, ctxt)

let set_balance ctxt key balance =
  let cost_of_key = Z.of_int 65 in
  fail_when
    Compare.Z.(balance < Z.zero)
    (Negative_ticket_balance {key; balance})
  >>=? fun () ->
  if Compare.Z.(balance = Z.zero) then
    Storage.Ticket_balance.Table.remove ctxt key
    >|=? fun (ctxt, freed, existed) ->
    (* If we remove an existing entry, then we return the freed size for
       both the key and the value. *)
    let freed =
      if existed then Z.neg @@ Z.add cost_of_key (Z.of_int freed) else Z.zero
    in
    (freed, ctxt)
  else
    Storage.Ticket_balance.Table.add ctxt key balance
    >|=? fun (ctxt, size_diff, existed) ->
    let size_diff =
      let z_diff = Z.of_int size_diff in
      (* For a new entry we also charge the space for storing the key *)
      if existed then z_diff else Z.add cost_of_key z_diff
    in
    (size_diff, ctxt)

let adjust_balance ctxt key ~delta =
  get_balance ctxt key >>=? fun (res, ctxt) ->
  let old_balance = Option.value ~default:Z.zero res in
  set_balance ctxt key (Z.add old_balance delta)
