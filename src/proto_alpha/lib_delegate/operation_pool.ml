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

open Protocol
open Alpha_context

(* Should we use a better ordering ? *)

module OpSet = Set.Make (struct
  type t = packed_operation

  let compare = compare
end)

(* TODO refine this: unpack operations *)
type pool = {
  consensus : OpSet.t;
  votes : OpSet.t;
  anonymous : OpSet.t;
  managers : OpSet.t;
}

(* TODO refine this: unpack operations *)
type ordered_pool = {
  ordered_consensus : packed_operation list;
  ordered_votes : packed_operation list;
  ordered_anonymous : packed_operation list;
  ordered_managers : packed_operation list;
}

let ordered_pool_encoding =
  let open Data_encoding in
  conv
    (fun {ordered_consensus; ordered_votes; ordered_anonymous; ordered_managers} ->
      (ordered_consensus, ordered_votes, ordered_anonymous, ordered_managers))
    (fun (ordered_consensus, ordered_votes, ordered_anonymous, ordered_managers) ->
      {ordered_consensus; ordered_votes; ordered_anonymous; ordered_managers})
    (obj4
       (req "ordered_consensus" (list (dynamic_size Operation.encoding)))
       (req "ordered_votes" (list (dynamic_size Operation.encoding)))
       (req "ordered_payload" (list (dynamic_size Operation.encoding)))
       (req "ordered_payload" (list (dynamic_size Operation.encoding))))

type payload = {
  votes_payload : packed_operation list;
  anonymous_payload : packed_operation list;
  managers_payload : packed_operation list;
}

let empty_payload =
  {votes_payload = []; anonymous_payload = []; managers_payload = []}

let payload_encoding =
  let open Data_encoding in
  conv
    (fun {votes_payload; anonymous_payload; managers_payload} ->
      (votes_payload, anonymous_payload, managers_payload))
    (fun (votes_payload, anonymous_payload, managers_payload) ->
      {votes_payload; anonymous_payload; managers_payload})
    (obj3
       (req "votes_payload" (list (dynamic_size Operation.encoding)))
       (req "anonymous_payload" (list (dynamic_size Operation.encoding)))
       (req "managers_payload" (list (dynamic_size Operation.encoding))))

let pp_payload fmt {votes_payload; anonymous_payload; managers_payload} =
  Format.fprintf
    fmt
    "payload: [votes: %d, anonymous: %d, managers: %d]"
    (List.length votes_payload)
    (List.length anonymous_payload)
    (List.length managers_payload)

let empty =
  {
    consensus = OpSet.empty;
    votes = OpSet.empty;
    anonymous = OpSet.empty;
    managers = OpSet.empty;
  }

let empty_ordered =
  {
    ordered_consensus = [];
    ordered_votes = [];
    ordered_anonymous = [];
    ordered_managers = [];
  }

let pp_pool fmt {consensus; votes; anonymous; managers} =
  Format.fprintf
    fmt
    "[consensus: %d, votes: %d, anonymous: %d, managers: %d]"
    (OpSet.cardinal consensus)
    (OpSet.cardinal votes)
    (OpSet.cardinal anonymous)
    (OpSet.cardinal managers)

let pp_ordered_pool fmt
    {ordered_consensus; ordered_votes; ordered_anonymous; ordered_managers} =
  Format.fprintf
    fmt
    "[consensus: %d, votes: %d, anonymous: %d, managers: %d]"
    (List.length ordered_consensus)
    (List.length ordered_votes)
    (List.length ordered_anonymous)
    (List.length ordered_managers)

(* Hypothesis : we suppose [List.length Protocol.Main.validation_passes = 4] *)
let consensus_index = 0

let votes_index = 1

let anonymous_index = 2

let managers_index = 3

let classify op =
  (* Hypothesis: acceptable passes returns a size at most 1 list  *)
  match Main.acceptable_passes op with
  | [pass] ->
      if pass = consensus_index then `Consensus
        (* TODO filter outdated consensus ops ? *)
      else if pass = votes_index then `Votes
      else if pass = anonymous_index then `Anonymous
      else if pass = managers_index then `Managers
      else `Bad
  | _ -> `Bad

let add_operation pool op =
  match classify op with
  | `Consensus ->
      let consensus = OpSet.add op pool.consensus in
      {pool with consensus}
  | `Votes ->
      let votes = OpSet.add op pool.votes in
      {pool with votes}
  | `Anonymous ->
      let anonymous = OpSet.add op pool.anonymous in
      {pool with anonymous}
  | `Managers ->
      let managers = OpSet.add op pool.managers in
      {pool with managers}
  | `Bad -> pool

let add_operations pool new_ops = List.fold_left add_operation pool new_ops

type consensus_filter = {
  level : int32;
  round : Round.t;
  payload_hash : Block_payload_hash.t;
}

(** From a pool of operations [operation_pool], the function filters
    out the endorsements that are different from the [current_level],
    the [current_round] or the optional [current_block_payload_hash],
    as well as preendorsements. *)
let filter_with_relevant_consensus_ops ~(endorsement_filter : consensus_filter)
    ~(preendorsement_filter : consensus_filter option) operation_set =
  OpSet.filter
    (fun {protocol_data; _} ->
      match (protocol_data, preendorsement_filter) with
      (* 1a. Remove preendorsements. *)
      | (Operation_data {contents = Single (Preendorsement _); _}, None) ->
          false
      (* 1b. Filter preendorsements. *)
      | ( Operation_data
            {
              contents =
                Single (Preendorsement {level; round; block_payload_hash; _});
              _;
            },
          Some
            {level = level'; round = round'; payload_hash = block_payload_hash'}
        ) ->
          Compare.Int32.(Raw_level.to_int32 level = level')
          && Round.(round = round')
          && Block_payload_hash.(block_payload_hash = block_payload_hash')
      (* 2. Filter endorsements. *)
      | ( Operation_data
            {
              contents =
                Single (Endorsement {level; round; block_payload_hash; _});
              _;
            },
          _ ) ->
          Compare.Int32.(Raw_level.to_int32 level = endorsement_filter.level)
          && Round.(round = endorsement_filter.round)
          && Block_payload_hash.(
               block_payload_hash = endorsement_filter.payload_hash)
      (* 3. Preserve all non-consensus operations. *)
      | _ -> true)
    operation_set

let unpack_preendorsement packed_preendorsement =
  let {shell; protocol_data = Operation_data data} = packed_preendorsement in
  match data with
  | {contents = Single (Preendorsement _); _} ->
      Some ({shell; protocol_data = data} : Kind.preendorsement Operation.t)
  | _ -> None

let unpack_endorsement packed_endorsement =
  let {shell; protocol_data = Operation_data data} = packed_endorsement in
  match data with
  | {contents = Single (Endorsement _); _} ->
      Some ({shell; protocol_data = data} : Kind.endorsement Operation.t)
  | _ -> None

let filter_preendorsements ops =
  List.filter_map
    (function
      | {
          shell = {branch};
          protocol_data =
            Operation_data
              ({contents = Single (Preendorsement _); _} as content);
          _;
        } ->
          Some
            ({shell = {branch}; protocol_data = content}
              : Kind.preendorsement operation)
      | _ -> None)
    ops

let filter_endorsements ops =
  List.filter_map
    (function
      | {
          shell = {branch};
          protocol_data =
            Operation_data ({contents = Single (Endorsement _); _} as content);
          _;
        } ->
          Some
            ({shell = {branch}; protocol_data = content}
              : Kind.endorsement operation)
      | _ -> None)
    ops

let pool_to_list_list {consensus; votes; anonymous; managers} =
  List.map OpSet.elements [consensus; votes; anonymous; managers]

let pool_of_list_list (ll : packed_operation list list) =
  List.fold_left add_operations empty ll

let ordered_to_list_list
    {ordered_consensus; ordered_votes; ordered_anonymous; ordered_managers} =
  [ordered_consensus; ordered_votes; ordered_anonymous; ordered_managers]

let ordered_of_list_list = function
  | [ordered_consensus; ordered_votes; ordered_anonymous; ordered_managers] ->
      Some
        {ordered_consensus; ordered_votes; ordered_anonymous; ordered_managers}
  | _ -> None

let payload_of_ordered_pool
    {ordered_votes; ordered_anonymous; ordered_managers; _} =
  {
    votes_payload = ordered_votes;
    anonymous_payload = ordered_anonymous;
    managers_payload = ordered_managers;
  }

let ordered_pool_of_payload ~consensus_operations
    {votes_payload; anonymous_payload; managers_payload} =
  {
    ordered_consensus = consensus_operations;
    ordered_votes = votes_payload;
    ordered_anonymous = anonymous_payload;
    ordered_managers = managers_payload;
  }

let extract_operations_of_list_list = function
  | [consensus; votes_payload; anonymous_payload; managers_payload] ->
      let (preendorsements, endorsements) =
        List.fold_left
          (fun ( (preendorsements : Kind.preendorsement Operation.t list),
                 (endorsements : Kind.endorsement Operation.t list) )
               packed_op ->
            let {shell; protocol_data = Operation_data data} = packed_op in
            match data with
            | {contents = Single (Preendorsement _); _} ->
                ({shell; protocol_data = data} :: preendorsements, endorsements)
            | {contents = Single (Endorsement _); _} ->
                (preendorsements, {shell; protocol_data = data} :: endorsements)
            | _ ->
                (* unreachable *)
                (preendorsements, endorsements))
          ([], [])
          consensus
        (* N.b. the order doesn't matter *)
      in
      let preendorsements =
        if preendorsements = [] then None else Some preendorsements
      in
      let payload = {votes_payload; anonymous_payload; managers_payload} in
      Some (preendorsements, endorsements, payload)
  | _ -> None
