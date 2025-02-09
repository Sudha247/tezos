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

(** Launch processes to gather operations from the mempool and make them
    available for the baker. *)

open Protocol
open Alpha_context

(** {1 Datatypes}*)

type t

type candidate = {
  hash : Block_hash.t;
  round_watched : Round.t;
  payload_hash_watched : Block_payload_hash.t;
}

val candidate_encoding : candidate Data_encoding.t

type event =
  | Prequorum_reached of candidate * Kind.preendorsement operation list
  | Quorum_reached of candidate * Kind.endorsement operation list

(** {1 Constructors}*)

(** [create ?initial_mempool ?monitor_node cctxt] create a monitoring process to
   fetch operations for the baker to process.


    @param initial_mempool initial operations to put in the worker's queue
   (default: [None])

    @param monitor_node_operations monitor operations on the node (defaults: [true]).  Set
   [monitor_node] to [false] to only consider the [initial_mempool] operations.

*)
val create :
  ?initial_mempool:Baking_configuration.Mempool.t ->
  ?monitor_node_operations:bool ->
  #Protocol_client_context.full ->
  t Lwt.t

(** {2 Accessors}*)

val get_current_operations : t -> Operation_pool.pool

val get_quorum_event_stream : t -> event Lwt_stream.t

(** {3 Observers} *)

val monitor_preendorsement_quorum :
  t ->
  consensus_threshold:int ->
  get_preendorsement_voting_power:(slot:Slot.t -> int) ->
  candidate ->
  unit Lwt.t

val monitor_endorsement_quorum :
  t ->
  consensus_threshold:int ->
  get_endorsement_voting_power:(slot:Slot.t -> int) ->
  candidate ->
  unit Lwt.t

val cancel_monitoring : t -> unit

val shutdown_worker : t -> (unit, exn list) result Lwt.t
