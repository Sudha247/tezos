(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018-2021 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

(** A newly received block is validated by replaying locally the block
    creation, applying each operation and its finalization to ensure their
    consistency. This module is stateless and creates and manipulates the
    prevalidation_state. *)

type 'operation_data operation = private {
  hash : Operation_hash.t;  (** Hash of an operation. *)
  raw : Operation.t;
      (** Raw representation of an operation (from the point view of the
          shell). *)
  protocol_data : 'operation_data;
      (** Economic protocol specific data of an operation. It is the
          unserialized representation of [raw.protocol_data]. For
          convenience, the type associated to this type may be [unit] if we
          do not have deserialized the operation yet. *)
}

module type T = sig
  type operation_data

  type operation_receipt

  type validation_state

  type chain_store

  type t

  val parse : Operation.t -> operation_data operation tzresult

  (** [parse_unsafe bytes] parses [bytes] as operation data. Any error
      happening during parsing becomes {!Parse_error}.

      [unsafe] because there are no length checks, unlike {!parse}.

      @deprecated You should use [parse] instead. *)
  val parse_unsafe : bytes -> operation_data tzresult

  (** Creates a new prevalidation context w.r.t. the protocol associate to the
      predecessor block . When ?protocol_data is passed to this function, it will
      be used to create the new block *)
  val create :
    chain_store ->
    ?protocol_data:Bytes.t ->
    predecessor:Store.Block.t ->
    live_operations:Operation_hash.Set.t ->
    timestamp:Time.Protocol.t ->
    unit ->
    t tzresult Lwt.t

  type result =
    | Applied of t * operation_receipt
    | Branch_delayed of tztrace
    | Branch_refused of tztrace
    | Refused of tztrace
    | Outdated of tztrace

  val apply_operation : t -> operation_data operation -> result Lwt.t

  val validation_state : t -> validation_state

  val pp_result : Format.formatter -> result -> unit
end

(** How-to obtain an instance of this module's main module type: {!T} *)
module Make : functor (Proto : Tezos_protocol_environment.PROTOCOL) ->
  T
    with type operation_data = Proto.operation_data
     and type operation_receipt = Proto.operation_receipt
     and type validation_state = Proto.validation_state
     and type chain_store = Store.chain_store

module Internal_for_tests : sig
  (** [safe_binary_of_bytes encoding bytes] parses [bytes] using [encoding]. Any error happening during parsing becomes {!Parse_error}.

      If one day the functor signature is simplified, tests could use [parse_unsafe] directly rather than relying on this function to
      replace [Proto.operation_data_encoding].

      TODO: https://gitlab.com/tezos/tezos/-/issues/1487
      Move this function to [data_encoding] or [tezos_base] and consider not catching some exceptions
      *)
  val safe_binary_of_bytes : 'a Data_encoding.t -> bytes -> 'a tzresult

  module type CHAIN_STORE = sig
    (** The [chain_store] type. Implemented by
        {!Tezos_store.Store.chain_store} in production and mocked in
        tests *)
    type chain_store

    (** [context store block] checkouts and returns the context of [block] *)
    val context : chain_store -> Store.Block.t -> Context.t tzresult Lwt.t

    (** [chain_id store] returns the {!Chain_id.t} to which [store] corresponds *)
    val chain_id : chain_store -> Chain_id.t
  end

  (** A variant of [Make] above that is parameterized by {!CHAIN_STORE},
      for mocking purposes. *)
  module Make : functor
    (Chain_store : CHAIN_STORE)
    (Proto : Tezos_protocol_environment.PROTOCOL)
    ->
    T
      with type operation_data = Proto.operation_data
       and type operation_receipt = Proto.operation_receipt
       and type validation_state = Proto.validation_state
       and type chain_store = Chain_store.chain_store
end
