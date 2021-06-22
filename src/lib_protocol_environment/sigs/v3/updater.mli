(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

(** Tezos Protocol Environment - Protocol updater. *)

(** Validation result: the record returned by the protocol
    on the successful validation of a block. *)
type validation_result = {
  context : Context.t;
      (** The resulting context, it will be used for the next block. *)
  fitness : Fitness.t;
      (** The effective fitness of the block (to be compared with the one
      'announced' in the block header). *)
  message : string option;
      (** An optional informative message, akin to a 'git commit' message,
      which can be attached to the [context] when it's being commited. *)
  max_operations_ttl : int;
      (** The "time-to-live" of operations for the next block: any
      operation whose 'branch' is older than 'ttl' blocks in the past
      cannot be included in the next block. *)
  last_allowed_fork_level : Int32.t;
      (** The level of the last block for which the node might consider an
      alternate branch. The shell should consider as invalid any branch
      whose fork point is older (has a lower level) than the
      given value. *)
}

type quota = {
  max_size : int;
      (** The maximum size (in bytes) of the serialized list of
      operations. *)
  max_op : int option;
      (** The maximum number of operations in a block.
      [None] means no limit. *)
}

type rpc_context = {
  block_hash : Block_hash.t;
  block_header : Block_header.shell_header;
  context : Context.t;
}

(** This is the signature of a Tezos protocol implementation. It has
    access to the standard library and the Environment module. *)
module type PROTOCOL = sig
  (** The maximum size of a block header in bytes. *)
  val max_block_length : int

  (** The maximum size of an operation in bytes. *)
  val max_operation_data_length : int

  (** Operations quota for each validation pass. The length of the
     list denotes the number of validation passes. *)
  val validation_passes : quota list

  (** The version-specific type of blocks. *)
  type block_header_data

  (** Encoding for version-specific part of block headers.  *)
  val block_header_data_encoding : block_header_data Data_encoding.t

  (** A fully parsed block header. *)
  type block_header = {
    shell : Block_header.shell_header;
    protocol_data : block_header_data;
  }

  (** Version-specific side information computed by the protocol
     during the validation of a block. Should not include information
     about the evaluation of operations which is handled separately by
     {!operation_metadata}. To be used as an execution trace by tools
     (client, indexer). Not necessary for validation. *)
  type block_header_metadata

  (** Encoding for version-specific block metadata. *)
  val block_header_metadata_encoding : block_header_metadata Data_encoding.t

  (** The version-specific type of operations. *)
  type operation_data

  (** Version-specific side information computed by the protocol
      during the validation of each operation, to be used conjointly
      with {!block_header_metadata}. *)
  type operation_receipt

  (** A fully parsed operation. *)
  type operation = {
    shell : Operation.shell_header;
    protocol_data : operation_data;
  }

  (** Encoding for version-specific operation data. *)
  val operation_data_encoding : operation_data Data_encoding.t

  (** Encoding for version-specific operation receipts. *)
  val operation_receipt_encoding : operation_receipt Data_encoding.t

  (** Encoding that mixes an operation data and its receipt. *)
  val operation_data_and_receipt_encoding :
    (operation_data * operation_receipt) Data_encoding.t

  (** [acceptable_passes op] lists the validation passes in which the
     input operation [op] can appear. For instance, it results in
     [[0]] if [op] only belongs to the first pass. An answer of [[]]
     means that the [op] is ill-formed and cannot be included at
     all in a block. *)
  val acceptable_passes : operation -> int list

  (** [relative_position_within_block op1 op2] provides a partial and
     strict order of operations within a block. It is intended to be
     used as an argument to {!List.sort} (and other sorting/ordering
     functions) to arrange a set of operations into a sequence, the
     order of which is valid for the protocol.

     A negative (respectively, positive) results means that [op1]
     should appear before (and, respectively, after) [op2] in a
     block. This function does not provide a total ordering on the
     operations: a result of [0] entails that the protocol does not
     impose any preferences to the order in which [op1] and [op2]
     should be included in a block.

     {b Caveat Emptor!} [relative_position_within_block o1 o2 = 0]
     does NOT imply that [o1] is equal to [o2] in any way.
     Consequently, it {e MUST NOT} be used as a [compare] component of
     an {!Stdlib.Map.OrderedType}, or any such collection which relies
     on a total comparison function. *)
  val relative_position_within_block : operation -> operation -> int

  (** A functional state that is transmitted through the steps of a
     block validation sequence: it can be created by any of the
     [begin_x] functions below, and its final value is produced by
     {!finalize_block}. It must retain the current state of the store
     -- which can be extracted from the outside using
     {!current_context} --, and it can also contain additional
     information that must be remembered during the validation
     process. Said extra content must however be immutable: validator
     or baker implementations are allowed to pause, replay or
     backtrack throughout validation steps. *)
  type validation_state

  (** [current_contexts vs] accesses the context at the given
     validation state [vs]. It allows for querying the context in
     between validation steps. *)
  val current_context : validation_state -> Context.t tzresult Lwt.t

  (** [begin_partial_application cid ctxt] checks that a block is
     well-formed in a given context. This function should run quickly,
     as its main use is to reject bad blocks from the chain as early
     as possible. The input [ancestor_context] is expected to result
     from the application of an ancestor block of the current head
     with the same economic protocol. Said ancestor block is also
     required to be more recent (i.e., it has a greater level), than
     the current head's "last_allowed_fork_level".

      The resulting `validation_state` will be used for multi-pass
     validation. *)
  val begin_partial_application :
    chain_id:Chain_id.t ->
    ancestor_context:Context.t ->
    predecessor_timestamp:Time.t ->
    predecessor_fitness:Fitness.t ->
    block_header ->
    validation_state tzresult Lwt.t

  (** [begin_application chain_id ... bh] defines the first step in a
     block validation sequence. It initializes a validation context
     for validating a block, whose header is [bh]. *)
  val begin_application :
    chain_id:Chain_id.t ->
    predecessor_context:Context.t ->
    predecessor_timestamp:Time.t ->
    predecessor_fitness:Fitness.t ->
    block_header ->
    validation_state tzresult Lwt.t

  (** [begin_construction] initializes a validation context for
     constructing a new block (as opposed to validating an existing
     block). When the [protocol_data] argument is specified, it should
     contain a 'prototype' of the protocol-specific part of a block
     header, and the function should produce the exact same effect on
     the context as the validation of a block containing an
     "equivalent" (but complete) header. For instance, if the block
     header usually includes a signature, the header provided to
     {!begin_construction} should include a faked signature. *)
  val begin_construction :
    chain_id:Chain_id.t ->
    predecessor_context:Context.t ->
    predecessor_timestamp:Time.t ->
    predecessor_level:Int32.t ->
    predecessor_fitness:Fitness.t ->
    predecessor:Block_hash.t ->
    timestamp:Time.t ->
    ?protocol_data:block_header_data ->
    unit ->
    validation_state tzresult Lwt.t

  (** [apply_operation vs op] applies the input operation [op] on top
     of the given {!validation_state} [vs]. It must be called after
     {!begin_application} or {!begin_construction}, and before
     {!finalize_block}, for each operation in a block. On a successful
     application, it returns a pair consisting of the resulting
     [validation_state], and the corresponding [operation_receipt]. *)
  val apply_operation :
    validation_state ->
    operation ->
    (validation_state * operation_receipt) tzresult Lwt.t

  (** [finalize_block vs] implements the last validation step in a
     block validation sequence. A successful call produces the context
     that will be used as input for the validation of its successor
     block candidates. *)
  val finalize_block :
    validation_state ->
    (validation_result * block_header_metadata) tzresult Lwt.t

  (** [rpc_services] provides the list of remote procedures exported
     by this protocol implementation. *)
  val rpc_services : rpc_context RPC_directory.t

  (** [init ctxt hd] initializes the context, or upgrades the context
     after a protocol amendment. This function receives as arguments
     the context [ctxt] resulting from the application of the block
     that triggered the amendment, as well as its header [hd]. This
     function should fail if the "protocol stitching", i.e., the
     transition from a valid previous protocol to the one being
     activated, has not been implemented. *)
  val init :
    Context.t -> Block_header.shell_header -> validation_result tzresult Lwt.t
end

(** [activate ctxt ph] activates an economic protocol version, given
   by its hash [ph], from a given context [ctxt]. This means that the
   context used for the next block will use the input protocol version
   (this is not an immediate change). The protocol must have been
   previously compiled successfully. *)
val activate : Context.t -> Protocol_hash.t -> Context.t Lwt.t
