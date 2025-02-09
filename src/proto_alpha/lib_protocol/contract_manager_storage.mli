(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

type error +=
  | (* `Branch *) Unrevealed_manager_key of Contract_repr.t
  | (* `Permanent *)
      Inconsistent_hash of
      Signature.Public_key.t
      * Signature.Public_key_hash.t
      * Signature.Public_key_hash.t
  | (* `Branch *) Previously_revealed_key of Contract_repr.t

(** [init ctxt contract manager] associates [manager] to [contract]. This
    function is undefined if [contract] has already a manager associated to it.
*)
val init :
  Raw_context.t ->
  Contract_repr.t ->
  Manager_repr.manager_key ->
  Raw_context.t tzresult Lwt.t

val is_manager_key_revealed :
  Raw_context.t -> Signature.Public_key_hash.t -> bool tzresult Lwt.t

val reveal_manager_key :
  Raw_context.t ->
  Signature.Public_key_hash.t ->
  Signature.Public_key.t ->
  Raw_context.t tzresult Lwt.t

(** [get_manager_key ctxt delegate] behaves as
    [revealed_key ctxt delegate error], where error is an
    [Unrevealed_manager_key]. However this function is undefined when the
    delegate has no manager. *)
val get_manager_key :
  Raw_context.t ->
  Signature.Public_key_hash.t ->
  Signature.Public_key.t tzresult Lwt.t

(** [revealed_key ctxt delegate error] returns the revealed public key of
    [delegate]. Fails with [error] if [delegate] does not have a manager, or if
    its key has not been revealed. *)
val revealed_key :
  Raw_context.t ->
  Signature.Public_key_hash.t ->
  error ->
  Signature.Public_key.t tzresult Lwt.t

val remove_existing :
  Raw_context.t -> Contract_repr.t -> Raw_context.t tzresult Lwt.t
