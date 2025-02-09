(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Nomadic Development. <contact@tezcore.com>             *)
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

open Protocol
open Alpha_context

(** The assumed number of blocks between operation-creation time and
    the actual time when the operation is included in a block. *)
let default_operation_inclusion_latency = 3

type Environment.Error_monad.error += Cannot_parse_operation (* `Branch *)

type Environment.Error_monad.error += Cannot_serialize_log

type Environment.Error_monad.error += Cannot_retrieve_predecessor_level

let () =
  Environment.Error_monad.register_error_kind
    `Branch
    ~id:"operation.cannot_parse"
    ~title:"Cannot parse operation"
    ~description:"The operation is ill-formed or for another protocol version"
    ~pp:(fun ppf () -> Format.fprintf ppf "The operation cannot be parsed")
    Data_encoding.unit
    (function Cannot_parse_operation -> Some () | _ -> None)
    (fun () -> Cannot_parse_operation) ;
  (* Cannot serialize log *)
  Environment.Error_monad.register_error_kind
    `Temporary
    ~id:"michelson_v1.cannot_serialize_log"
    ~title:"Not enough gas to serialize execution trace"
    ~description:
      "Execution trace with stacks was to big to be serialized with the \
       provided gas"
    Data_encoding.empty
    (function Cannot_serialize_log -> Some () | _ -> None)
    (fun () -> Cannot_serialize_log) ;
  Environment.Error_monad.register_error_kind
    `Temporary
    ~id:"cannot_retrieve_predecessor_level"
    ~title:"Cannot retrieve predecessor level"
    ~description:"Cannot retrieve predecessor level."
    Data_encoding.empty
    (function Cannot_retrieve_predecessor_level -> Some () | _ -> None)
    (fun () -> Cannot_retrieve_predecessor_level)

module Mempool = struct
  type nanotez = Q.t

  let nanotez_enc : nanotez Data_encoding.t =
    let open Data_encoding in
    def
      "nanotez"
      ~title:"A thousandth of a mutez"
      ~description:"One thousand nanotez make a mutez (1 tez = 1e9 nanotez)"
      (conv
         (fun q -> (q.Q.num, q.Q.den))
         (fun (num, den) -> {Q.num; den})
         (tup2 z z))

  type config = {
    minimal_fees : Tez.t;
    minimal_nanotez_per_gas_unit : nanotez;
    minimal_nanotez_per_byte : nanotez;
    allow_script_failure : bool;
    clock_drift : Period.t option;
  }

  let default_minimal_fees =
    match Tez.of_mutez 100L with None -> assert false | Some t -> t

  let default_minimal_nanotez_per_gas_unit = Q.of_int 100

  let default_minimal_nanotez_per_byte = Q.of_int 1000

  (* If the drift is not specified, it will be the duration of round zero.
     It allows only to spam with one future round.

     /!\ Warning /!\ : current plugin implementation implies that this drift
     cumulates with the accepted  drift regarding the current head's timestamp.
  *)

  let config_encoding : config Data_encoding.t =
    let open Data_encoding in
    conv
      (fun {
             minimal_fees;
             minimal_nanotez_per_gas_unit;
             minimal_nanotez_per_byte;
             allow_script_failure;
             clock_drift;
           } ->
        ( minimal_fees,
          minimal_nanotez_per_gas_unit,
          minimal_nanotez_per_byte,
          allow_script_failure,
          clock_drift ))
      (fun ( minimal_fees,
             minimal_nanotez_per_gas_unit,
             minimal_nanotez_per_byte,
             allow_script_failure,
             clock_drift ) ->
        {
          minimal_fees;
          minimal_nanotez_per_gas_unit;
          minimal_nanotez_per_byte;
          allow_script_failure;
          clock_drift;
        })
      (obj5
         (dft "minimal_fees" Tez.encoding default_minimal_fees)
         (dft
            "minimal_nanotez_per_gas_unit"
            nanotez_enc
            default_minimal_nanotez_per_gas_unit)
         (dft
            "minimal_nanotez_per_byte"
            nanotez_enc
            default_minimal_nanotez_per_byte)
         (dft "allow_script_failure" bool true)
         (opt "clock_drift" Period.encoding))

  let default_config =
    {
      minimal_fees = default_minimal_fees;
      minimal_nanotez_per_gas_unit = default_minimal_nanotez_per_gas_unit;
      minimal_nanotez_per_byte = default_minimal_nanotez_per_byte;
      allow_script_failure = true;
      clock_drift = None;
    }

  type state = {
    grandparent_level_start : Alpha_context.Timestamp.t option;
    round_zero_duration : Period.t option;
  }

  let init config ?(validation_state : validation_state option) ~predecessor ()
      =
    ignore config ;
    (match validation_state with
    | None ->
        return {grandparent_level_start = None; round_zero_duration = None}
    | Some {ctxt; _} ->
        let {
          Tezos_base.Block_header.fitness = predecessor_fitness;
          timestamp = predecessor_timestamp;
          _;
        } =
          predecessor.Tezos_base.Block_header.shell
        in
        Alpha_context.Fitness.predecessor_round_from_raw predecessor_fitness
        >>?= fun grandparent_round ->
        Alpha_context.Fitness.round_from_raw predecessor_fitness
        >>?= fun predecessor_round ->
        Alpha_context.(
          let round_durations = Constants.round_durations ctxt in
          let round_zero_duration =
            Round.round_duration round_durations Round.zero
          in
          Round.level_offset_of_round
            round_durations
            ~round:Round.(succ grandparent_round)
          >>?= fun proposal_level_offset ->
          Round.level_offset_of_round round_durations ~round:predecessor_round
          >>?= fun proposal_round_offset ->
          Period.(add proposal_level_offset proposal_round_offset)
          >>?= fun proposal_offset ->
          return
            {
              grandparent_level_start =
                Some Timestamp.(predecessor_timestamp - proposal_offset);
              round_zero_duration = Some round_zero_duration;
            }))
    >|= Environment.wrap_tzresult

  let on_flush config filter_state ?(validation_state : validation_state option)
      ~predecessor () =
    ignore filter_state ;
    init config ?validation_state ~predecessor ()

  let get_manager_operation_gas_and_fee contents =
    let open Operation in
    let l = to_list (Contents_list contents) in
    List.fold_left
      (fun acc -> function
        | Contents (Manager_operation {fee; gas_limit; _}) -> (
            match acc with
            | Error _ as e -> e
            | Ok (total_fee, total_gas) -> (
                match Tez.(total_fee +? fee) with
                | Ok total_fee ->
                    Ok (total_fee, Gas.Arith.add total_gas gas_limit)
                | Error _ as e -> e))
        | _ -> acc)
      (Ok (Tez.zero, Gas.Arith.zero))
      l

  type Environment.Error_monad.error += Fees_too_low

  let () =
    Environment.Error_monad.register_error_kind
      `Permanent
      ~id:"prefilter.fees_too_low"
      ~title:"Operation fees are too low"
      ~description:"Operation fees are too low"
      ~pp:(fun ppf () -> Format.fprintf ppf "Operation fees are too low")
      Data_encoding.unit
      (function Fees_too_low -> Some () | _ -> None)
      (fun () -> Fees_too_low)

  let pre_filter_manager :
      type t.
      config ->
      t Kind.manager contents_list ->
      int ->
      [ `Undecided
      | `Branch_refused of tztrace
      | `Branch_delayed of tztrace
      | `Refused of tztrace
      | `Outdated of tztrace ] =
   fun config op size ->
    match get_manager_operation_gas_and_fee op with
    | Error err ->
        let err = Environment.wrap_tztrace err in
        `Refused err
    | Ok (fee, gas) ->
        let fees_in_nanotez =
          Q.mul (Q.of_int64 (Tez.to_mutez fee)) (Q.of_int 1000)
        in
        let minimal_fees_in_nanotez =
          Q.mul (Q.of_int64 (Tez.to_mutez config.minimal_fees)) (Q.of_int 1000)
        in
        let minimal_fees_for_gas_in_nanotez =
          Q.mul
            config.minimal_nanotez_per_gas_unit
            (Q.of_bigint @@ Gas.Arith.integral_to_z gas)
        in
        let minimal_fees_for_size_in_nanotez =
          Q.mul config.minimal_nanotez_per_byte (Q.of_int size)
        in
        if
          Q.compare
            fees_in_nanotez
            (Q.add
               minimal_fees_in_nanotez
               (Q.add
                  minimal_fees_for_gas_in_nanotez
                  minimal_fees_for_size_in_nanotez))
          >= 0
        then `Undecided
        else `Refused [Environment.wrap_tzerror Fees_too_low]

  type Environment.Error_monad.error += Outdated_endorsement

  let () =
    Environment.Error_monad.register_error_kind
      `Temporary
      ~id:"prefilter.outdated_endorsement"
      ~title:"Endorsement is outdated"
      ~description:"Endorsement is outdated"
      ~pp:(fun ppf () -> Format.fprintf ppf "Endorsement is outdated")
      Data_encoding.unit
      (function Outdated_endorsement -> Some () | _ -> None)
      (fun () -> Outdated_endorsement)

  type Environment.Error_monad.error += Wrong_operation

  let () =
    Environment.Error_monad.register_error_kind
      `Temporary
      ~id:"prefilter.wrong_operation"
      ~title:"Wrong operation"
      ~description:"Failing_noop and old endorsement format are not accepted."
      ~pp:(fun ppf () ->
        Format.fprintf
          ppf
          "Failing_noop and old endorsement format are not accepted")
      Data_encoding.unit
      (function Wrong_operation -> Some () | _ -> None)
      (fun () -> Wrong_operation)

  type Environment.Error_monad.error += Consensus_operation_in_far_future

  let () =
    Environment.Error_monad.register_error_kind
      `Branch
      ~id:"prefilter.Consensus_operation_in_far_future"
      ~title:"Consensus operation in far future"
      ~description:"Consensus operation too far in the future are not accepted."
      ~pp:(fun ppf () ->
        Format.fprintf
          ppf
          "Consensus operation too far in the future are not accepted.")
      Data_encoding.unit
      (function Consensus_operation_in_far_future -> Some () | _ -> None)
      (fun () -> Consensus_operation_in_far_future)

  (** {2} consensus operation filtering.

     In Tenderbake, we increased a lot the number of consensus
      operations, therefore it seems necessary to be able to filter consensus
     operations that could be produced by a Byzantine baker mis-using
     its right to produce operations in future rounds or levels.

      We consider the situation where the head is at level [h_l],
     round [h_r], and with timestamp [h_ts], with the predecessor of the head
     being at round [hp_r].
      We receive at a time [now] a consensus operation for level [op_l] and
     round [op_r].

       A consensus operation is considered too far in the future, and therefore filtered,
      if the earliest possible starting time of its round is greater than the
      current time plus a safety margin of [config.clock_drift].

      To consider potential level 2 reorgs, we first compute the expected
      timestamp of round zero at previous level [hp0_ts],

      All ops at level p_l and round r' such that time(r') is greater than (now + drift) are
     deemed too far in the future:

                  h_r                          op_ts    now+drift     (h_l,r')
     hp0_ts h_0   h_l                            |        |              |
        +----+-----+---------+-------------------+--+-----+--------------+-----------
             |     |         |                   |  |     |              |
             |    h_ts     h_r end time          | now    |        earliest expected
             |     |                             |        |        time of round r'
             |<----op_r rounds duration -------->|        |
                   |
                   |<--------------- operations kept ---->|<-rejected----------...
                   |
                   |<-----------operations considered by the filter -----------...

    For an operation on a proposal at the next level, we consider the minimum
    starting time of the operation's round, obtained by assuming that the proposal
    at the next level was built on top of a proposal at round 0 for the current
    level, itself based on a proposal at round 0 of previous level.
    Operations on proposal with higher levels are treated similarly.

    All ops at the next level and round r' such that timestamp(r') > now+drift
    are deemed too far in the future.

                r=0     r=1   h_r      now     now+drift   (h_l+1,r')
   hp0_ts h_0   h_l           h_l       |          |          |
      +----+---- |-------+----+---------+----------+----------+----------
           |     |       |    |                               |
           |     t0      |   h_ts                      earliest expected
           |     |       |    |                         time of round r'
           |<--- |    earliest|                               |
                 |  next level|                               |
                 |       |<---------------------------------->|
                                  round_offset(r')

  *)

  (** At a given level a consensus operation is acceptable if its earliest
      expected timestamp, [op_earliest_ts] is below the current clock with an
      accepted drift for the clock given by a configuration.  *)
  let acceptable ~drift ~op_earliest_ts ~now_timestamp =
    Timestamp.(
      now_timestamp +? drift >|? fun now_drifted ->
      op_earliest_ts <= now_drifted)

  (** Check that an operation with the given [op_round], at level [op_level]
      is likely to be correct, meaning it could have been produced before
      now (+ the safety margin from configuration).

      Given an operation at level greater or equal than/to the current level, we
      compute the expected timestamp of the operation's round. If the operation
      is at a greater level, we assume that it is based on the proposal at round
      zero of the current level.

      All operations whose (level, round) is lower than or equal to the current
      head are deemed valid.
      Note that in case where their is a high drift in the computer clock, they
      might not have been considered valid by comparing their expected timestamp
      to the clock.

      This is a stricter than necessary filter as it will reject operations that
      could be valid in the current timeframe if the proposal they endorse is
      built over a predecessor of the current proposal that would be of lower
      round than the current one.

      What can we do that would be smarter: get current head's predecessor round
      and timestamp to compute the timestamp t0 of a predecessor that would have
      been proposed at round 0.

      Timestamp of round at current level for an alternative head that would be
      based on such proposal would be computed based on t0.
      For level higher than current head, compute the round's earliest timestamp
      if all proposal passed at round 0 starting from t0.
  *)
  let acceptable_op ~config ~round_durations ~round_zero_duration
      ~proposal_level ~proposal_round ~proposal_timestamp
      ~(proposal_predecessor_level_start : Timestamp.t) ~op_level ~op_round
      ~now_timestamp =
    if
      Raw_level.(succ op_level < proposal_level)
      || (op_level = proposal_level && op_round <= proposal_round)
    then
      (* Past and current round operations are not in the future *)
      (* This case could be handled directly in `pre_filter_far_future_consensus_ops`
         for a (slightly) better performance. *)
      Ok true
    else
      (* If, by some tolerance on local clock drift, the timestamp of the
         current head is itself in the future, we use this time instead of
         now_timestamp *)
      let now_timestamp = Timestamp.(max now_timestamp proposal_timestamp) in
      (* Computing when the current level started. *)
      let drift =
        Option.value ~default:round_zero_duration config.clock_drift
      in
      (* We compute the earliest timestamp possible [op_earliest_ts] for the
         operation's (level,round), as if all proposals were accepted at round 0
         since the previous level. *)
      (* Invariant: [op_level + 1 >= proposal_level] *)
      let level_offset = Raw_level.(diff (succ op_level) proposal_level) in
      Period.mult level_offset round_zero_duration >>? fun time_shift ->
      Timestamp.(proposal_predecessor_level_start +? time_shift)
      >>? fun earliest_op_level_start ->
      (* computing the operations's round start from it's earliest
         possible level start *)
      Round.timestamp_of_another_round_same_level
        round_durations
        ~current_round:Round.zero
        ~current_timestamp:earliest_op_level_start
        ~considered_round:op_round
      >>? fun op_earliest_ts ->
      (* We finally check that the expected time of the operation is
         acceptable *)
      acceptable ~drift ~op_earliest_ts ~now_timestamp

  let pre_filter_far_future_consensus_ops config
      ~filter_state:({grandparent_level_start; round_zero_duration} : state)
      ?validation_state_before
      ({level = op_level; round = op_round; _} : consensus_content) : bool Lwt.t
      =
    match
      (grandparent_level_start, validation_state_before, round_zero_duration)
    with
    | (None, _, _) | (_, None, _) | (_, _, None) -> Lwt.return_true
    | ( Some grandparent_level_start,
        Some validation_state_before,
        Some round_zero_duration ) -> (
        let ctxt : t = validation_state_before.ctxt in
        match validation_state_before.mode with
        | Application _ | Partial_application _ | Full_construction _ ->
            assert false
        (* Prefilter is always applied in mempool mode aka Partial_construction *)
        | Partial_construction {predecessor_round = proposal_round; _} -> (
            (let proposal_timestamp =
               Alpha_context.Timestamp.predecessor ctxt
             in
             let now_timestamp = Systime_os.now () |> Time.System.to_protocol in
             let Level.{level; _} = Alpha_context.Level.current ctxt in
             let proposal_level =
               match Raw_level.pred level with
               | None ->
                   (* mempool level is set to the successor of the
                      current head *)
                   assert false
               | Some proposal_level -> proposal_level
             in
             let round_durations = Constants.round_durations ctxt in
             Lwt.return
             @@ acceptable_op
                  ~config
                  ~round_durations
                  ~round_zero_duration
                  ~proposal_level
                  ~proposal_round
                  ~proposal_timestamp
                  ~proposal_predecessor_level_start:grandparent_level_start
                  ~op_level
                  ~op_round
                  ~now_timestamp)
            >>= function
            | Ok b -> Lwt.return b
            | _ -> Lwt.return_false))

  (** A quasi infinite amount of "valid" (pre)endorsements could be
      sent by a committee member, one for each possible round number.

      This filter rejects (pre)endorsements that refer to a round
      that could not have been reached within the time span between
      the last head's timestamp and the current local clock.

      We add [config.clock_drift] time as a safety margin.
  *)
  let pre_filter config ~(filter_state : state) ?validation_state_before
      (Operation_data {contents; _} as op : Operation.packed_protocol_data) =
    let bytes =
      (WithExceptions.Option.get ~loc:__LOC__
      @@ Data_encoding.Binary.fixed_length
           Tezos_base.Operation.shell_header_encoding)
      + Data_encoding.Binary.length Operation.protocol_data_encoding op
    in
    (match contents with
    | Single (Failing_noop _) ->
        Lwt.return @@ `Refused [Environment.wrap_tzerror Wrong_operation]
    | Single (Preendorsement consensus_content)
    | Single (Endorsement consensus_content) ->
        pre_filter_far_future_consensus_ops
          ~filter_state
          config
          ?validation_state_before
          consensus_content
        >>= fun keep ->
        if keep then Lwt.return `Undecided
        else
          Lwt.return
          @@ `Branch_refused
               [Environment.wrap_tzerror Consensus_operation_in_far_future]
    | Single (Seed_nonce_revelation _)
    | Single (Double_preendorsement_evidence _)
    | Single (Double_endorsement_evidence _)
    | Single (Double_baking_evidence _)
    | Single (Activate_account _)
    | Single (Proposals _)
    | Single (Ballot _) ->
        Lwt.return @@ `Undecided
    | Single (Manager_operation _) as op ->
        Lwt.return @@ pre_filter_manager config op bytes
    | Cons (Manager_operation _, _) as op ->
        Lwt.return @@ pre_filter_manager config op bytes)
    >>= fun res -> Lwt.return (res, filter_state)

  open Apply_results

  let rec post_filter_manager :
      type t.
      Alpha_context.t ->
      t Kind.manager contents_result_list ->
      config ->
      bool Lwt.t =
   fun ctxt op config ->
    match op with
    | Single_result (Manager_operation_result {operation_result; _}) -> (
        match operation_result with
        | Applied _ -> Lwt.return_true
        | Skipped _ | Failed _ | Backtracked _ ->
            Lwt.return config.allow_script_failure)
    | Cons_result (Manager_operation_result res, rest) -> (
        post_filter_manager
          ctxt
          (Single_result (Manager_operation_result res))
          config
        >>= function
        | false -> Lwt.return_false
        | true -> post_filter_manager ctxt rest config)

  let post_filter config ~(filter_state : state) ~validation_state_before:_
      ~validation_state_after:({ctxt; _} : validation_state) (_op, receipt) =
    (match receipt with
    | No_operation_metadata -> assert false (* only for multipass validator *)
    | Operation_metadata {contents} -> (
        match contents with
        | Single_result (Preendorsement_result _) -> Lwt.return_true
        | Single_result (Endorsement_result _) -> Lwt.return_true
        | Single_result (Seed_nonce_revelation_result _) -> Lwt.return_true
        | Single_result (Double_preendorsement_evidence_result _) ->
            Lwt.return_true
        | Single_result (Double_endorsement_evidence_result _) ->
            Lwt.return_true
        | Single_result (Double_baking_evidence_result _) -> Lwt.return_true
        | Single_result (Activate_account_result _) -> Lwt.return_true
        | Single_result Proposals_result -> Lwt.return_true
        | Single_result Ballot_result -> Lwt.return_true
        | Single_result (Manager_operation_result _) as op ->
            post_filter_manager ctxt op config
        | Cons_result (Manager_operation_result _, _) as op ->
            post_filter_manager ctxt op config))
    >>= fun res -> Lwt.return (res, filter_state)
end

module View_helpers = struct
  open Tezos_micheline

  type Environment.Error_monad.error += Viewed_contract_has_no_script

  type Environment.Error_monad.error += View_callback_origination_failed

  type Environment.Error_monad.error +=
    | Illformed_view_type of string * Script.expr

  type Environment.Error_monad.error +=
    | View_never_returns of string * Contract.t

  type Environment.Error_monad.error +=
    | View_unexpected_return of string * Contract.t

  let () =
    Environment.Error_monad.register_error_kind
      `Permanent
      ~id:"viewedContractHasNoScript"
      ~title:"Viewed contract has no script"
      ~description:"A view was called on a contract with no script."
      ~pp:(fun ppf () ->
        Format.fprintf ppf "A view was called on a contract with no script.")
      Data_encoding.(unit)
      (function Viewed_contract_has_no_script -> Some () | _ -> None)
      (fun () -> Viewed_contract_has_no_script) ;
    Environment.Error_monad.register_error_kind
      `Permanent
      ~id:"viewCallbackOriginationFailed"
      ~title:"View callback origination failed"
      ~description:"View callback origination failed"
      ~pp:(fun ppf () ->
        Format.fprintf ppf "Error during origination of view callback contract.")
      Data_encoding.(unit)
      (function View_callback_origination_failed -> Some () | _ -> None)
      (fun () -> View_callback_origination_failed) ;
    Environment.Error_monad.register_error_kind
      `Permanent
      ~id:"illformedViewType"
      ~title:"An entrypoint type is incompatible with TZIP-4 view type."
      ~description:"An entrypoint type is incompatible with TZIP-4 view type."
      ~pp:(fun ppf (entrypoint, typ) ->
        Format.fprintf
          ppf
          "The view %s has type %a, it is not compatible with a TZIP-4 view \
           type."
          entrypoint
          Micheline_printer.print_expr
          (Micheline_printer.printable
             (fun x -> x)
             (Michelson_v1_primitives.strings_of_prims typ)))
      Data_encoding.(
        obj2 (req "entrypoint" string) (req "type" Script.expr_encoding))
      (function Illformed_view_type (etp, exp) -> Some (etp, exp) | _ -> None)
      (fun (etp, exp) -> Illformed_view_type (etp, exp)) ;
    Environment.Error_monad.register_error_kind
      `Permanent
      ~id:"viewNeverReturns"
      ~title:
        "A view never returned a transaction to the given callback contract"
      ~description:
        "A view never initiated a transaction to the given callback contract."
      ~pp:(fun ppf (entrypoint, callback) ->
        Format.fprintf
          ppf
          "The view %s never initiated a transaction to the given callback \
           contract %a."
          entrypoint
          Contract.pp
          callback)
      Data_encoding.(
        obj2 (req "entrypoint" string) (req "callback" Contract.encoding))
      (function View_never_returns (e, c) -> Some (e, c) | _ -> None)
      (fun (e, c) -> View_never_returns (e, c)) ;
    Environment.Error_monad.register_error_kind
      `Permanent
      ~id:"viewUnexpectedReturn"
      ~title:"A view returned an unexpected list of operations"
      ~description:
        "A view initiated a list of operations while the TZIP-4 standard \
         expects only a transaction to the given callback contract."
      ~pp:(fun ppf (entrypoint, callback) ->
        Format.fprintf
          ppf
          "The view %s initiated a list of operations while the TZIP-4 \
           standard expects only a transaction to the given callback contract \
           %a."
          entrypoint
          Contract.pp
          callback)
      Data_encoding.(
        obj2 (req "entrypoint" string) (req "callback" Contract.encoding))
      (function View_never_returns (e, c) -> Some (e, c) | _ -> None)
      (fun (e, c) -> View_never_returns (e, c))

  (* This script is actually never run, its usage is to ensure a
     contract that has the type `contract <ty>` is originated, which
     will be required as callback of the view. *)
  let make_viewer_script ty : Script.t =
    let loc = 0 in
    let ty = Micheline.root ty in
    let code =
      Micheline.strip_locations
      @@ Micheline.Seq
           ( loc,
             [
               Micheline.Prim (loc, Script.K_parameter, [ty], []);
               Micheline.Prim
                 ( loc,
                   Script.K_storage,
                   [Micheline.Prim (loc, Script.T_unit, [], [])],
                   [] );
               Micheline.Prim
                 ( loc,
                   Script.K_code,
                   [Micheline.Prim (loc, Script.I_FAILWITH, [], [])],
                   [] );
             ] )
    in
    let storage =
      Micheline.strip_locations (Micheline.Prim (loc, Script.D_Unit, [], []))
    in
    {code = Script.lazy_expr code; storage = Script.lazy_expr storage}

  let make_view_parameter input callback =
    let loc = 0 in
    Micheline.strip_locations
      (Micheline.Prim
         ( loc,
           Script.D_Pair,
           [
             input;
             Micheline.Bytes
               ( loc,
                 Data_encoding.Binary.to_bytes_exn Contract.encoding callback );
           ],
           [] ))

  let extract_view_output_type entrypoint ty =
    match Micheline.root ty with
    | Micheline.Prim
        ( _,
          Script.T_pair,
          [_; Micheline.Prim (_, Script.T_contract, [ty], _)],
          _ ) ->
        ok (Micheline.strip_locations ty)
    | _ -> Environment.Error_monad.error (Illformed_view_type (entrypoint, ty))

  (* 'view' entrypoints returns their value by calling a callback contract, thus
     the expected result is a unique internal transaction to this callback. *)
  let extract_parameter_from_operations entrypoint operations callback =
    let unexpected_return =
      Environment.Error_monad.error
      @@ View_unexpected_return (entrypoint, callback)
    in
    match operations with
    | [
     Internal_operation
       {operation = Transaction {destination; parameters; _}; _};
    ]
      when Contract.equal destination callback ->
        ok parameters
    | [] ->
        Environment.Error_monad.error
          (View_never_returns (entrypoint, callback))
    | _ -> unexpected_return
end

module RPC = struct
  open Environment
  open Alpha_context
  open Environment.Error_monad

  let parse_operation (op : Operation.raw) =
    match
      Data_encoding.Binary.of_bytes_opt
        Operation.protocol_data_encoding
        op.proto
    with
    | Some protocol_data -> ok {shell = op.shell; protocol_data}
    | None -> error Cannot_parse_operation

  let path = RPC_path.(open_root / "helpers")

  module Registration = struct
    let patched_services =
      ref (RPC_directory.empty : Updater.rpc_context RPC_directory.t)

    let register0_fullctxt ~chunked s f =
      patched_services :=
        RPC_directory.register ~chunked !patched_services s (fun ctxt q i ->
            Services_registration.rpc_init ctxt >>=? fun ctxt -> f ctxt q i)

    let register0 ~chunked s f =
      register0_fullctxt ~chunked s (fun {context; _} -> f context)

    let register0_noctxt ~chunked s f =
      patched_services :=
        RPC_directory.register ~chunked !patched_services s (fun _ q i -> f q i)

    let opt_register0_fullctxt ~chunked s f =
      patched_services :=
        RPC_directory.opt_register ~chunked !patched_services s (fun ctxt q i ->
            Services_registration.rpc_init ctxt >>=? fun ctxt -> f ctxt q i)

    let opt_register0 ~chunked s f =
      opt_register0_fullctxt ~chunked s (fun {context; _} -> f context)

    let register1_fullctxt ~chunked s f =
      patched_services :=
        RPC_directory.register
          ~chunked
          !patched_services
          s
          (fun (ctxt, arg) q i ->
            Services_registration.rpc_init ctxt >>=? fun ctxt -> f ctxt arg q i)

    let register1 ~chunked s f =
      register1_fullctxt ~chunked s (fun {context; _} x -> f context x)

    let register2_fullctxt ~chunked s f =
      patched_services :=
        RPC_directory.register
          ~chunked
          !patched_services
          s
          (fun ((ctxt, arg1), arg2) q i ->
            Services_registration.rpc_init ctxt >>=? fun ctxt ->
            f ctxt arg1 arg2 q i)

    let register2 ~chunked s f =
      register2_fullctxt ~chunked s (fun {context; _} a1 a2 q i ->
          f context a1 a2 q i)
  end

  let unparsing_mode_encoding =
    let open Script_ir_translator in
    let open Data_encoding in
    union
      ~tag_size:`Uint8
      [
        case
          (Tag 0)
          ~title:"Readable"
          (constant "Readable")
          (function
            | Readable -> Some () | Optimized | Optimized_legacy -> None)
          (fun () -> Readable);
        case
          (Tag 1)
          ~title:"Optimized"
          (constant "Optimized")
          (function
            | Optimized -> Some () | Readable | Optimized_legacy -> None)
          (fun () -> Optimized);
        case
          (Tag 2)
          ~title:"Optimized_legacy"
          (constant "Optimized_legacy")
          (function
            | Optimized_legacy -> Some () | Readable | Optimized -> None)
          (fun () -> Optimized_legacy);
      ]

  module Scripts = struct
    module S = struct
      open Data_encoding

      let path = RPC_path.(path / "scripts")

      let run_code_input_encoding =
        merge_objs
          (obj10
             (req "script" Script.expr_encoding)
             (req "storage" Script.expr_encoding)
             (req "input" Script.expr_encoding)
             (req "amount" Tez.encoding)
             (req "balance" Tez.encoding)
             (req "chain_id" Chain_id.encoding)
             (opt "source" Contract.encoding)
             (opt "payer" Contract.encoding)
             (opt "gas" Gas.Arith.z_integral_encoding)
             (dft "entrypoint" string "default"))
          (obj1 (opt "unparsing_mode" unparsing_mode_encoding))

      let run_code_output_encoding =
        conv
          (fun (storage, operations, lazy_storage_diff) ->
            (storage, operations, lazy_storage_diff, lazy_storage_diff))
          (fun (storage, operations, legacy_lazy_storage_diff, lazy_storage_diff)
               ->
            let lazy_storage_diff =
              Option.either lazy_storage_diff legacy_lazy_storage_diff
            in
            (storage, operations, lazy_storage_diff))
          (obj4
             (req "storage" Script.expr_encoding)
             (req "operations" (list Operation.internal_operation_encoding))
             (opt "big_map_diff" Lazy_storage.legacy_big_map_diff_encoding)
             (opt "lazy_storage_diff" Lazy_storage.encoding))

      let trace_code_input_encoding = run_code_input_encoding

      let trace_encoding =
        def "scripted.trace" @@ list
        @@ obj3
             (req "location" Script.location_encoding)
             (req "gas" Gas.encoding)
             (req
                "stack"
                (list
                   (obj2 (req "item" Script.expr_encoding) (opt "annot" string))))

      let trace_code_output_encoding =
        conv
          (fun (storage, operations, trace, lazy_storage_diff) ->
            (storage, operations, trace, lazy_storage_diff, lazy_storage_diff))
          (fun ( storage,
                 operations,
                 trace,
                 legacy_lazy_storage_diff,
                 lazy_storage_diff ) ->
            let lazy_storage_diff =
              Option.either lazy_storage_diff legacy_lazy_storage_diff
            in
            (storage, operations, trace, lazy_storage_diff))
          (obj5
             (req "storage" Script.expr_encoding)
             (req "operations" (list Operation.internal_operation_encoding))
             (req "trace" trace_encoding)
             (opt "big_map_diff" Lazy_storage.legacy_big_map_diff_encoding)
             (opt "lazy_storage_diff" Lazy_storage.encoding))

      let run_view_encoding =
        let open Data_encoding in
        obj8
          (req "contract" Contract.encoding)
          (req "entrypoint" string)
          (req "input" Script.expr_encoding)
          (req "chain_id" Chain_id.encoding)
          (opt "source" Contract.encoding)
          (opt "payer" Contract.encoding)
          (opt "gas" Gas.Arith.z_integral_encoding)
          (req "unparsing_mode" unparsing_mode_encoding)

      let run_code =
        RPC_service.post_service
          ~description:"Run a piece of code in the current context"
          ~query:RPC_query.empty
          ~input:run_code_input_encoding
          ~output:run_code_output_encoding
          RPC_path.(path / "run_code")

      let trace_code =
        RPC_service.post_service
          ~description:
            "Run a piece of code in the current context, keeping a trace"
          ~query:RPC_query.empty
          ~input:trace_code_input_encoding
          ~output:trace_code_output_encoding
          RPC_path.(path / "trace_code")

      let run_view =
        RPC_service.post_service
          ~description:
            "Simulate a call to a view following the TZIP-4 standard. See \
             https://gitlab.com/tzip/tzip/-/blob/master/proposals/tzip-4/tzip-4.md#view-entrypoints."
          ~input:run_view_encoding
          ~output:(obj1 (req "data" Script.expr_encoding))
          ~query:RPC_query.empty
          RPC_path.(path / "run_view")

      let typecheck_code =
        RPC_service.post_service
          ~description:"Typecheck a piece of code in the current context"
          ~query:RPC_query.empty
          ~input:
            (obj3
               (req "program" Script.expr_encoding)
               (opt "gas" Gas.Arith.z_integral_encoding)
               (opt "legacy" bool))
          ~output:
            (obj2
               (req "type_map" Script_tc_errors_registration.type_map_enc)
               (req "gas" Gas.encoding))
          RPC_path.(path / "typecheck_code")

      let script_size =
        RPC_service.post_service
          ~description:"Compute the size of a script in the current context"
          ~query:RPC_query.empty
          ~input:
            (obj4
               (req "program" Script.expr_encoding)
               (req "storage" Script.expr_encoding)
               (opt "gas" Gas.Arith.z_integral_encoding)
               (opt "legacy" bool))
          ~output:(obj1 (req "script_size" int31))
          RPC_path.(path / "script_size")

      let typecheck_data =
        RPC_service.post_service
          ~description:
            "Check that some data expression is well formed and of a given \
             type in the current context"
          ~query:RPC_query.empty
          ~input:
            (obj4
               (req "data" Script.expr_encoding)
               (req "type" Script.expr_encoding)
               (opt "gas" Gas.Arith.z_integral_encoding)
               (opt "legacy" bool))
          ~output:(obj1 (req "gas" Gas.encoding))
          RPC_path.(path / "typecheck_data")

      let pack_data =
        RPC_service.post_service
          ~description:
            "Computes the serialized version of some data expression using the \
             same algorithm as script instruction PACK"
          ~input:
            (obj3
               (req "data" Script.expr_encoding)
               (req "type" Script.expr_encoding)
               (opt "gas" Gas.Arith.z_integral_encoding))
          ~output:(obj2 (req "packed" bytes) (req "gas" Gas.encoding))
          ~query:RPC_query.empty
          RPC_path.(path / "pack_data")

      let normalize_data =
        RPC_service.post_service
          ~description:
            "Normalizes some data expression using the requested unparsing mode"
          ~input:
            (obj4
               (req "data" Script.expr_encoding)
               (req "type" Script.expr_encoding)
               (req "unparsing_mode" unparsing_mode_encoding)
               (opt "legacy" bool))
          ~output:(obj1 (req "normalized" Script.expr_encoding))
          ~query:RPC_query.empty
          RPC_path.(path / "normalize_data")

      let normalize_script =
        RPC_service.post_service
          ~description:
            "Normalizes a Michelson script using the requested unparsing mode"
          ~input:
            (obj2
               (req "script" Script.expr_encoding)
               (req "unparsing_mode" unparsing_mode_encoding))
          ~output:(obj1 (req "normalized" Script.expr_encoding))
          ~query:RPC_query.empty
          RPC_path.(path / "normalize_script")

      let normalize_type =
        RPC_service.post_service
          ~description:
            "Normalizes some Michelson type by expanding `pair a b c` as `pair \
             a (pair b c)"
          ~input:(obj1 (req "type" Script.expr_encoding))
          ~output:(obj1 (req "normalized" Script.expr_encoding))
          ~query:RPC_query.empty
          RPC_path.(path / "normalize_type")

      let run_operation =
        RPC_service.post_service
          ~description:"Run an operation without signature checks"
          ~query:RPC_query.empty
          ~input:
            (obj2
               (req "operation" Operation.encoding)
               (req "chain_id" Chain_id.encoding))
          ~output:Apply_results.operation_data_and_metadata_encoding
          RPC_path.(path / "run_operation")

      let simulate_operation =
        RPC_service.post_service
          ~description:"Simulate an operation"
          ~query:RPC_query.empty
          ~input:
            (obj3
               (req "operation" Operation.encoding)
               (req "chain_id" Chain_id.encoding)
               (dft "latency" int16 default_operation_inclusion_latency))
          ~output:Apply_results.operation_data_and_metadata_encoding
          RPC_path.(path / "simulate_operation")

      let entrypoint_type =
        RPC_service.post_service
          ~description:"Return the type of the given entrypoint"
          ~query:RPC_query.empty
          ~input:
            (obj2
               (req "script" Script.expr_encoding)
               (dft "entrypoint" string "default"))
          ~output:(obj1 (req "entrypoint_type" Script.expr_encoding))
          RPC_path.(path / "entrypoint")

      let list_entrypoints =
        RPC_service.post_service
          ~description:"Return the list of entrypoints of the given script"
          ~query:RPC_query.empty
          ~input:(obj1 (req "script" Script.expr_encoding))
          ~output:
            (obj2
               (dft
                  "unreachable"
                  (Data_encoding.list
                     (obj1
                        (req
                           "path"
                           (Data_encoding.list
                              Michelson_v1_primitives.prim_encoding))))
                  [])
               (req "entrypoints" (assoc Script.expr_encoding)))
          RPC_path.(path / "entrypoints")
    end

    module type UNPARSING_MODE = sig
      val unparsing_mode : Script_ir_translator.unparsing_mode
    end

    module Traced_interpreter (Unparsing_mode : UNPARSING_MODE) = struct
      type log_element =
        | Log :
            context
            * Script.location
            * ('a * 's)
            * ('a, 's) Script_typed_ir.stack_ty
            -> log_element

      let unparse_stack ctxt (stack, stack_ty) =
        (* We drop the gas limit as this function is only used for debugging/errors. *)
        let ctxt = Gas.set_unlimited ctxt in
        let rec unparse_stack :
            type a s.
            (a, s) Script_typed_ir.stack_ty * (a * s) ->
            (Script.expr * string option) list tzresult Lwt.t = function
          | (Bot_t, (EmptyCell, EmptyCell)) -> return_nil
          | (Item_t (ty, rest_ty, annot), (v, rest)) ->
              Script_ir_translator.unparse_data
                ctxt
                Unparsing_mode.unparsing_mode
                ty
                v
              >>=? fun (data, _ctxt) ->
              unparse_stack (rest_ty, rest) >|=? fun rest ->
              let annot =
                match Script_ir_annot.unparse_var_annot annot with
                | [] -> None
                | [a] -> Some a
                | _ -> assert false
              in
              let data = Micheline.strip_locations data in
              (data, annot) :: rest
        in
        unparse_stack (stack_ty, stack)

      let trace_logger () : Script_typed_ir.logger =
        let log : log_element list ref = ref [] in
        let log_interp _ ctxt loc sty stack =
          log := Log (ctxt, loc, stack, sty) :: !log
        in
        let log_entry _ _ctxt _loc _sty _stack = () in
        let log_exit _ ctxt loc sty stack =
          log := Log (ctxt, loc, stack, sty) :: !log
        in
        let log_control _ = () in
        let get_log () =
          List.map_es
            (fun (Log (ctxt, loc, stack, stack_ty)) ->
              trace Cannot_serialize_log (unparse_stack ctxt (stack, stack_ty))
              >>=? fun stack -> return (loc, Gas.level ctxt, stack))
            !log
          >>=? fun res -> return (Some (List.rev res))
        in
        {log_exit; log_entry; log_interp; get_log; log_control}

      let execute ctxt step_constants ~script ~entrypoint ~parameter =
        let open Script_interpreter in
        let logger = trace_logger () in
        execute
          ~logger
          ~cached_script:None
          ctxt
          Unparsing_mode.unparsing_mode
          step_constants
          ~script
          ~entrypoint
          ~parameter
          ~internal:true
        >>=? fun ({ctxt; storage; lazy_storage_diff; operations}, _) ->
        logger.get_log () >|=? fun trace ->
        let trace = Option.value ~default:[] trace in
        ({ctxt; storage; lazy_storage_diff; operations}, trace)
    end

    let typecheck_data :
        legacy:bool ->
        context ->
        Script.expr * Script.expr ->
        context tzresult Lwt.t =
     fun ~legacy ctxt (data, exp_ty) ->
      record_trace
        (Script_tc_errors.Ill_formed_type (None, exp_ty, 0))
        (Script_ir_translator.parse_parameter_ty
           ctxt
           ~legacy
           (Micheline.root exp_ty))
      >>?= fun (Ex_ty exp_ty, ctxt) ->
      trace_eval
        (fun () ->
          let exp_ty = Script_ir_translator.serialize_ty_for_error exp_ty in
          return @@ Script_tc_errors.Ill_typed_data (None, data, exp_ty))
        (let allow_forged =
           true
           (* Safe since we ignore the value afterwards. *)
         in
         Script_ir_translator.parse_data
           ctxt
           ~legacy
           ~allow_forged
           exp_ty
           (Micheline.root data))
      >|=? fun (_, ctxt) -> ctxt

    module Unparse_types = struct
      (* Same as the unparsing functions for types in Script_ir_translator but
         does not consume gas and never folds (pair a (pair b c)) *)

      open Script_ir_translator
      open Micheline
      open Michelson_v1_primitives
      open Script_ir_annot
      open Script_typed_ir

      let rec unparse_comparable_ty : type a. a comparable_ty -> Script.node =
        function
        | Unit_key meta -> Prim (-1, T_unit, [], unparse_type_annot meta.annot)
        | Never_key meta -> Prim (-1, T_never, [], unparse_type_annot meta.annot)
        | Int_key meta -> Prim (-1, T_int, [], unparse_type_annot meta.annot)
        | Nat_key meta -> Prim (-1, T_nat, [], unparse_type_annot meta.annot)
        | Signature_key meta ->
            Prim (-1, T_signature, [], unparse_type_annot meta.annot)
        | String_key meta ->
            Prim (-1, T_string, [], unparse_type_annot meta.annot)
        | Bytes_key meta -> Prim (-1, T_bytes, [], unparse_type_annot meta.annot)
        | Mutez_key meta -> Prim (-1, T_mutez, [], unparse_type_annot meta.annot)
        | Bool_key meta -> Prim (-1, T_bool, [], unparse_type_annot meta.annot)
        | Key_hash_key meta ->
            Prim (-1, T_key_hash, [], unparse_type_annot meta.annot)
        | Key_key meta -> Prim (-1, T_key, [], unparse_type_annot meta.annot)
        | Timestamp_key meta ->
            Prim (-1, T_timestamp, [], unparse_type_annot meta.annot)
        | Address_key meta ->
            Prim (-1, T_address, [], unparse_type_annot meta.annot)
        | Chain_id_key meta ->
            Prim (-1, T_chain_id, [], unparse_type_annot meta.annot)
        | Pair_key ((l, al), (r, ar), meta) ->
            let tl = add_field_annot al None (unparse_comparable_ty l) in
            let tr = add_field_annot ar None (unparse_comparable_ty r) in
            Prim (-1, T_pair, [tl; tr], unparse_type_annot meta.annot)
        | Union_key ((l, al), (r, ar), meta) ->
            let tl = add_field_annot al None (unparse_comparable_ty l) in
            let tr = add_field_annot ar None (unparse_comparable_ty r) in
            Prim (-1, T_or, [tl; tr], unparse_type_annot meta.annot)
        | Option_key (t, meta) ->
            Prim
              ( -1,
                T_option,
                [unparse_comparable_ty t],
                unparse_type_annot meta.annot )

      let unparse_memo_size memo_size =
        let z = Alpha_context.Sapling.Memo_size.unparse_to_z memo_size in
        Int (-1, z)

      let rec unparse_ty : type a. a ty -> Script.node =
       fun ty ->
        let return (name, args, annot) = Prim (-1, name, args, annot) in
        match ty with
        | Unit_t meta -> return (T_unit, [], unparse_type_annot meta.annot)
        | Int_t meta -> return (T_int, [], unparse_type_annot meta.annot)
        | Nat_t meta -> return (T_nat, [], unparse_type_annot meta.annot)
        | Signature_t meta ->
            return (T_signature, [], unparse_type_annot meta.annot)
        | String_t meta -> return (T_string, [], unparse_type_annot meta.annot)
        | Bytes_t meta -> return (T_bytes, [], unparse_type_annot meta.annot)
        | Mutez_t meta -> return (T_mutez, [], unparse_type_annot meta.annot)
        | Bool_t meta -> return (T_bool, [], unparse_type_annot meta.annot)
        | Key_hash_t meta ->
            return (T_key_hash, [], unparse_type_annot meta.annot)
        | Key_t meta -> return (T_key, [], unparse_type_annot meta.annot)
        | Timestamp_t meta ->
            return (T_timestamp, [], unparse_type_annot meta.annot)
        | Address_t meta -> return (T_address, [], unparse_type_annot meta.annot)
        | Operation_t meta ->
            return (T_operation, [], unparse_type_annot meta.annot)
        | Chain_id_t meta ->
            return (T_chain_id, [], unparse_type_annot meta.annot)
        | Never_t meta -> return (T_never, [], unparse_type_annot meta.annot)
        | Bls12_381_g1_t meta ->
            return (T_bls12_381_g1, [], unparse_type_annot meta.annot)
        | Bls12_381_g2_t meta ->
            return (T_bls12_381_g2, [], unparse_type_annot meta.annot)
        | Bls12_381_fr_t meta ->
            return (T_bls12_381_fr, [], unparse_type_annot meta.annot)
        | Contract_t (ut, meta) ->
            let t = unparse_ty ut in
            return (T_contract, [t], unparse_type_annot meta.annot)
        | Pair_t ((utl, l_field, l_var), (utr, r_field, r_var), meta) ->
            let annot = unparse_type_annot meta.annot in
            let utl = unparse_ty utl in
            let tl = add_field_annot l_field l_var utl in
            let utr = unparse_ty utr in
            let tr = add_field_annot r_field r_var utr in
            return (T_pair, [tl; tr], annot)
        | Union_t ((utl, l_field), (utr, r_field), meta) ->
            let annot = unparse_type_annot meta.annot in
            let utl = unparse_ty utl in
            let tl = add_field_annot l_field None utl in
            let utr = unparse_ty utr in
            let tr = add_field_annot r_field None utr in
            return (T_or, [tl; tr], annot)
        | Lambda_t (uta, utr, meta) ->
            let ta = unparse_ty uta in
            let tr = unparse_ty utr in
            return (T_lambda, [ta; tr], unparse_type_annot meta.annot)
        | Option_t (ut, meta) ->
            let annot = unparse_type_annot meta.annot in
            let ut = unparse_ty ut in
            return (T_option, [ut], annot)
        | List_t (ut, meta) ->
            let t = unparse_ty ut in
            return (T_list, [t], unparse_type_annot meta.annot)
        | Ticket_t (ut, meta) ->
            let t = unparse_comparable_ty ut in
            return (T_ticket, [t], unparse_type_annot meta.annot)
        | Set_t (ut, meta) ->
            let t = unparse_comparable_ty ut in
            return (T_set, [t], unparse_type_annot meta.annot)
        | Map_t (uta, utr, meta) ->
            let ta = unparse_comparable_ty uta in
            let tr = unparse_ty utr in
            return (T_map, [ta; tr], unparse_type_annot meta.annot)
        | Big_map_t (uta, utr, meta) ->
            let ta = unparse_comparable_ty uta in
            let tr = unparse_ty utr in
            return (T_big_map, [ta; tr], unparse_type_annot meta.annot)
        | Sapling_transaction_t (memo_size, meta) ->
            return
              ( T_sapling_transaction,
                [unparse_memo_size memo_size],
                unparse_type_annot meta.annot )
        | Sapling_state_t (memo_size, meta) ->
            return
              ( T_sapling_state,
                [unparse_memo_size memo_size],
                unparse_type_annot meta.annot )
        | Chest_t meta -> return (T_chest, [], unparse_type_annot meta.annot)
        | Chest_key_t meta ->
            return (T_chest_key, [], unparse_type_annot meta.annot)
    end

    let run_operation_service ctxt ()
        ({shell; protocol_data = Operation_data protocol_data}, chain_id) =
      (* this code is a duplicate of Apply without signature check *)
      let partial_precheck_manager_contents (type kind) ctxt
          (op : kind Kind.manager contents) :
          (context * Receipt.balance_updates) tzresult Lwt.t =
        let (Manager_operation
              {source; fee; counter; operation; gas_limit; storage_limit}) =
          op
        in
        Gas.consume_limit_in_block ctxt gas_limit >>?= fun ctxt ->
        let ctxt = Gas.set_limit ctxt gas_limit in
        Fees.check_storage_limit ctxt ~storage_limit >>?= fun () ->
        Contract.must_be_allocated ctxt (Contract.implicit_contract source)
        >>=? fun () ->
        Contract.check_counter_increment ctxt source counter >>=? fun () ->
        (match operation with
        | Reveal pk -> Contract.reveal_manager_key ctxt source pk
        | Transaction {parameters; _} ->
            (* Here the data comes already deserialized, so we need to fake the deserialization to mimic apply *)
            let arg_bytes =
              Data_encoding.Binary.to_bytes_exn
                Script.lazy_expr_encoding
                parameters
            in
            let arg =
              match
                Data_encoding.Binary.of_bytes_opt
                  Script.lazy_expr_encoding
                  arg_bytes
              with
              | Some arg -> arg
              | None -> assert false
            in
            Lwt.return
            @@ record_trace Apply.Gas_quota_exceeded_init_deserialize
            @@ (* Fail if not enough gas for complete deserialization cost *)
            ( Script.force_decode_in_context ctxt arg >|? fun (_arg, ctxt) ->
              ctxt )
        | Origination {script; _} ->
            (* Here the data comes already deserialized, so we need to fake the deserialization to mimic apply *)
            let script_bytes =
              Data_encoding.Binary.to_bytes_exn Script.encoding script
            in
            let script =
              match
                Data_encoding.Binary.of_bytes_opt Script.encoding script_bytes
              with
              | Some script -> script
              | None -> assert false
            in
            Lwt.return
            @@ record_trace Apply.Gas_quota_exceeded_init_deserialize
            @@ (* Fail if not enough gas for complete deserialization cost *)
            ( Script.force_decode_in_context ctxt script.code
            >>? fun (_code, ctxt) ->
              Script.force_decode_in_context ctxt script.storage
              >|? fun (_storage, ctxt) -> ctxt )
        | _ -> return ctxt)
        >>=? fun ctxt ->
        Contract.get_manager_key ctxt source >>=? fun _public_key ->
        (* signature check unplugged from here *)
        Contract.increment_counter ctxt source >>=? fun ctxt ->
        let source_contract = Contract.implicit_contract source in
        Token.transfer ctxt (`Contract source_contract) `Block_fees fee
      in
      let open Apply_results in
      let rec partial_precheck_manager_contents_list :
          type kind.
          Alpha_context.t ->
          kind Kind.manager contents_list ->
          payload_producer:Signature.Public_key_hash.t ->
          (context
          * ( kind Kind.manager,
              Receipt.balance_updates )
            prechecked_contents_list)
          tzresult
          Lwt.t =
       fun ctxt contents_list ~payload_producer ->
        match contents_list with
        | Single contents ->
            partial_precheck_manager_contents ctxt contents
            >>=? fun (ctxt, balance_updates) ->
            return (ctxt, PrecheckedSingle {contents; result = balance_updates})
        | Cons (contents, rest) ->
            partial_precheck_manager_contents ctxt contents
            >>=? fun (ctxt, balance_updates) ->
            partial_precheck_manager_contents_list ctxt rest ~payload_producer
            >>=? fun (ctxt, prechecked_contents_list) ->
            return
              ( ctxt,
                PrecheckedCons
                  ( {contents; result = balance_updates},
                    prechecked_contents_list ) )
      in
      let ret contents =
        ( Operation_data protocol_data,
          Apply_results.Operation_metadata {contents} )
      in
      let operation : _ operation = {shell; protocol_data} in
      let hash = Operation.hash {shell; protocol_data} in
      let ctxt = Contract.init_origination_nonce ctxt hash in
      let payload_producer = Signature.Public_key_hash.zero in
      match protocol_data.contents with
      | Single (Manager_operation _) as op ->
          partial_precheck_manager_contents_list ctxt op ~payload_producer
          >>=? fun (ctxt, prechecked_contents_list) ->
          Apply.apply_manager_contents_list
            ctxt
            Optimized
            ~payload_producer
            chain_id
            prechecked_contents_list
          >|= fun (_ctxt, result) -> ok @@ ret result
      | Cons (Manager_operation _, _) as op ->
          partial_precheck_manager_contents_list ctxt op ~payload_producer
          >>=? fun (ctxt, prechecked_contents_list) ->
          Apply.apply_manager_contents_list
            ctxt
            Optimized
            ~payload_producer
            chain_id
            prechecked_contents_list
          >|= fun (_ctxt, result) -> ok @@ ret result
      | _ ->
          let predecessor_level =
            match
              Alpha_context.Level.pred ctxt (Alpha_context.Level.current ctxt)
            with
            | Some level -> level
            | None -> assert false
          in
          Alpha_context.Round.get ctxt >>=? fun predecessor_round ->
          Apply.apply_contents_list
            ctxt
            chain_id
            (Partial_construction
               {
                 predecessor_level;
                 predecessor_round;
                 grand_parent_round = Round.zero;
               })
            Optimized
            ~payload_producer
            operation
            operation.protocol_data.contents
          >|=? fun (_ctxt, result) -> ret result

    (*

       The execution of an operation depends on the state of the
       cache. In particular, gas consumption is usually impacted by
       cache hits and misses.

       Unfortunately, the state of the cache is different between the
       context at operation-creation time and the context when is
       included in a block.

       Therefore, the simulation tries to predict the state of the
       cache in a [time_in_blocks] assumed to be close to the inclusion
       time of the operation.

    *)
    let simulate_operation_service ctxt () (op, chain_id, time_in_blocks) =
      let ctxt = Cache.Admin.future_cache_expectation ctxt ~time_in_blocks in
      run_operation_service ctxt () (op, chain_id)

    let register () =
      let originate_dummy_contract ctxt script balance =
        let ctxt = Contract.init_origination_nonce ctxt Operation_hash.zero in
        Lwt.return (Contract.fresh_contract_from_current_nonce ctxt)
        >>=? fun (ctxt, dummy_contract) ->
        Contract.raw_originate
          ctxt
          ~prepaid_bootstrap_storage:false
          dummy_contract
          ~script:(script, None)
        >>=? fun ctxt ->
        Token.transfer
          ~origin:Simulation
          ctxt
          `Minted
          (`Contract dummy_contract)
          balance
        >>=? fun (ctxt, _) -> return (ctxt, dummy_contract)
      in
      let script_entrypoint_type ctxt expr entrypoint =
        let ctxt = Gas.set_unlimited ctxt in
        let legacy = false in
        let open Script_ir_translator in
        parse_toplevel ctxt ~legacy expr
        >>=? fun ({arg_type; root_name; _}, ctxt) ->
        Lwt.return
          ( ( parse_parameter_ty ctxt ~legacy arg_type
            >>? fun (Ex_ty arg_type, _) ->
              Script_ir_translator.find_entrypoint
                ~root_name
                arg_type
                entrypoint )
          >>? fun (_f, Ex_ty ty) ->
            unparse_ty ctxt ty >|? fun (ty_node, _) ->
            Micheline.strip_locations ty_node )
      in
      Registration.register0
        ~chunked:true
        S.run_code
        (fun
          ctxt
          ()
          ( ( code,
              storage,
              parameter,
              amount,
              balance,
              chain_id,
              source,
              payer,
              gas,
              entrypoint ),
            unparsing_mode )
        ->
          let unparsing_mode = Option.value ~default:Readable unparsing_mode in
          let storage = Script.lazy_expr storage in
          let code = Script.lazy_expr code in
          originate_dummy_contract ctxt {storage; code} balance
          >>=? fun (ctxt, dummy_contract) ->
          let (source, payer) =
            match (source, payer) with
            | (Some source, Some payer) -> (source, payer)
            | (Some source, None) -> (source, source)
            | (None, Some payer) -> (payer, payer)
            | (None, None) -> (dummy_contract, dummy_contract)
          in
          let gas =
            match gas with
            | Some gas -> gas
            | None -> Constants.hard_gas_limit_per_operation ctxt
          in
          let ctxt = Gas.set_limit ctxt gas in
          let step_constants =
            let open Script_interpreter in
            {source; payer; self = dummy_contract; amount; chain_id}
          in
          Script_interpreter.execute
            ctxt
            unparsing_mode
            step_constants
            ~cached_script:None
            ~script:{storage; code}
            ~entrypoint
            ~parameter
            ~internal:true
          >|=? fun ( {
                       Script_interpreter.storage;
                       operations;
                       lazy_storage_diff;
                       _;
                     },
                     _ ) -> (storage, operations, lazy_storage_diff)) ;
      Registration.register0
        ~chunked:true
        S.trace_code
        (fun
          ctxt
          ()
          ( ( code,
              storage,
              parameter,
              amount,
              balance,
              chain_id,
              source,
              payer,
              gas,
              entrypoint ),
            unparsing_mode )
        ->
          let unparsing_mode = Option.value ~default:Readable unparsing_mode in
          let storage = Script.lazy_expr storage in
          let code = Script.lazy_expr code in
          originate_dummy_contract ctxt {storage; code} balance
          >>=? fun (ctxt, dummy_contract) ->
          let (source, payer) =
            match (source, payer) with
            | (Some source, Some payer) -> (source, payer)
            | (Some source, None) -> (source, source)
            | (None, Some payer) -> (payer, payer)
            | (None, None) -> (dummy_contract, dummy_contract)
          in
          let gas =
            match gas with
            | Some gas -> gas
            | None -> Constants.hard_gas_limit_per_operation ctxt
          in
          let ctxt = Gas.set_limit ctxt gas in
          let step_constants =
            let open Script_interpreter in
            {source; payer; self = dummy_contract; amount; chain_id}
          in
          let module Unparsing_mode = struct
            let unparsing_mode = unparsing_mode
          end in
          let module Interp = Traced_interpreter (Unparsing_mode) in
          Interp.execute
            ctxt
            step_constants
            ~script:{storage; code}
            ~entrypoint
            ~parameter
          >|=? fun ( {
                       Script_interpreter.storage;
                       operations;
                       lazy_storage_diff;
                       _;
                     },
                     trace ) -> (storage, operations, trace, lazy_storage_diff)) ;
      Registration.register0
        ~chunked:true
        S.run_view
        (fun
          ctxt
          ()
          ( contract,
            entrypoint,
            input,
            chain_id,
            source,
            payer,
            gas,
            unparsing_mode )
        ->
          Contract.get_script ctxt contract >>=? fun (ctxt, script_opt) ->
          Option.fold
            ~some:ok
            ~none:(Error_monad.error View_helpers.Viewed_contract_has_no_script)
            script_opt
          >>?= fun script ->
          Script_repr.(force_decode script.code) >>?= fun decoded_script ->
          script_entrypoint_type ctxt decoded_script entrypoint
          >>=? fun view_ty ->
          View_helpers.extract_view_output_type entrypoint view_ty
          >>?= fun ty ->
          Error_monad.trace View_helpers.View_callback_origination_failed
          @@ originate_dummy_contract
               ctxt
               (View_helpers.make_viewer_script ty)
               Tez.zero
          >>=? fun (ctxt, viewer_contract) ->
          let (source, payer) =
            match (source, payer) with
            | (Some source, Some payer) -> (source, payer)
            | (Some source, None) -> (source, source)
            | (None, Some payer) -> (payer, payer)
            | (None, None) -> (contract, contract)
          in
          let gas =
            Option.value
              ~default:(Constants.hard_gas_limit_per_operation ctxt)
              gas
          in
          let ctxt = Gas.set_limit ctxt gas in
          let step_constants =
            let open Script_interpreter in
            {source; payer; self = contract; amount = Tez.zero; chain_id}
          in
          let parameter =
            View_helpers.make_view_parameter
              (Micheline.root input)
              viewer_contract
          in
          Script_interpreter.execute
            ctxt
            unparsing_mode
            step_constants
            ~script
            ~cached_script:None
            ~entrypoint
            ~parameter
            ~internal:true
          >>=? fun ({Script_interpreter.operations; _}, (_, _)) ->
          View_helpers.extract_parameter_from_operations
            entrypoint
            operations
            viewer_contract
          >>?= fun parameter -> Lwt.return (Script_repr.force_decode parameter)) ;
      Registration.register0
        ~chunked:false
        S.typecheck_code
        (fun ctxt () (expr, maybe_gas, legacy) ->
          let legacy = Option.value ~default:false legacy in
          let ctxt =
            match maybe_gas with
            | None -> Gas.set_unlimited ctxt
            | Some gas -> Gas.set_limit ctxt gas
          in
          Script_ir_translator.typecheck_code ~legacy ctxt expr
          >|=? fun (res, ctxt) -> (res, Gas.level ctxt)) ;
      Registration.register0
        ~chunked:false
        S.script_size
        (fun ctxt () (expr, storage, maybe_gas, legacy) ->
          let legacy = Option.value ~default:false legacy in
          let ctxt =
            match maybe_gas with
            | None -> Gas.set_unlimited ctxt
            | Some gas -> Gas.set_limit ctxt gas
          in
          let code = Script.lazy_expr expr in
          Script_ir_translator.parse_code ~legacy ctxt ~code
          >>=? fun ( Ex_code
                       {
                         code;
                         arg_type;
                         storage_type;
                         views;
                         root_name;
                         code_size;
                       },
                     ctxt ) ->
          Script_ir_translator.parse_data
            ~legacy
            ~allow_forged:true
            ctxt
            storage_type
            (Micheline.root storage)
          >>=? fun (storage, _) ->
          let script =
            Script_ir_translator.Ex_script
              {
                code;
                arg_type;
                storage_type;
                views;
                root_name;
                code_size;
                storage;
              }
          in
          let (size, cost) = Script_ir_translator.script_size script in
          Gas.consume ctxt cost >>?= fun _ctxt -> return @@ size) ;

      Registration.register0
        ~chunked:false
        S.typecheck_data
        (fun ctxt () (data, ty, maybe_gas, legacy) ->
          let legacy = Option.value ~default:false legacy in
          let ctxt =
            match maybe_gas with
            | None -> Gas.set_unlimited ctxt
            | Some gas -> Gas.set_limit ctxt gas
          in
          typecheck_data ~legacy ctxt (data, ty) >|=? fun ctxt -> Gas.level ctxt) ;
      Registration.register0
        ~chunked:true
        S.pack_data
        (fun ctxt () (expr, typ, maybe_gas) ->
          let open Script_ir_translator in
          let ctxt =
            match maybe_gas with
            | None -> Gas.set_unlimited ctxt
            | Some gas -> Gas.set_limit ctxt gas
          in
          parse_packable_ty ctxt ~legacy:true (Micheline.root typ)
          >>?= fun (Ex_ty typ, ctxt) ->
          parse_data
            ctxt
            ~legacy:true
            ~allow_forged:true
            typ
            (Micheline.root expr)
          >>=? fun (data, ctxt) ->
          Script_ir_translator.pack_data ctxt typ data >|=? fun (bytes, ctxt) ->
          (bytes, Gas.level ctxt)) ;
      Registration.register0
        ~chunked:true
        S.normalize_data
        (fun ctxt () (expr, typ, unparsing_mode, legacy) ->
          let open Script_ir_translator in
          let legacy = Option.value ~default:false legacy in
          let ctxt = Gas.set_unlimited ctxt in
          Script_ir_translator.parse_any_ty ctxt ~legacy (Micheline.root typ)
          >>?= fun (Ex_ty typ, ctxt) ->
          parse_data ctxt ~legacy ~allow_forged:true typ (Micheline.root expr)
          >>=? fun (data, ctxt) ->
          Script_ir_translator.unparse_data ctxt unparsing_mode typ data
          >|=? fun (normalized, _ctxt) -> Micheline.strip_locations normalized) ;
      Registration.register0
        ~chunked:true
        S.normalize_script
        (fun ctxt () (script, unparsing_mode) ->
          let ctxt = Gas.set_unlimited ctxt in
          Script_ir_translator.unparse_code
            ctxt
            unparsing_mode
            (Micheline.root script)
          >|=? fun (normalized, _ctxt) -> Micheline.strip_locations normalized) ;
      Registration.register0 ~chunked:true S.normalize_type (fun ctxt () typ ->
          let open Script_ir_translator in
          let ctxt = Gas.set_unlimited ctxt in
          (* Unfortunately, Script_ir_translator.parse_any_ty is not exported *)
          Script_ir_translator.parse_ty
            ctxt
            ~legacy:true
            ~allow_lazy_storage:true
            ~allow_operation:true
            ~allow_contract:true
            ~allow_ticket:true
            (Micheline.root typ)
          >>?= fun (Ex_ty typ, _ctxt) ->
          let normalized = Unparse_types.unparse_ty typ in
          return @@ Micheline.strip_locations normalized) ;
      Registration.register0 ~chunked:true S.run_operation run_operation_service ;
      Registration.register0
        ~chunked:true
        S.simulate_operation
        simulate_operation_service ;
      Registration.register0
        ~chunked:true
        S.entrypoint_type
        (fun ctxt () (expr, entrypoint) ->
          script_entrypoint_type ctxt expr entrypoint) ;
      Registration.register0
        ~chunked:true
        S.list_entrypoints
        (fun ctxt () expr ->
          let ctxt = Gas.set_unlimited ctxt in
          let legacy = false in
          let open Script_ir_translator in
          parse_toplevel ~legacy ctxt expr
          >>=? fun ({arg_type; root_name; _}, ctxt) ->
          Lwt.return
            ( parse_parameter_ty ctxt ~legacy arg_type
            >>? fun (Ex_ty arg_type, _) ->
              Script_ir_translator.list_entrypoints ~root_name arg_type ctxt
              >|? fun (unreachable_entrypoint, map) ->
              ( unreachable_entrypoint,
                Entrypoints_map.fold
                  (fun entry (_, ty) acc ->
                    (entry, Micheline.strip_locations ty) :: acc)
                  map
                  [] ) ))

    let run_code ?unparsing_mode ?gas ?(entrypoint = "default") ~script ~storage
        ~input ~amount ~balance ~chain_id ~source ~payer ctxt block =
      RPC_context.make_call0
        S.run_code
        ctxt
        block
        ()
        ( ( script,
            storage,
            input,
            amount,
            balance,
            chain_id,
            source,
            payer,
            gas,
            entrypoint ),
          unparsing_mode )

    let trace_code ?unparsing_mode ?gas ?(entrypoint = "default") ~script
        ~storage ~input ~amount ~balance ~chain_id ~source ~payer ctxt block =
      RPC_context.make_call0
        S.trace_code
        ctxt
        block
        ()
        ( ( script,
            storage,
            input,
            amount,
            balance,
            chain_id,
            source,
            payer,
            gas,
            entrypoint ),
          unparsing_mode )

    let run_view ?gas ~contract ~entrypoint ~input ~chain_id ?source ?payer
        ~unparsing_mode ctxt block =
      RPC_context.make_call0
        S.run_view
        ctxt
        block
        ()
        ( contract,
          entrypoint,
          input,
          chain_id,
          source,
          payer,
          gas,
          unparsing_mode )

    let typecheck_code ?gas ?legacy ~script ctxt block =
      RPC_context.make_call0 S.typecheck_code ctxt block () (script, gas, legacy)

    let script_size ?gas ?legacy ~script ~storage ctxt block =
      RPC_context.make_call0
        S.script_size
        ctxt
        block
        ()
        (script, storage, gas, legacy)

    let typecheck_data ?gas ?legacy ~data ~ty ctxt block =
      RPC_context.make_call0
        S.typecheck_data
        ctxt
        block
        ()
        (data, ty, gas, legacy)

    let pack_data ?gas ~data ~ty ctxt block =
      RPC_context.make_call0 S.pack_data ctxt block () (data, ty, gas)

    let normalize_data ?legacy ~data ~ty ~unparsing_mode ctxt block =
      RPC_context.make_call0
        S.normalize_data
        ctxt
        block
        ()
        (data, ty, unparsing_mode, legacy)

    let normalize_script ~script ~unparsing_mode ctxt block =
      RPC_context.make_call0
        S.normalize_script
        ctxt
        block
        ()
        (script, unparsing_mode)

    let normalize_type ~ty ctxt block =
      RPC_context.make_call0 S.normalize_type ctxt block () ty

    let run_operation ~op ~chain_id ctxt block =
      RPC_context.make_call0 S.run_operation ctxt block () (op, chain_id)

    let simulate_operation ~op ~chain_id ~latency ctxt block =
      RPC_context.make_call0
        S.simulate_operation
        ctxt
        block
        ()
        (op, chain_id, latency)

    let entrypoint_type ~script ~entrypoint ctxt block =
      RPC_context.make_call0 S.entrypoint_type ctxt block () (script, entrypoint)

    let list_entrypoints ctxt block ~script =
      RPC_context.make_call0 S.list_entrypoints ctxt block () script
  end

  module Contract = struct
    module S = struct
      let path =
        (RPC_path.(open_root / "context" / "contracts")
          : RPC_context.t RPC_path.context)

      let get_storage_normalized =
        let open Data_encoding in
        RPC_service.post_service
          ~description:
            "Access the data of the contract and normalize it using the \
             requested unparsing mode."
          ~input:(obj1 (req "unparsing_mode" unparsing_mode_encoding))
          ~query:RPC_query.empty
          ~output:(option Script.expr_encoding)
          RPC_path.(path /: Contract.rpc_arg / "storage" / "normalized")

      let get_script_normalized =
        let open Data_encoding in
        RPC_service.post_service
          ~description:
            "Access the script of the contract and normalize it using the \
             requested unparsing mode."
          ~input:(obj1 (req "unparsing_mode" unparsing_mode_encoding))
          ~query:RPC_query.empty
          ~output:(option Script.encoding)
          RPC_path.(path /: Contract.rpc_arg / "script" / "normalized")
    end

    let register () =
      (* Patched RPC: get_storage *)
      Registration.register1
        ~chunked:true
        S.get_storage_normalized
        (fun ctxt contract () unparsing_mode ->
          Contract.get_script ctxt contract >>=? fun (ctxt, script) ->
          match script with
          | None -> return_none
          | Some script ->
              let ctxt = Gas.set_unlimited ctxt in
              let open Script_ir_translator in
              parse_script
                ctxt
                ~legacy:true
                ~allow_forged_in_storage:true
                script
              >>=? fun (Ex_script script, ctxt) ->
              unparse_script ctxt unparsing_mode script
              >>=? fun (script, ctxt) ->
              Script.force_decode_in_context ctxt script.storage
              >>?= fun (storage, _ctxt) -> return_some storage) ;
      (* Patched RPC: get_script *)
      Registration.register1
        ~chunked:true
        S.get_script_normalized
        (fun ctxt contract () unparsing_mode ->
          Contract.get_script ctxt contract >>=? fun (ctxt, script) ->
          match script with
          | None -> return_none
          | Some script ->
              let ctxt = Gas.set_unlimited ctxt in
              let open Script_ir_translator in
              parse_script
                ctxt
                ~legacy:true
                ~allow_forged_in_storage:true
                script
              >>=? fun (Ex_script script, ctxt) ->
              unparse_script ctxt unparsing_mode script
              >>=? fun (script, _ctxt) -> return_some script)

    let get_storage_normalized ctxt block ~contract ~unparsing_mode =
      RPC_context.make_call1
        S.get_storage_normalized
        ctxt
        block
        contract
        ()
        unparsing_mode

    let get_script_normalized ctxt block ~contract ~unparsing_mode =
      RPC_context.make_call1
        S.get_script_normalized
        ctxt
        block
        contract
        ()
        unparsing_mode
  end

  module Big_map = struct
    module S = struct
      let path =
        (RPC_path.(open_root / "context" / "big_maps")
          : RPC_context.t RPC_path.context)

      let big_map_get_normalized =
        let open Data_encoding in
        RPC_service.post_service
          ~description:
            "Access the value associated with a key in a big map, normalize \
             the output using the requested unparsing mode."
          ~query:RPC_query.empty
          ~input:(obj1 (req "unparsing_mode" unparsing_mode_encoding))
          ~output:Script.expr_encoding
          RPC_path.(
            path /: Big_map.Id.rpc_arg /: Script_expr_hash.rpc_arg
            / "normalized")
    end

    let register () =
      Registration.register2
        ~chunked:true
        S.big_map_get_normalized
        (fun ctxt id key () unparsing_mode ->
          let open Script_ir_translator in
          let ctxt = Gas.set_unlimited ctxt in
          Big_map.exists ctxt id >>=? fun (ctxt, types) ->
          match types with
          | None -> raise Not_found
          | Some (_, value_type) -> (
              parse_big_map_value_ty
                ctxt
                ~legacy:true
                (Micheline.root value_type)
              >>?= fun (Ex_ty value_type, ctxt) ->
              Big_map.get_opt ctxt id key >>=? fun (_ctxt, value) ->
              match value with
              | None -> raise Not_found
              | Some value ->
                  parse_data
                    ctxt
                    ~legacy:true
                    ~allow_forged:true
                    value_type
                    (Micheline.root value)
                  >>=? fun (value, ctxt) ->
                  unparse_data ctxt unparsing_mode value_type value
                  >|=? fun (value, _ctxt) -> Micheline.strip_locations value))

    let big_map_get_normalized ctxt block id key ~unparsing_mode =
      RPC_context.make_call2
        S.big_map_get_normalized
        ctxt
        block
        id
        key
        ()
        unparsing_mode
  end

  module Forge = struct
    module S = struct
      open Data_encoding

      let path = RPC_path.(path / "forge")

      let operations =
        RPC_service.post_service
          ~description:"Forge an operation"
          ~query:RPC_query.empty
          ~input:Operation.unsigned_encoding
          ~output:bytes
          RPC_path.(path / "operations")

      let empty_proof_of_work_nonce =
        Bytes.make Constants_repr.proof_of_work_nonce_size '\000'

      let protocol_data =
        RPC_service.post_service
          ~description:"Forge the protocol-specific part of a block header"
          ~query:RPC_query.empty
          ~input:
            (obj5
               (req "payload_hash" Block_payload_hash.encoding)
               (req "payload_round" Round.encoding)
               (opt "nonce_hash" Nonce_hash.encoding)
               (dft
                  "proof_of_work_nonce"
                  (Fixed.bytes Alpha_context.Constants.proof_of_work_nonce_size)
                  empty_proof_of_work_nonce)
               (dft "liquidity_baking_escape_vote" bool false))
          ~output:(obj1 (req "protocol_data" bytes))
          RPC_path.(path / "protocol_data")
    end

    let register () =
      Registration.register0_noctxt
        ~chunked:true
        S.operations
        (fun () (shell, proto) ->
          return
            (Data_encoding.Binary.to_bytes_exn
               Operation.unsigned_encoding
               (shell, proto))) ;
      Registration.register0_noctxt
        ~chunked:true
        S.protocol_data
        (fun
          ()
          ( payload_hash,
            payload_round,
            seed_nonce_hash,
            proof_of_work_nonce,
            liquidity_baking_escape_vote )
        ->
          return
            (Data_encoding.Binary.to_bytes_exn
               Block_header.contents_encoding
               {
                 payload_hash;
                 payload_round;
                 seed_nonce_hash;
                 proof_of_work_nonce;
                 liquidity_baking_escape_vote;
               }))

    module Manager = struct
      let[@coq_axiom_with_reason "cast on e"] operations ctxt block ~branch
          ~source ?sourcePubKey ~counter ~fee ~gas_limit ~storage_limit
          operations =
        Contract_services.manager_key ctxt block source >>= function
        | Error _ as e -> Lwt.return e
        | Ok revealed ->
            let ops =
              List.map
                (fun (Manager operation) ->
                  Contents
                    (Manager_operation
                       {
                         source;
                         counter;
                         operation;
                         fee;
                         gas_limit;
                         storage_limit;
                       }))
                operations
            in
            let ops =
              match (sourcePubKey, revealed) with
              | (None, _) | (_, Some _) -> ops
              | (Some pk, None) ->
                  let operation = Reveal pk in
                  Contents
                    (Manager_operation
                       {
                         source;
                         counter;
                         operation;
                         fee;
                         gas_limit;
                         storage_limit;
                       })
                  :: ops
            in
            Environment.wrap_tzresult @@ Operation.of_list ops >>?= fun ops ->
            RPC_context.make_call0 S.operations ctxt block () ({branch}, ops)

      let reveal ctxt block ~branch ~source ~sourcePubKey ~counter ~fee () =
        operations
          ctxt
          block
          ~branch
          ~source
          ~sourcePubKey
          ~counter
          ~fee
          ~gas_limit:Gas.Arith.zero
          ~storage_limit:Z.zero
          []

      let transaction ctxt block ~branch ~source ?sourcePubKey ~counter ~amount
          ~destination ?(entrypoint = "default") ?parameters ~gas_limit
          ~storage_limit ~fee () =
        let parameters =
          Option.fold
            ~some:Script.lazy_expr
            ~none:Script.unit_parameter
            parameters
        in
        operations
          ctxt
          block
          ~branch
          ~source
          ?sourcePubKey
          ~counter
          ~fee
          ~gas_limit
          ~storage_limit
          [Manager (Transaction {amount; parameters; destination; entrypoint})]

      let origination ctxt block ~branch ~source ?sourcePubKey ~counter ~balance
          ?delegatePubKey ~script ~gas_limit ~storage_limit ~fee () =
        operations
          ctxt
          block
          ~branch
          ~source
          ?sourcePubKey
          ~counter
          ~fee
          ~gas_limit
          ~storage_limit
          [
            Manager
              (Origination
                 {
                   delegate = delegatePubKey;
                   script;
                   credit = balance;
                   preorigination = None;
                 });
          ]

      let delegation ctxt block ~branch ~source ?sourcePubKey ~counter ~fee
          delegate =
        operations
          ctxt
          block
          ~branch
          ~source
          ?sourcePubKey
          ~counter
          ~fee
          ~gas_limit:Gas.Arith.zero
          ~storage_limit:Z.zero
          [Manager (Delegation delegate)]
    end

    let operation ctxt block ~branch operation =
      RPC_context.make_call0
        S.operations
        ctxt
        block
        ()
        ({branch}, Contents_list (Single operation))

    let endorsement ctxt b ~branch ~consensus_content () =
      operation ctxt b ~branch (Endorsement consensus_content)

    let proposals ctxt b ~branch ~source ~period ~proposals () =
      operation ctxt b ~branch (Proposals {source; period; proposals})

    let ballot ctxt b ~branch ~source ~period ~proposal ~ballot () =
      operation ctxt b ~branch (Ballot {source; period; proposal; ballot})

    let failing_noop ctxt b ~branch ~message () =
      operation ctxt b ~branch (Failing_noop message)

    let seed_nonce_revelation ctxt block ~branch ~level ~nonce () =
      operation ctxt block ~branch (Seed_nonce_revelation {level; nonce})

    let double_baking_evidence ctxt block ~branch ~bh1 ~bh2 () =
      operation ctxt block ~branch (Double_baking_evidence {bh1; bh2})

    let double_endorsement_evidence ctxt block ~branch ~op1 ~op2 () =
      operation ctxt block ~branch (Double_endorsement_evidence {op1; op2})

    let double_preendorsement_evidence ctxt block ~branch ~op1 ~op2 () =
      operation ctxt block ~branch (Double_preendorsement_evidence {op1; op2})

    let empty_proof_of_work_nonce =
      Bytes.make Constants_repr.proof_of_work_nonce_size '\000'

    let protocol_data ctxt block ?(payload_hash = Block_payload_hash.zero)
        ?(payload_round = Round.zero) ?seed_nonce_hash
        ?(proof_of_work_nonce = empty_proof_of_work_nonce)
        ~liquidity_baking_escape_vote () =
      RPC_context.make_call0
        S.protocol_data
        ctxt
        block
        ()
        ( payload_hash,
          payload_round,
          seed_nonce_hash,
          proof_of_work_nonce,
          liquidity_baking_escape_vote )
  end

  module Parse = struct
    module S = struct
      open Data_encoding

      let path = RPC_path.(path / "parse")

      let operations =
        RPC_service.post_service
          ~description:"Parse operations"
          ~query:RPC_query.empty
          ~input:
            (obj2
               (req "operations" (list (dynamic_size Operation.raw_encoding)))
               (opt "check_signature" bool))
          ~output:(list (dynamic_size Operation.encoding))
          RPC_path.(path / "operations")

      let block =
        RPC_service.post_service
          ~description:"Parse a block"
          ~query:RPC_query.empty
          ~input:Block_header.raw_encoding
          ~output:Block_header.protocol_data_encoding
          RPC_path.(path / "block")
    end

    let parse_protocol_data protocol_data =
      match
        Data_encoding.Binary.of_bytes_opt
          Block_header.protocol_data_encoding
          protocol_data
      with
      | None -> Stdlib.failwith "Cant_parse_protocol_data"
      | Some protocol_data -> protocol_data

    let register () =
      Registration.register0
        ~chunked:true
        S.operations
        (fun _ctxt () (operations, check) ->
          List.map_es
            (fun raw ->
              parse_operation raw >>?= fun op ->
              (match check with
              | Some true -> return_unit (* FIXME *)
              (* I.check_signature ctxt *)
              (* op.protocol_data.signature op.shell op.protocol_data.contents *)
              | Some false | None -> return_unit)
              >|=? fun () -> op)
            operations) ;
      Registration.register0_noctxt ~chunked:false S.block (fun () raw_block ->
          return @@ parse_protocol_data raw_block.protocol_data)

    let operations ctxt block ?check operations =
      RPC_context.make_call0 S.operations ctxt block () (operations, check)

    let block ctxt block shell protocol_data =
      RPC_context.make_call0
        S.block
        ctxt
        block
        ()
        ({shell; protocol_data} : Block_header.raw)
  end

  (* Compute the estimated starting time of a [round] at a future
     [level], given the head's level [current_level], timestamp
     [current_timestamp], and round [current_round]. Assumes blocks at
     intermediate levels are produced at round 0. *)
  let estimated_time round_durations ~current_level ~current_round
      ~current_timestamp ~level ~round =
    if Level.(level <= current_level) then Result.return_none
    else
      Round.of_int round >>? fun round ->
      Round.timestamp_of_round
        round_durations
        ~round
        ~predecessor_timestamp:current_timestamp
        ~predecessor_round:current_round
      >>? fun round_start_at_next_level ->
      let step = Round.round_duration round_durations Round.zero in
      let diff = Level.diff level current_level in
      Period.mult (Int32.pred diff) step >>? fun delay ->
      Timestamp.(round_start_at_next_level +? delay) >>? fun timestamp ->
      Result.return_some timestamp

  let requested_levels ~default_level ctxt cycles levels =
    match (levels, cycles) with
    | ([], []) -> [default_level]
    | (levels, cycles) ->
        (* explicitly fail when requested levels or cycle are in the past...
           or too far in the future...
           TODO-TB: this old comment (from version Alpha) conflicts with
           the specification of the RPCs that use this code.
        *)
        List.sort_uniq
          Level.compare
          (List.concat
             (List.map (Level.from_raw ctxt) levels
              :: List.map (Level.levels_in_cycle ctxt) cycles))

  module Baking_rights = struct
    type t = {
      level : Raw_level.t;
      delegate : Signature.Public_key_hash.t;
      round : int;
      timestamp : Timestamp.t option;
    }

    let encoding =
      let open Data_encoding in
      conv
        (fun {level; delegate; round; timestamp} ->
          (level, delegate, round, timestamp))
        (fun (level, delegate, round, timestamp) ->
          {level; delegate; round; timestamp})
        (obj4
           (req "level" Raw_level.encoding)
           (req "delegate" Signature.Public_key_hash.encoding)
           (req "round" uint16)
           (opt "estimated_time" Timestamp.encoding))

    let default_max_round = 64

    module S = struct
      open Data_encoding

      let path = RPC_path.(open_root / "helpers" / "baking_rights")

      type baking_rights_query = {
        levels : Raw_level.t list;
        cycle : Cycle.t option;
        delegates : Signature.Public_key_hash.t list;
        max_round : int option;
        all : bool;
      }

      let baking_rights_query =
        let open RPC_query in
        query (fun levels cycle delegates max_round all ->
            {levels; cycle; delegates; max_round; all})
        |+ multi_field "level" Raw_level.rpc_arg (fun t -> t.levels)
        |+ opt_field "cycle" Cycle.rpc_arg (fun t -> t.cycle)
        |+ multi_field "delegate" Signature.Public_key_hash.rpc_arg (fun t ->
               t.delegates)
        |+ opt_field "max_round" RPC_arg.int (fun t -> t.max_round)
        |+ flag "all" (fun t -> t.all)
        |> seal

      let baking_rights =
        RPC_service.get_service
          ~description:
            (Format.sprintf
               "Retrieves the list of delegates allowed to bake a block.\n\
                By default, it gives the best baking opportunities (in terms \
                of rounds) for bakers that have at least one opportunity below \
                the %dth round for the next block.\n\
                Parameters `level` and `cycle` can be used to specify the \
                (valid) level(s) in the past or future at which the baking \
                rights have to be returned.\n\
                Parameter `delegate` can be used to restrict the results to \
                the given delegates. If parameter `all` is set, all the baking \
                opportunities for each baker at each level are returned, \
                instead of just the first one.\n\
                Returns the list of baking opportunities up to round %d. Also \
                returns the minimal timestamps that correspond to these \
                opportunities. The timestamps are omitted for levels in the \
                past, and are only estimates for levels higher that the next \
                block's, based on the hypothesis that all predecessor blocks \
                were baked at the first round."
               default_max_round
               default_max_round)
          ~query:baking_rights_query
          ~output:(list encoding)
          path
    end

    let baking_rights_at_level ctxt max_round level =
      Baking.baking_rights ctxt level >>=? fun delegates ->
      Round.get ctxt >>=? fun current_round ->
      let current_level = Level.current ctxt in
      let current_timestamp = Timestamp.current ctxt in
      let round_durations = Constants.round_durations ctxt in
      let rec loop l acc round =
        if Compare.Int.(round > max_round) then return (List.rev acc)
        else
          let (Misc.LCons (pk, next)) = l in
          let delegate = Signature.Public_key.hash pk in
          estimated_time
            round_durations
            ~current_level
            ~current_round
            ~current_timestamp
            ~level
            ~round
          >>?= fun timestamp ->
          let acc = {level = level.level; delegate; round; timestamp} :: acc in
          next () >>=? fun l -> loop l acc (round + 1)
      in
      loop delegates [] 0

    let remove_duplicated_delegates rights =
      List.rev @@ fst
      @@ List.fold_left
           (fun (acc, previous) r ->
             if
               Signature.Public_key_hash.Set.exists
                 (Signature.Public_key_hash.equal r.delegate)
                 previous
             then (acc, previous)
             else
               (r :: acc, Signature.Public_key_hash.Set.add r.delegate previous))
           ([], Signature.Public_key_hash.Set.empty)
           rights

    let register () =
      Registration.register0 ~chunked:true S.baking_rights (fun ctxt q () ->
          let cycles =
            match q.cycle with None -> [] | Some cycle -> [cycle]
          in
          let levels =
            requested_levels
              ~default_level:(Level.succ ctxt (Level.current ctxt))
              ctxt
              cycles
              q.levels
          in
          let max_round =
            match q.max_round with
            | None -> default_max_round
            | Some max_round ->
                Compare.Int.min
                  max_round
                  (Constants.consensus_committee_size ctxt)
          in
          List.map_es (baking_rights_at_level ctxt max_round) levels
          >|=? fun rights ->
          let rights =
            if q.all then rights
            else List.map remove_duplicated_delegates rights
          in
          let rights = List.concat rights in
          match q.delegates with
          | [] -> rights
          | _ :: _ as delegates ->
              let is_requested p =
                List.exists
                  (Signature.Public_key_hash.equal p.delegate)
                  delegates
              in
              List.filter is_requested rights)

    let get ctxt ?(levels = []) ?cycle ?(delegates = []) ?(all = false)
        ?max_round block =
      RPC_context.make_call0
        S.baking_rights
        ctxt
        block
        {levels; cycle; delegates; max_round; all}
        ()
  end

  module Endorsing_rights = struct
    type delegate_rights = {
      delegate : Signature.Public_key_hash.t;
      first_slot : Slot.t;
      endorsing_power : int;
    }

    type t = {
      level : Raw_level.t;
      delegates_rights : delegate_rights list;
      estimated_time : Time.t option;
    }

    let delegate_rights_encoding =
      let open Data_encoding in
      conv
        (fun {delegate; first_slot; endorsing_power} ->
          (delegate, first_slot, endorsing_power))
        (fun (delegate, first_slot, endorsing_power) ->
          {delegate; first_slot; endorsing_power})
        (obj3
           (req "delegate" Signature.Public_key_hash.encoding)
           (req "first_slot" Slot.encoding)
           (req "endorsing_power" uint16))

    let encoding =
      let open Data_encoding in
      conv
        (fun {level; delegates_rights; estimated_time} ->
          (level, delegates_rights, estimated_time))
        (fun (level, delegates_rights, estimated_time) ->
          {level; delegates_rights; estimated_time})
        (obj3
           (req "level" Raw_level.encoding)
           (req "delegates" (list delegate_rights_encoding))
           (opt "estimated_time" Timestamp.encoding))

    module S = struct
      open Data_encoding

      let path = RPC_path.(path / "endorsing_rights")

      type endorsing_rights_query = {
        levels : Raw_level.t list;
        cycle : Cycle.t option;
        delegates : Signature.Public_key_hash.t list;
      }

      let endorsing_rights_query =
        let open RPC_query in
        query (fun levels cycle delegates -> {levels; cycle; delegates})
        |+ multi_field "level" Raw_level.rpc_arg (fun t -> t.levels)
        |+ opt_field "cycle" Cycle.rpc_arg (fun t -> t.cycle)
        |+ multi_field "delegate" Signature.Public_key_hash.rpc_arg (fun t ->
               t.delegates)
        |> seal

      let endorsing_rights =
        RPC_service.get_service
          ~description:
            "Retrieves the delegates allowed to endorse a block.\n\
             By default, it gives the endorsing power for delegates that have \
             at least one endorsing slot for the next block.\n\
             Parameters `level` and `cycle` can be used to specify the (valid) \
             level(s) in the past or future at which the endorsing rights have \
             to be returned. Parameter `delegate` can be used to restrict the \
             results to the given delegates.\n\
             Returns the smallest endorsing slots and the endorsing power. \
             Also returns the minimal timestamp that corresponds to endorsing \
             at the given level. The timestamps are omitted for levels in the \
             past, and are only estimates for levels higher that the next \
             block's, based on the hypothesis that all predecessor blocks were \
             baked at the first round."
          ~query:endorsing_rights_query
          ~output:(list encoding)
          path
    end

    let endorsing_rights_at_level ctxt level =
      Baking.endorsing_rights_by_first_slot ctxt level
      >>=? fun (ctxt, rights) ->
      Round.get ctxt >>=? fun current_round ->
      let current_level = Level.current ctxt in
      let current_timestamp = Timestamp.current ctxt in
      let round_durations = Constants.round_durations ctxt in
      estimated_time
        round_durations
        ~current_level
        ~current_round
        ~current_timestamp
        ~level
        ~round:0
      >>?= fun estimated_time ->
      let rights =
        Slot.Map.fold
          (fun first_slot (_pk, delegate, endorsing_power) acc ->
            {delegate; first_slot; endorsing_power} :: acc)
          rights
          []
      in
      return {level = level.level; delegates_rights = rights; estimated_time}

    let register () =
      Registration.register0 ~chunked:true S.endorsing_rights (fun ctxt q () ->
          let cycles =
            match q.cycle with None -> [] | Some cycle -> [cycle]
          in
          let levels =
            requested_levels
              ~default_level:(Level.current ctxt)
              ctxt
              cycles
              q.levels
          in
          List.map_es (endorsing_rights_at_level ctxt) levels
          >|=? fun rights_per_level ->
          match q.delegates with
          | [] -> rights_per_level
          | _ :: _ as delegates ->
              List.filter_map
                (fun rights_at_level ->
                  let is_requested p =
                    List.exists
                      (Signature.Public_key_hash.equal p.delegate)
                      delegates
                  in
                  match
                    List.filter is_requested rights_at_level.delegates_rights
                  with
                  | [] -> None
                  | delegates_rights ->
                      Some {rights_at_level with delegates_rights})
                rights_per_level)

    let get ctxt ?(levels = []) ?cycle ?(delegates = []) block =
      RPC_context.make_call0
        S.endorsing_rights
        ctxt
        block
        {levels; cycle; delegates}
        ()
  end

  module Validators = struct
    type t = {
      level : Raw_level.t;
      delegate : Signature.Public_key_hash.t;
      slots : Slot.t list;
    }

    let encoding =
      let open Data_encoding in
      conv
        (fun {level; delegate; slots} -> (level, delegate, slots))
        (fun (level, delegate, slots) -> {level; delegate; slots})
        (obj3
           (req "level" Raw_level.encoding)
           (req "delegate" Signature.Public_key_hash.encoding)
           (req "slots" (list Slot.encoding)))

    module S = struct
      open Data_encoding

      let path = RPC_path.(path / "validators")

      type validators_query = {
        levels : Raw_level.t list;
        delegates : Signature.Public_key_hash.t list;
      }

      let validators_query =
        let open RPC_query in
        query (fun levels delegates -> {levels; delegates})
        |+ multi_field "level" Raw_level.rpc_arg (fun t -> t.levels)
        |+ multi_field "delegate" Signature.Public_key_hash.rpc_arg (fun t ->
               t.delegates)
        |> seal

      let validators =
        RPC_service.get_service
          ~description:
            "Retrieves the delegates allowed to endorse a block.\n\
             By default, it gives the endorsing slots for delegates that have \
             at least one in the next block.\n\
             Parameter `level` can be used to specify the (valid) level(s) in \
             the past or future at which the endorsement rights have to be \
             returned. Parameter `delegate` can be used to restrict the \
             results to the given delegates.\n\
             Returns the list of endorsing slots. Also returns the minimal \
             timestamps that correspond to these slots. The timestamps are \
             omitted for levels in the past, and are only estimates for levels \
             later that the next block, based on the hypothesis that all \
             predecessor blocks were baked at the first round."
          ~query:validators_query
          ~output:(list encoding)
          path
    end

    let endorsing_slots_at_level ctxt level =
      Baking.endorsing_rights ctxt level >|=? fun (_, rights) ->
      Signature.Public_key_hash.Map.fold
        (fun delegate slots acc ->
          {level = level.level; delegate; slots} :: acc)
        rights
        []

    let register () =
      Registration.register0 ~chunked:true S.validators (fun ctxt q () ->
          let levels =
            requested_levels
              ~default_level:(Level.current ctxt)
              ctxt
              []
              q.levels
          in
          List.map_es (endorsing_slots_at_level ctxt) levels >|=? fun rights ->
          let rights = List.concat rights in
          match q.delegates with
          | [] -> rights
          | _ :: _ as delegates ->
              let is_requested p =
                List.exists
                  (Signature.Public_key_hash.equal p.delegate)
                  delegates
              in
              List.filter is_requested rights)

    let get ctxt ?(levels = []) ?(delegates = []) block =
      RPC_context.make_call0 S.validators ctxt block {levels; delegates} ()
  end

  module S = struct
    open Data_encoding

    type level_query = {offset : int32}

    let level_query : level_query RPC_query.t =
      let open RPC_query in
      query (fun offset -> {offset})
      |+ field "offset" RPC_arg.int32 0l (fun t -> t.offset)
      |> seal

    let current_level =
      RPC_service.get_service
        ~description:
          "Returns the level of the interrogated block, or the one of a block \
           located `offset` blocks after in the chain (or before when \
           negative). For instance, the next block if `offset` is 1."
        ~query:level_query
        ~output:Level.encoding
        RPC_path.(path / "current_level")

    let levels_in_current_cycle =
      RPC_service.get_service
        ~description:"Levels of a cycle"
        ~query:level_query
        ~output:
          (obj2
             (req "first" Raw_level.encoding)
             (req "last" Raw_level.encoding))
        RPC_path.(path / "levels_in_current_cycle")

    let round =
      RPC_service.get_service
        ~description:
          "Returns the round of the interrogated block, or the one of a block \
           located `offset` blocks after in the chain (or before when \
           negative). For instance, the next block if `offset` is 1."
        ~query:RPC_query.empty
        ~output:Round.encoding
        RPC_path.(path / "round")
  end

  let register () =
    Scripts.register () ;
    Forge.register () ;
    Parse.register () ;
    Contract.register () ;
    Big_map.register () ;
    Baking_rights.register () ;
    Endorsing_rights.register () ;
    Validators.register () ;
    Registration.register0 ~chunked:false S.current_level (fun ctxt q () ->
        Lwt.return
          (Level.from_raw_with_offset
             ctxt
             ~offset:q.offset
             (Level.current ctxt).level)) ;
    Registration.opt_register0
      ~chunked:true
      S.levels_in_current_cycle
      (fun ctxt q () ->
        let rev_levels =
          Level.levels_in_current_cycle ctxt ~offset:q.offset ()
        in
        match rev_levels with
        | [] -> return_none
        | [level] -> return (Some (level.level, level.level))
        | last :: default_first :: rest ->
            (* The [rev_levels] list is reversed, the last level is the head *)
            let first = List.last default_first rest in
            return (Some (first.level, last.level))) ;
    Registration.register0 ~chunked:false S.round (fun ctxt () () ->
        Round.get ctxt)

  let current_level ctxt ?(offset = 0l) block =
    RPC_context.make_call0 S.current_level ctxt block {offset} ()

  let levels_in_current_cycle ctxt ?(offset = 0l) block =
    RPC_context.make_call0 S.levels_in_current_cycle ctxt block {offset} ()

  let rpc_services =
    register () ;
    RPC_directory.merge rpc_services !Registration.patched_services
end
