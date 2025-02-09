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

open Lib_test.Qcheck_helpers
open Plugin.Mempool
open Tezos_protocol_alpha.Protocol.Alpha_context
open Tezos_protocol_alpha.Protocol

let config drift_opt =
  {
    minimal_fees = default_minimal_fees;
    minimal_nanotez_per_gas_unit = default_minimal_nanotez_per_gas_unit;
    minimal_nanotez_per_byte = default_minimal_nanotez_per_byte;
    allow_script_failure = true;
    clock_drift =
      Option.map
        (fun drift -> Period.of_seconds_exn (Int64.of_int drift))
        drift_opt;
  }

type Environment.Error_monad.error += Generation_failure

(** Conversion helpers  *)

let int32_of_timestamp ts =
  let i64 = Timestamp.to_seconds ts in
  let i32 = Int64.to_int32 i64 in
  if Int64.(equal (of_int32 i32) i64) then Ok i32
  else Environment.Error_monad.error Generation_failure

let int32_of_timestamp_exn ts =
  match int32_of_timestamp ts with
  | Ok i32 -> i32
  | Error _err -> Stdlib.failwith "int32_of_timestamp_exn: number too big"

let timestamp_of_int32 ts = Timestamp.of_seconds (Int64.of_int32 ts)

(** Data Generators *)
module Generator = struct
  open QCheck

  let decorate ?(prefix = "") ?(suffix = "") printer gen =
    set_print (fun d -> prefix ^ printer d ^ suffix) gen

  let config =
    decorate ~prefix:"clock_drift " (fun config ->
        Option.fold
          ~none:"round_0 duration"
          ~some:(fun drift -> Int64.to_string @@ Period.to_seconds drift)
          config.clock_drift)
    @@ map
         ~rev:(fun {clock_drift; _} ->
           Option.map
             (fun drift -> Int64.to_int @@ Period.to_seconds drift)
             clock_drift)
         config
         (option small_nat)

  let of_error_arb gen =
    of_option_arb
    @@ map
         ~rev:(function
           | Some x -> Ok x
           | None -> Environment.Error_monad.error Generation_failure)
         (function Ok x -> Some x | Error _err -> None)
         gen

  let small_nat_32 ?prefix ?suffix () =
    decorate ?prefix ?suffix Int32.to_string
    @@ map ~rev:Int32.to_int Int32.of_int small_nat

  let small_signed_64 ?prefix ?suffix () =
    decorate ?prefix ?suffix Int64.to_string
    @@ map ~rev:Int64.to_int Int64.of_int small_signed_int

  let small_signed_32 ?prefix ?suffix () =
    decorate ?prefix ?suffix Int32.to_string
    @@ map ~rev:Int32.to_int Int32.of_int small_signed_int

  let decorated_small_nat ?prefix ?suffix () =
    decorate ?prefix ?suffix string_of_int small_nat

  let dup gen = map ~rev:fst (fun x -> (x, x)) gen

  let round =
    of_error_arb
    @@ map
         ~rev:(function Ok l -> Round.to_int32 l | Error _ -> -1l)
         (fun i32 -> Round.of_int32 i32)
         (small_nat_32 ~prefix:"rnd " ())

  let same_rounds = dup round

  let level =
    of_error_arb
    @@ map
         ~rev:(function Ok l -> Raw_level.to_int32 l | Error _ -> -1l)
         Raw_level.of_int32
         (small_nat_32 ~prefix:"lev " ())

  let same_levels = dup level

  let timestamp =
    set_print Timestamp.to_notation
    @@ map ~rev:int32_of_timestamp_exn (fun f -> timestamp_of_int32 f) int32

  let near_timestamps =
    map
      ~rev:(fun (ts1, ts2) ->
        let its1 = int32_of_timestamp_exn ts1 in
        let its2 = int32_of_timestamp_exn ts2 in
        Int32.(its1, sub its2 its1))
      (fun (i, diff) ->
        timestamp_of_int32 i |> fun ts1 ->
        timestamp_of_int32 Int32.(add i diff) |> fun ts2 -> (ts1, ts2))
      (pair int32 (small_signed_32 ~prefix:"+" ~suffix:"sec." ()))

  let dummy_timestamp =
    match Timestamp.of_seconds_string "0" with
    | Some ts -> ts
    | _ -> assert false

  let unsafe_sub ts1 ts2 =
    Int64.to_int @@ Period.to_seconds
    @@
    match Timestamp.(ts1 -? ts2) with
    | Ok diff -> diff
    | Error _ -> assert false

  let successive_timestamp =
    of_error_arb
    @@ map
         ~rev:(function
           | Ok (ts1, ts2) -> (ts1, unsafe_sub ts2 ts1)
           | Error _ -> (dummy_timestamp, -1))
         (fun (ts, (diff : int)) ->
           Period.of_seconds (Int64.of_int diff) >>? fun diff ->
           Timestamp.(ts +? diff) >>? fun ts2 -> Ok (ts, ts2))
         (pair timestamp (decorated_small_nat ~prefix:"+" ~suffix:"sec." ()))

  let param_acceptable ?(rounds = pair round round) ?(levels = pair level level)
      ?(timestamps = near_timestamps) () =
    pair config (pair (pair rounds levels) timestamps)
end

let assert_no_error d = match d with Error _ -> assert false | Ok d -> d

(** Constants :
    This could be generated but it would largely increase the search space. *)
let round_durations : Round.round_durations =
  assert_no_error
  @@ Round.Durations.create
       ~round0:Period.(of_seconds_exn 4L)
       ~round1:Period.(of_seconds_exn 14L)
       ()

let round_zero_duration = Round.round_duration round_durations Round.zero

(** Don't allow test to fail  *)
let no_error = function
  | Ok b -> b
  | Error errs ->
      Format.printf
        "test fail due to error : %a@."
        Error_monad.pp_print_trace
        (Environment.wrap_tztrace errs) ;
      false

(** Helper to compute  *)
let durations round_durations start stop =
  List.map_e
    (fun round ->
      Round.of_int round >|? fun round ->
      Round.round_duration round_durations round |> Period.to_seconds)
    Tezos_stdlib.Utils.Infix.(start -- stop)

(** Expected timestamp for the begining of a round at same level that
    the proposal.

    It has been developped before the  Round.timestamp_of_round_same_level and has a
    different implementation.

*)
let timestamp_of_round round_durations ~proposal_timestamp ~proposal_round
    ~round =
  (let iproposal_round = Int32.to_int @@ Round.to_int32 proposal_round in
   let iround = Int32.to_int @@ Round.to_int32 round in
   if Round.(proposal_round = round) then ok (Period.zero, proposal_timestamp)
   else if Round.(proposal_round < round) then
     durations round_durations iproposal_round (iround - 1) >>? fun durations ->
     Period.of_seconds @@ List.fold_left Int64.add Int64.zero durations
     >>? fun rounds_duration ->
     Timestamp.(proposal_timestamp +? rounds_duration) >|? fun ts ->
     (rounds_duration, ts)
   else
     durations round_durations iround (iproposal_round - 1) >>? fun durations ->
     List.fold_left Int64.add Int64.zero durations |> fun rounds_duration ->
     Timestamp.of_seconds
     @@ Int64.sub (Timestamp.to_seconds proposal_timestamp) rounds_duration
     |> fun ts ->
     Period.of_seconds rounds_duration >|? fun rounds_duration ->
     (rounds_duration, ts))
  >>? fun (_rnd_dur, exp_ts) -> ok exp_ts

let drift_of =
  let r0_dur = Round.round_duration round_durations Round.zero in
  fun clock_drift -> Option.value ~default:r0_dur clock_drift

(** [max_ts] computes the upper bound on future timestamps given the
    accepted round drift.
*)
let max_ts clock_drift prop_ts now =
  Timestamp.(max prop_ts now +? drift_of clock_drift)

let count = None

let predecessor_start proposal_timestamp proposal_round grandparent_round =
  assert_no_error
  @@ ( Round.level_offset_of_round
         round_durations
         ~round:Round.(succ grandparent_round)
     >>? fun proposal_level_offset ->
       Round.level_offset_of_round round_durations ~round:proposal_round
       >>? fun proposal_round_offset ->
       Period.(add proposal_level_offset proposal_round_offset)
       >>? fun proposal_offset ->
       Ok Timestamp.(proposal_timestamp - proposal_offset) )

(** TESTS   *)

(** Test past operations that are accepted whatever the current timestamp is:
   strictly before the predecessor level or at the current level and with a
   strictly lower round than the head. *)

let test_acceptable_past_level =
  QCheck.Test.make
    ~name:"acceptable past op "
    (Generator.param_acceptable ())
    (fun
      ( config,
        ( ((proposal_round, op_round), (proposal_level, op_level)),
          (proposal_timestamp, now_timestamp) ) )
    ->
      QCheck.(
        Raw_level.(
          proposal_level > succ op_level
          || (proposal_level = op_level && Round.(proposal_round > op_round)))
        ==> no_error
            @@ acceptable_op
                 ~config
                 ~round_durations
                 ~round_zero_duration
                 ~proposal_level
                 ~proposal_round
                 ~proposal_timestamp
                 ~proposal_predecessor_level_start:
                   (predecessor_start
                      proposal_timestamp
                      proposal_round
                      Round.zero)
                 ~op_level
                 ~op_round
                 ~now_timestamp))

(** Test acceptable operations at current level, current round, i.e. on the
   currently considered proposal *)
let test_acceptable_current_level_current_round =
  let open QCheck in
  Test.make
    ?count
    ~name:"same round, same level "
    Generator.(param_acceptable ~rounds:same_rounds ~levels:same_levels ())
    (fun ( config,
           (((op_round, _), (_, op_level)), (proposal_timestamp, now_timestamp))
         ) ->
      let proposal_level = op_level in
      let proposal_round = op_round in
      no_error
      @@ acceptable_op
           ~config
           ~round_durations
           ~round_zero_duration
           ~proposal_level
           ~proposal_round
           ~proposal_timestamp
           ~proposal_predecessor_level_start:
             (predecessor_start proposal_timestamp proposal_round Round.zero)
           ~op_level
           ~op_round
           ~now_timestamp)

(** Test operations at same level, different round, with an acceptable expected
    timestamp for the operation.   *)
let test_acceptable_current_level =
  let open QCheck in
  Test.make
    ?count
    ~name:"same level, different round, acceptable op"
    Generator.(param_acceptable ~levels:same_levels ())
    (fun ( config,
           ( ((proposal_round, op_round), (_, op_level)),
             (proposal_timestamp, now_timestamp) ) ) ->
      let proposal_level = op_level in
      no_error
        ( timestamp_of_round
            round_durations
            ~proposal_timestamp
            ~proposal_round
            ~round:op_round
        >>? fun expected_time ->
          max_ts config.clock_drift proposal_timestamp now_timestamp
          >>? fun max_timestamp -> ok Timestamp.(expected_time <= max_timestamp)
        )
      ==> no_error
          @@ acceptable_op
               ~config
               ~round_durations
               ~round_zero_duration
               ~proposal_level
               ~proposal_round
               ~proposal_timestamp
               ~proposal_predecessor_level_start:
                 (predecessor_start
                    proposal_timestamp
                    proposal_round
                    Round.zero)
               ~op_level
               ~op_round
               ~now_timestamp)

(** Test operations at same level, different round, with a too high expected
    timestamp for the operation,  and not at current round (which is always accepted). *)
let test_not_acceptable_current_level =
  let open QCheck in
  Test.make
    ?count
    ~name:"same level, different round, too far"
    Generator.(param_acceptable ~levels:same_levels ())
    (fun ( config,
           ( ((proposal_round, op_round), (_, op_level)),
             (proposal_timestamp, now_timestamp) ) ) ->
      let proposal_level = op_level in
      no_error
        ( timestamp_of_round
            round_durations
            ~proposal_timestamp
            ~proposal_round
            ~round:op_round
        >>? fun expected_time ->
          max_ts config.clock_drift proposal_timestamp now_timestamp
          >>? fun max_timestamp ->
          ok
            Timestamp.(
              expected_time > max_timestamp
              && Round.(proposal_round <> op_round)) )
      ==> no_error
            (acceptable_op
               ~config
               ~round_durations
               ~round_zero_duration
               ~proposal_level
               ~proposal_round
               ~proposal_timestamp
               ~proposal_predecessor_level_start:
                 (predecessor_start
                    proposal_timestamp
                    proposal_round
                    Round.zero)
               ~op_level
               ~op_round
               ~now_timestamp
            >|? not))

(** Test operations at next level, different round, with an acceptable timestamp for
    the operation. *)
let test_acceptable_next_level =
  let open QCheck in
  Test.make
    ?count
    ~name:"next level, acceptable op"
    Generator.(param_acceptable ~levels:same_levels ())
    (fun ( config,
           ( ((proposal_round, op_round), (proposal_level, _)),
             (proposal_timestamp, now_timestamp) ) ) ->
      let op_level = Raw_level.succ proposal_level in
      no_error
        ( timestamp_of_round
            round_durations
            ~proposal_timestamp
            ~proposal_round
            ~round:Round.zero
        >>? fun current_level_start ->
          Round.timestamp_of_round
            round_durations
            ~predecessor_timestamp:current_level_start
            ~predecessor_round:Round.zero
            ~round:op_round
          >>? fun expected_time ->
          max_ts config.clock_drift proposal_timestamp now_timestamp
          >>? fun max_timestamp -> ok Timestamp.(expected_time <= max_timestamp)
        )
      ==> no_error
          @@ acceptable_op
               ~config
               ~round_durations
               ~round_zero_duration
               ~proposal_level
               ~proposal_round
               ~proposal_timestamp
               ~proposal_predecessor_level_start:
                 (predecessor_start
                    proposal_timestamp
                    proposal_round
                    Round.zero)
               ~op_level
               ~op_round
               ~now_timestamp)

(** Test operations at next level, different round, with a too high timestamp
   for the operation. *)
let test_not_acceptable_next_level =
  let open QCheck in
  Test.make
    ?count
    ~name:"next level, too far"
    Generator.(
      param_acceptable ~levels:same_levels ~timestamps:successive_timestamp ())
    (fun ( config,
           ( ((proposal_round, op_round), (proposal_level, _)),
             (proposal_timestamp, now_timestamp) ) ) ->
      let op_level = Raw_level.succ proposal_level in
      QCheck.assume
      @@ no_error
           ( timestamp_of_round
               round_durations
               ~proposal_timestamp
               ~proposal_round
               ~round:Round.zero
           >>? fun current_level_start ->
             Round.timestamp_of_round
               round_durations
               ~predecessor_timestamp:current_level_start
               ~predecessor_round:Round.zero
               ~round:op_round
             >>? fun expected_time ->
             Timestamp.(
               proposal_timestamp
               +? Round.round_duration round_durations proposal_round)
             >>? fun next_level_ts ->
             max_ts config.clock_drift next_level_ts now_timestamp
             >>? fun max_timestamp ->
             ok Timestamp.(expected_time > max_timestamp) ) ;
      no_error
      @@ (acceptable_op
            ~config
            ~round_durations
            ~round_zero_duration
            ~proposal_level
            ~proposal_round
            ~proposal_timestamp
            ~proposal_predecessor_level_start:
              (predecessor_start proposal_timestamp proposal_round Round.zero)
            ~op_level
            ~op_round
            ~now_timestamp
         >|? not))

let tests =
  Alcotest.run
    "Filter"
    [
      ( "pre_filter",
        qcheck_wrap
          [
            test_acceptable_past_level;
            test_acceptable_current_level_current_round;
            test_acceptable_current_level;
            test_not_acceptable_current_level;
            test_acceptable_next_level;
            test_not_acceptable_next_level;
          ] );
    ]
