(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(* Testing
   -------
   Component:    Shell
   Invocation:   dune exec src/lib_shell/test/test.exe test "synchronisation heuristic"
   Subject:      Test the synchronisation heuristic
*)

(* In the following we will use:

   - `Sync` for Synchronised with `is_chain_stuck` is `false`

   - `Stuck` for Synchronised with `is_chain_stuck` is `true`

   - `Unsync` for `Not_synchronised`

   A value refers to a pair for a timestamp and a peer. A value in the
   past means a value which is more than `latency` in the past where
   `latency` is a parameter of the test.

   By default, for each value, we generate a timestamp using the
   current time when the test is executed. Using a `latency` of `100`
   assumes that each unit test takes less than `100` to be
   executed. *)

open Synchronisation_heuristic

let prn = function
  | Synchronised {is_chain_stuck = true} ->
      "Synchronised (stuck)"
  | Not_synchronised ->
      "Not synchronised"
  | Synchronised {is_chain_stuck = false} ->
      "Synchronised (not stuck)"

let forge_peer_id () =
  let identity = P2p_identity.generate_with_target_0 () in
  identity.peer_id

let forge_timestamp ?(delay = 0) () =
  let time = Time.System.to_protocol @@ Systime_os.now () in
  Time.Protocol.add time (Int64.of_int delay)

(* NOTE: timestamp supersedes delay *)
let forge_value ?delay ?timestamp ?peer () =
  let peer = match peer with Some peer -> peer | None -> forge_peer_id () in
  let timestamp =
    match timestamp with
    | Some timestamp ->
        timestamp
    | None ->
        forge_timestamp ?delay ()
  in
  (timestamp, peer)

(* Test.
   Check the status is `Sync` when the threshold is negative. *)
let test_threshold_negative () =
  let heuristic = create ~threshold:(-1) ~latency:120 in
  Assert.equal ~prn (get_status heuristic) Not_synchronised ;
  update heuristic @@ forge_value () ;
  Assert.equal ~prn (get_status heuristic) Not_synchronised ;
  update heuristic @@ forge_value () ;
  update heuristic @@ forge_value () ;
  update heuristic @@ forge_value () ;
  Assert.equal ~prn (get_status heuristic) Not_synchronised

(* Test.
   Check the status is `Sync` when the threshold is zero. *)
let test_threshold_is_zero () =
  let heuristic = create ~threshold:0 ~latency:120 in
  Assert.equal
    ~prn
    (get_status heuristic)
    (Synchronised {is_chain_stuck = false}) ;
  update heuristic @@ forge_value () ;
  Assert.equal
    ~prn
    (get_status heuristic)
    (Synchronised {is_chain_stuck = false}) ;
  update heuristic @@ forge_value () ;
  update heuristic @@ forge_value () ;
  update heuristic @@ forge_value () ;
  Assert.equal
    ~prn
    (get_status heuristic)
    (Synchronised {is_chain_stuck = false})

(* Test.

   When the threshold is one, Check that:

   1. The status is first `Unsync`

   2. After adding one or more values, the status is `Sync`
*)
let test_threshold_is_one () =
  let heuristic = create ~threshold:1 ~latency:120 in
  Assert.equal ~prn Not_synchronised (get_status heuristic) ;
  update heuristic @@ forge_value () ;
  Assert.equal
    ~prn
    (Synchronised {is_chain_stuck = false})
    (get_status heuristic) ;
  update heuristic @@ forge_value () ;
  update heuristic @@ forge_value () ;
  Assert.equal
    ~prn
    (Synchronised {is_chain_stuck = false})
    (get_status heuristic) ;
  update heuristic @@ forge_value () ;
  Assert.equal
    ~prn
    (Synchronised {is_chain_stuck = false})
    (get_status heuristic) ;
  update heuristic @@ forge_value () ;
  update heuristic @@ forge_value () ;
  Assert.equal
    ~prn
    (Synchronised {is_chain_stuck = false})
    (get_status heuristic)

(* Test.

   When the threshold is one, check that:

   1. The status is `Unsync`

   2. After adding a value, the status is `Sync`

   3. After adding a value in the past, the status is still `Sync`
*)
let test_threshold_is_one_update_in_the_past () =
  let latency = 120 in
  let heuristic = create ~threshold:1 ~latency in
  Assert.equal ~prn Not_synchronised (get_status heuristic) ;
  let peer = forge_peer_id () in
  update heuristic @@ forge_value ~peer () ;
  Assert.equal
    ~prn
    (Synchronised {is_chain_stuck = false})
    (get_status heuristic) ;
  update heuristic @@ forge_value ~delay:(latency * -2) ~peer () ;
  Assert.equal
    ~prn
    (Synchronised {is_chain_stuck = false})
    (get_status heuristic) ;
  update heuristic @@ forge_value ~delay:(latency * -10) ~peer () ;
  Assert.equal
    ~prn
    (Synchronised {is_chain_stuck = false})
    (get_status heuristic)

(* Test.

   When the threshold is one,  check that:

   1. The status is `Unsync`

   2. After adding a value in the past, the status is still `Unsync`
*)
let test_threshold_is_one_value_in_the_past () =
  let latency = 120 in
  let heuristic = create ~threshold:1 ~latency in
  Assert.equal ~prn Not_synchronised (get_status heuristic) ;
  update heuristic @@ forge_value ~delay:(latency * -2) () ;
  Assert.equal ~prn Not_synchronised (get_status heuristic) ;
  update heuristic @@ forge_value ~delay:(latency * -10) () ;
  Assert.equal ~prn Not_synchronised (get_status heuristic)

(* Test.

   When the threshold is one, check that:

   1. The status is `Unsync`

   2. After adding a value, the status is `Sync`

   3. After adding a value for the same peer with an old timestamp the
   status is still `Sync` *)
let test_threshold_is_one_always_takes_best_timestamp () =
  let latency = 120 in
  let heuristic = create ~threshold:1 ~latency in
  Assert.equal ~prn Not_synchronised (get_status heuristic) ;
  update heuristic @@ forge_value () ;
  Assert.equal
    ~prn
    (Synchronised {is_chain_stuck = false})
    (get_status heuristic) ;
  update heuristic @@ forge_value ~delay:(latency * -2) () ;
  Assert.equal
    ~prn
    (Synchronised {is_chain_stuck = false})
    (get_status heuristic) ;
  update heuristic @@ forge_value ~delay:(latency * -2) () ;
  Assert.equal
    ~prn
    (Synchronised {is_chain_stuck = false})
    (get_status heuristic) ;
  update heuristic @@ forge_value ~delay:(latency * -10) () ;
  Assert.equal
    ~prn
    (Synchronised {is_chain_stuck = false})
    (get_status heuristic)

(* Test.

   When the treshold is two, check that:

   1. The status is `Unsync`

   2. After adding a value, the status is still `Unsync`

   3. After adding another value from another peer, the status is
   `Sync`

   4. After adding more values (including in the past, from other peers), the
   status still is `Sync`

   *)
let test_threshold_is_two () =
  let latency = 120 in
  let heuristic = create ~threshold:2 ~latency in
  Assert.equal ~prn Not_synchronised (get_status heuristic) ;
  let peer = forge_peer_id () in
  update heuristic @@ forge_value ~peer () ;
  Assert.equal ~prn Not_synchronised (get_status heuristic) ;
  update heuristic @@ forge_value () ;
  Assert.equal
    ~prn
    (Synchronised {is_chain_stuck = false})
    (get_status heuristic) ;
  update heuristic @@ forge_value ~delay:(latency * -2) () ;
  update heuristic @@ forge_value ~peer ~delay:(latency * -2) () ;
  update heuristic @@ forge_value ~delay:(latency * -10) () ;
  Assert.equal
    ~prn
    (Synchronised {is_chain_stuck = false})
    (get_status heuristic)

(* Test.

   When the threshold is two, check that:

   1. The status is `Unsync`

   2. After adding a value, the status is still `Unsync`

   3. After adding a value in the past, the status is still `Unsync`

   4. Adding adding a new value value with the same peer as step 3,
   the status is `Sync` *)
let test_threshold_is_two_one_in_the_past () =
  let latency = 120 in
  let heuristic = create ~threshold:2 ~latency in
  Assert.equal ~prn Not_synchronised (get_status heuristic) ;
  update heuristic @@ forge_value () ;
  Assert.equal ~prn Not_synchronised (get_status heuristic) ;
  let peer = forge_peer_id () in
  update heuristic @@ forge_value ~peer ~delay:(latency * -2) () ;
  Assert.equal ~prn Not_synchronised (get_status heuristic) ;
  update heuristic @@ forge_value ~peer () ;
  Assert.equal
    ~prn
    (Synchronised {is_chain_stuck = false})
    (get_status heuristic)

(* Test.

   When the threshold is two, check that:

   1. The status is `Unsync`

   2. After adding a value, the status is still `Unsync`

   3. After adding a value from the same peer, the status is still `Unsync` *)
let test_threshold_is_two_one_in_the_past_and_one_more () =
  let latency = 120 in
  let heuristic = create ~threshold:2 ~latency in
  Assert.equal ~prn Not_synchronised (get_status heuristic) ;
  let peer = forge_peer_id () in
  update heuristic @@ forge_value ~peer ~delay:(-3) () ;
  Assert.equal ~prn Not_synchronised (get_status heuristic) ;
  update heuristic @@ forge_value ~peer ~delay:(-1) () ;
  Assert.equal ~prn Not_synchronised (get_status heuristic)

(* Test.

   When the threshold is two, check that:

   1. The status is `Unsync`

   2. After adding a value in the past, the status is still `Unsync`

   3. After adding another value with the same timestamp but a different peer,
   the status is `Stuck`.

   4. After a more recent value, the status is `Unsync`.
   *)
let test_threshold_is_two_two_in_the_past () =
  let latency = 120 in
  let heuristic = create ~threshold:2 ~latency in
  Assert.equal ~prn Not_synchronised (get_status heuristic) ;
  let timestamp = forge_timestamp ~delay:(latency * -3) () in
  update heuristic @@ forge_value ~timestamp () ;
  Assert.equal ~prn Not_synchronised (get_status heuristic) ;
  update heuristic @@ forge_value ~timestamp () ;
  Assert.equal
    ~prn
    (Synchronised {is_chain_stuck = true})
    (get_status heuristic) ;
  update heuristic @@ forge_value ~delay:(latency * -2) () ;
  Assert.equal ~prn Not_synchronised (get_status heuristic)

(* Test.

   When the threshold is three, check that:

   1. The status is `Unsync`

   2. After adding a value, the status is still `Unsync`

   3. After adding another value, the status is still `Unsync`

   4. After adding another value, the status is `Sync`
*)
let test_threshold_is_three () =
  let latency = 120 in
  let heuristic = create ~threshold:3 ~latency in
  Assert.equal ~prn Not_synchronised (get_status heuristic) ;
  update heuristic @@ forge_value () ;
  Assert.equal ~prn Not_synchronised (get_status heuristic) ;
  update heuristic @@ forge_value () ;
  Assert.equal ~prn Not_synchronised (get_status heuristic) ;
  update heuristic @@ forge_value () ;
  Assert.equal
    ~prn
    (Synchronised {is_chain_stuck = false})
    (get_status heuristic)

(* Test.

   When the threshold is three, check that:

   1. The status is `Unsync`

   2. After adding a value `(t1, peer1)` with t1 in the past, the
   status is still `Unsync`

   3. After adding a value `(t1, peer2)`, the status is still `Unsync`

   4. After adding a value `(t1, peer3)`, the status is `Stuck`

   5. After adding a value `(t2, peer1)`, the status is `Unsync`

   6. After adding a value `(t3, peer2)`, the status is `Unsync`

   7. After adding a value `(t4, peer3)`, the status is `Sync` *)
let test_threshold_is_three_and_stuck () =
  let latency = 120 in
  let heuristic = create ~threshold:3 ~latency in
  Assert.equal ~prn Not_synchronised (get_status heuristic) ;
  let peer1 = forge_peer_id () in
  let timestamp = forge_timestamp ~delay:(latency * -2) () in
  update heuristic @@ forge_value ~peer:peer1 ~timestamp () ;
  Assert.equal ~prn Not_synchronised (get_status heuristic) ;
  let peer2 = forge_peer_id () in
  update heuristic @@ forge_value ~peer:peer2 ~delay:(latency * -2) () ;
  Assert.equal ~prn Not_synchronised (get_status heuristic) ;
  let peer3 = forge_peer_id () in
  update heuristic @@ forge_value ~peer:peer3 ~timestamp () ;
  Assert.equal
    ~prn
    (Synchronised {is_chain_stuck = true})
    (get_status heuristic) ;
  update heuristic @@ forge_value ~peer:peer1 () ;
  Assert.equal ~prn Not_synchronised (get_status heuristic) ;
  update heuristic @@ forge_value ~peer:peer2 () ;
  Assert.equal ~prn Not_synchronised (get_status heuristic) ;
  update heuristic @@ forge_value ~peer:peer3 () ;
  Assert.equal
    ~prn
    (Synchronised {is_chain_stuck = false})
    (get_status heuristic)

(* This counterexample was generated by crowbar in a previous version
   of the synchronisation heuristic. *)
let test_counterexample_1 () =
  let latency = 100 in
  let heuristic = create ~threshold:3 ~latency in
  let p9 = forge_peer_id () in
  let fresh = forge_peer_id () in
  let p2 = forge_peer_id () in
  let p7 = forge_peer_id () in
  let p8 = forge_peer_id () in
  let delay = -279 in
  let peer = p9 in
  update heuristic @@ forge_value ~peer ~delay () ;
  let delay = -273 in
  let peer = fresh in
  update heuristic @@ forge_value ~peer ~delay () ;
  let delay = -200 in
  let peer = p2 in
  update heuristic @@ forge_value ~peer ~delay () ;
  let delay = -123 in
  let peer = p9 in
  update heuristic @@ forge_value ~peer ~delay () ;
  let delay = -162 in
  let peer = p7 in
  update heuristic @@ forge_value ~peer ~delay () ;
  let delay = -50 in
  let peer = p8 in
  update heuristic @@ forge_value ~peer ~delay () ;
  Assert.equal ~prn Not_synchronised (get_status heuristic)

let tests_raw : (string * (unit -> unit)) list =
  [ ("Threshold negative", test_threshold_negative);
    ("Threshold is zero", test_threshold_is_zero);
    ("Threshold is one, update in the present", test_threshold_is_one);
    ( "Threshold is one, update in the past",
      test_threshold_is_one_update_in_the_past );
    ( "Threshold is one, best timestamp in the past",
      test_threshold_is_one_value_in_the_past );
    ( "Threshold is one, update in the past does not erase update in the present",
      test_threshold_is_one_always_takes_best_timestamp );
    ("Threshold is two", test_threshold_is_two);
    ( "Threshold is two, one peer in the past",
      test_threshold_is_two_one_in_the_past );
    ( "Threshold is two, one peer in the past twice",
      test_threshold_is_two_one_in_the_past_and_one_more );
    ( "test_threshold is two, two peers in the past",
      test_threshold_is_two_two_in_the_past );
    ("Threshold is three", test_threshold_is_three);
    ("Threshold is three, three in the past", test_threshold_is_three_and_stuck);
    ("Crowbar counterexample 1", test_counterexample_1) ]

let tests =
  List.map (fun (s, f) -> Alcotest_lwt.test_case_sync s `Quick f) tests_raw
