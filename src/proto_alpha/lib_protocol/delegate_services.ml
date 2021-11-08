(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
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

open Alpha_context

type error += Balance_rpc_non_delegate of public_key_hash

let () =
  register_error_kind
    `Temporary
    ~id:"delegate_service.balance_rpc_on_non_delegate"
    ~title:"Balance request for an unregistered delegate"
    ~description:"The account whose balance was requested is not a delegate."
    ~pp:(fun ppf pkh ->
      Format.fprintf
        ppf
        "The implicit account (%a) whose balance was requested is not a \
         registered delegate. To get the balance of this account you can use \
         the ../context/contracts/%a/balance RPC."
        Signature.Public_key_hash.pp
        pkh
        Signature.Public_key_hash.pp
        pkh)
    Data_encoding.(obj1 (req "pkh" Signature.Public_key_hash.encoding))
    (function Balance_rpc_non_delegate pkh -> Some pkh | _ -> None)
    (fun pkh -> Balance_rpc_non_delegate pkh)

type info = {
  full_balance : Tez.t;
  frozen_deposits : Tez.t;
  staking_balance : Tez.t;
  frozen_deposits_limit : Tez.t option;
  delegated_contracts : Contract.t list;
  delegated_balance : Tez.t;
  deactivated : bool;
  grace_period : Cycle.t;
  voting_power : int32;
}

let info_encoding =
  let open Data_encoding in
  conv
    (fun {
           full_balance;
           frozen_deposits;
           staking_balance;
           frozen_deposits_limit;
           delegated_contracts;
           delegated_balance;
           deactivated;
           grace_period;
           voting_power;
         } ->
      ( full_balance,
        frozen_deposits,
        staking_balance,
        frozen_deposits_limit,
        delegated_contracts,
        delegated_balance,
        deactivated,
        grace_period,
        voting_power ))
    (fun ( full_balance,
           frozen_deposits,
           staking_balance,
           frozen_deposits_limit,
           delegated_contracts,
           delegated_balance,
           deactivated,
           grace_period,
           voting_power ) ->
      {
        full_balance;
        frozen_deposits;
        staking_balance;
        frozen_deposits_limit;
        delegated_contracts;
        delegated_balance;
        deactivated;
        grace_period;
        voting_power;
      })
    (obj9
       (req "full_balance" Tez.encoding)
       (req "frozen_deposits" Tez.encoding)
       (req "staking_balance" Tez.encoding)
       (opt "frozen_deposits_limit" Tez.encoding)
       (req "delegated_contracts" (list Contract.encoding))
       (req "delegated_balance" Tez.encoding)
       (req "deactivated" bool)
       (req "grace_period" Cycle.encoding)
       (req "voting_power" int32))

let participation_info_encoding =
  let open Data_encoding in
  conv
    (fun {
           Delegate.expected_cycle_activity;
           minimal_cycle_activity;
           missed_slots;
           remaining_allowed_missed_slots;
           expected_endorsing_rewards;
           current_pending_rewards;
         } ->
      ( expected_cycle_activity,
        minimal_cycle_activity,
        missed_slots,
        remaining_allowed_missed_slots,
        expected_endorsing_rewards,
        current_pending_rewards ))
    (fun ( expected_cycle_activity,
           minimal_cycle_activity,
           missed_slots,
           remaining_allowed_missed_slots,
           expected_endorsing_rewards,
           current_pending_rewards ) ->
      {
        expected_cycle_activity;
        minimal_cycle_activity;
        missed_slots;
        remaining_allowed_missed_slots;
        expected_endorsing_rewards;
        current_pending_rewards;
      })
    (obj6
       (req "expected_cycle_activity" int31)
       (req "minimal_cycle_activity" int31)
       (req "missed_slots" bool)
       (req "remaining_allowed_missed_slots" int31)
       (req "expected_endorsing_rewards" Tez.encoding)
       (req "current_pending_rewards" Tez.encoding))

module S = struct
  let raw_path = RPC_path.(open_root / "context" / "delegates")

  open Data_encoding

  type list_query = {active : bool; inactive : bool}

  let list_query : list_query RPC_query.t =
    let open RPC_query in
    query (fun active inactive -> {active; inactive})
    |+ flag "active" (fun t -> t.active)
    |+ flag "inactive" (fun t -> t.inactive)
    |> seal

  let list_delegate =
    RPC_service.get_service
      ~description:"Lists all registered delegates."
      ~query:list_query
      ~output:(list Signature.Public_key_hash.encoding)
      raw_path

  let path = RPC_path.(raw_path /: Signature.Public_key_hash.rpc_arg)

  let info =
    RPC_service.get_service
      ~description:"Everything about a delegate."
      ~query:RPC_query.empty
      ~output:info_encoding
      path

  let full_balance =
    RPC_service.get_service
      ~description:
        "Returns the full balance (in mutez) of a given delegate, including \
         the frozen balances."
      ~query:RPC_query.empty
      ~output:Tez.encoding
      RPC_path.(path / "full_balance")

  let frozen_deposits =
    RPC_service.get_service
      ~description:
        "Returns the amount of frozen deposits (in mutez) for a specific level \
         or the total sum when no specific level is provided."
      ~query:RPC_query.empty
      ~output:Tez.encoding
      RPC_path.(path / "frozen_deposits")

  let staking_balance =
    RPC_service.get_service
      ~description:
        "Returns the total amount of tokens (in mutez) delegated to a given \
         delegate. This includes the balances of all the contracts that \
         delegate to it, but also the balance of the delegate itself and its \
         frozen fees and deposits. The rewards do not count in the delegated \
         balance until they are unfrozen."
      ~query:RPC_query.empty
      ~output:Tez.encoding
      RPC_path.(path / "staking_balance")

  let frozen_deposits_limit =
    RPC_service.get_service
      ~description:
        "Returns the frozen deposits limit for the given delegate or none if \
         unbounded."
      ~query:RPC_query.empty
      ~output:(Data_encoding.option Tez.encoding)
      RPC_path.(path / "frozen_deposits_limit")

  let delegated_contracts =
    RPC_service.get_service
      ~description:
        "Returns the list of contracts that delegate to a given delegate."
      ~query:RPC_query.empty
      ~output:(list Contract.encoding)
      RPC_path.(path / "delegated_contracts")

  let delegated_balance =
    RPC_service.get_service
      ~description:
        "Returns the balances (in mutez) of all the contracts that delegate to \
         a given delegate. This excludes the delegate's own balance and its \
         frozen balances."
      ~query:RPC_query.empty
      ~output:Tez.encoding
      RPC_path.(path / "delegated_balance")

  let deactivated =
    RPC_service.get_service
      ~description:
        "Tells whether the delegate is currently tagged as deactivated or not."
      ~query:RPC_query.empty
      ~output:bool
      RPC_path.(path / "deactivated")

  let grace_period =
    RPC_service.get_service
      ~description:
        "Returns the cycle by the end of which the delegate might be \
         deactivated if she fails to execute any delegate action. A \
         deactivated delegate might be reactivated (without loosing any rolls) \
         by simply re-registering as a delegate. For deactivated delegates, \
         this value contains the cycle by which they were deactivated."
      ~query:RPC_query.empty
      ~output:Cycle.encoding
      RPC_path.(path / "grace_period")

  let voting_power =
    RPC_service.get_service
      ~description:
        "The number of rolls in the vote listings for a given delegate"
      ~query:RPC_query.empty
      ~output:Data_encoding.int32
      RPC_path.(path / "voting_power")

  let participation =
    RPC_service.get_service
      ~description:
        "Returns cycle and level participation information. In particular this \
         indicates the total number of slots that are required for a delegate \
         to endorse in the current cycle in order to be awarded its endorsing \
         rewards."
      ~query:RPC_query.empty
      ~output:participation_info_encoding
      RPC_path.(path / "participation")
end

let register () =
  let open Services_registration in
  register0 ~chunked:true S.list_delegate (fun ctxt q () ->
      Delegate.list ctxt >>= fun delegates ->
      match q with
      | {active = true; inactive = false} ->
          List.filter_es
            (fun pkh -> Delegate.deactivated ctxt pkh >|=? not)
            delegates
      | {active = false; inactive = true} ->
          List.filter_es (fun pkh -> Delegate.deactivated ctxt pkh) delegates
      | _ -> return delegates) ;
  register1 ~chunked:false S.info (fun ctxt pkh () () ->
      Delegate.check_delegate ctxt pkh >>=? fun () ->
      Delegate.full_balance ctxt pkh >>=? fun full_balance ->
      Delegate.frozen_deposits ctxt pkh >>=? fun frozen_deposits ->
      Delegate.staking_balance ctxt pkh >>=? fun staking_balance ->
      Delegate.frozen_deposits_limit ctxt pkh >>=? fun frozen_deposits_limit ->
      Delegate.delegated_contracts ctxt pkh >>= fun delegated_contracts ->
      Delegate.delegated_balance ctxt pkh >>=? fun delegated_balance ->
      Delegate.deactivated ctxt pkh >>=? fun deactivated ->
      Delegate.grace_period ctxt pkh >>=? fun grace_period ->
      Vote.get_voting_power_free ctxt pkh >|=? fun voting_power ->
      {
        full_balance;
        frozen_deposits;
        staking_balance;
        frozen_deposits_limit;
        delegated_contracts;
        delegated_balance;
        deactivated;
        grace_period;
        voting_power;
      }) ;
  register1 ~chunked:false S.full_balance (fun ctxt pkh () () ->
      trace (Balance_rpc_non_delegate pkh) (Delegate.check_delegate ctxt pkh)
      >>=? fun () -> Delegate.full_balance ctxt pkh) ;
  register1 ~chunked:false S.frozen_deposits (fun ctxt pkh () () ->
      Delegate.check_delegate ctxt pkh >>=? fun () ->
      Delegate.frozen_deposits ctxt pkh) ;
  register1 ~chunked:false S.staking_balance (fun ctxt pkh () () ->
      Delegate.check_delegate ctxt pkh >>=? fun () ->
      Delegate.staking_balance ctxt pkh) ;
  register1 ~chunked:false S.frozen_deposits_limit (fun ctxt pkh () () ->
      Delegate.check_delegate ctxt pkh >>=? fun () ->
      Delegate.frozen_deposits_limit ctxt pkh) ;
  register1 ~chunked:true S.delegated_contracts (fun ctxt pkh () () ->
      Delegate.check_delegate ctxt pkh >>=? fun () ->
      Delegate.delegated_contracts ctxt pkh >|= ok) ;
  register1 ~chunked:false S.delegated_balance (fun ctxt pkh () () ->
      Delegate.check_delegate ctxt pkh >>=? fun () ->
      Delegate.delegated_balance ctxt pkh) ;
  register1 ~chunked:false S.deactivated (fun ctxt pkh () () ->
      Delegate.check_delegate ctxt pkh >>=? fun () ->
      Delegate.deactivated ctxt pkh) ;
  register1 ~chunked:false S.grace_period (fun ctxt pkh () () ->
      Delegate.check_delegate ctxt pkh >>=? fun () ->
      Delegate.grace_period ctxt pkh) ;
  register1 ~chunked:false S.voting_power (fun ctxt pkh () () ->
      Delegate.check_delegate ctxt pkh >>=? fun () ->
      Vote.get_voting_power_free ctxt pkh) ;
  register1 ~chunked:false S.participation (fun ctxt pkh () () ->
      Delegate.check_delegate ctxt pkh >>=? fun () ->
      Delegate.delegate_participation_info ctxt pkh)

let list ctxt block ?(active = true) ?(inactive = false) () =
  RPC_context.make_call0 S.list_delegate ctxt block {active; inactive} ()

let info ctxt block pkh = RPC_context.make_call1 S.info ctxt block pkh () ()

let full_balance ctxt block pkh =
  RPC_context.make_call1 S.full_balance ctxt block pkh () ()

let frozen_deposits ctxt block pkh =
  RPC_context.make_call1 S.frozen_deposits ctxt block pkh () ()

let staking_balance ctxt block pkh =
  RPC_context.make_call1 S.staking_balance ctxt block pkh () ()

let frozen_deposits_limit ctxt block pkh =
  RPC_context.make_call1 S.frozen_deposits_limit ctxt block pkh () ()

let delegated_contracts ctxt block pkh =
  RPC_context.make_call1 S.delegated_contracts ctxt block pkh () ()

let delegated_balance ctxt block pkh =
  RPC_context.make_call1 S.delegated_balance ctxt block pkh () ()

let deactivated ctxt block pkh =
  RPC_context.make_call1 S.deactivated ctxt block pkh () ()

let grace_period ctxt block pkh =
  RPC_context.make_call1 S.grace_period ctxt block pkh () ()

let voting_power ctxt block pkh =
  RPC_context.make_call1 S.voting_power ctxt block pkh () ()
