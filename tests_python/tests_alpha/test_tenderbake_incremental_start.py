import copy
import time
import pytest
from tools import constants
from launchers.sandbox import Sandbox
from . import protocol

NUM_NODES = 5  # because we assume 5 (bootstrap) accounts
NUM_EARLY_START_NODES = 2
ROUND_DURATION = 4
ROUND_DURATIONS = [str(ROUND_DURATION), str(ROUND_DURATION)]
TEST_DURATION = 5 * ROUND_DURATION


@pytest.mark.baker
@pytest.mark.multinode
@pytest.mark.slow
@pytest.mark.incremental
@pytest.mark.tenderbake
class TestProtoTenderbakeIncrementalStart:
    """Run a number of nodes and start the bakers incrementally, each one
    after one round duration more. After all bakers have been
    started, they should be able to reach a decision."""

    def test_init_nodes(self, sandbox: Sandbox):
        for i in range(NUM_NODES):
            sandbox.add_node(
                i,
                params=constants.NODE_PARAMS,
                log_levels=constants.TENDERBAKE_NODE_LOG_LEVELS,
            )

    def test_start_some_bakers(self, sandbox: Sandbox):
        for i in range(NUM_EARLY_START_NODES):
            account = f'bootstrap{i + 1}'
            sandbox.add_baker(
                i,
                [account],
                proto=protocol.DAEMON,
                log_levels=constants.TENDERBAKE_BAKER_LOG_LEVELS,
            )

    def test_activate(self, sandbox):
        proto_params = dict(protocol.TENDERBAKE_PARAMETERS)
        parameters = copy.deepcopy(proto_params)
        parameters['round_durations'] = ROUND_DURATIONS
        parameters['consensus_threshold'] = (
            2 * (parameters['consensus_threshold'] // 3) + 1
        )

        time.sleep(2 * ROUND_DURATION)
        protocol.activate(
            sandbox.client(0),
            parameters=parameters,
        )

    def test_start_remaining_bakers(self, sandbox: Sandbox):
        for i in range(NUM_EARLY_START_NODES, NUM_NODES):
            account = f'bootstrap{i + 1}'
            sandbox.add_baker(
                i,
                [account],
                proto=protocol.DAEMON,
                log_levels=constants.TENDERBAKE_BAKER_LOG_LEVELS,
            )
            time.sleep(ROUND_DURATION)

    def test_wait(self):
        time.sleep(TEST_DURATION)

    def test_level(self, sandbox):
        # a decision should be taken in the first round, so we can deduce at
        # which minimal level the nodes should be at
        expected_min_level = 1 + TEST_DURATION // ROUND_DURATION
        for client in sandbox.all_clients():
            level = client.get_level()
            assert level >= expected_min_level
            for i in range(level + 1):
                if i > 1:
                    block_round = client.get_tenderbake_round(level=str(i))
                    assert block_round == 0
