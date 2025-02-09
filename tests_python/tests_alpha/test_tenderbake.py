import time
import copy
import pytest
from tools import constants
from launchers.sandbox import Sandbox
from . import protocol


ROUND_DURATION = 4
ROUND_DURATIONS = [str(ROUND_DURATION), str(ROUND_DURATION)]
TEST_DURATION = 5 * ROUND_DURATION
NUM_NODES = 5


@pytest.mark.baker
@pytest.mark.multinode
@pytest.mark.slow
@pytest.mark.incremental
@pytest.mark.tenderbake
class TestProtoTenderbake:
    """Run a number of nodes and bakers, wait and check that all blocks
    were agreed upon at round 0"""

    def test_init(self, sandbox: Sandbox):

        for i in range(NUM_NODES):
            sandbox.add_node(i, params=constants.NODE_PARAMS)

        proto_params = dict(protocol.TENDERBAKE_PARAMETERS)
        parameters = copy.deepcopy(proto_params)
        parameters['round_durations'] = ROUND_DURATIONS
        parameters['consensus_threshold'] = (
            2 * (parameters['consensus_threshold'] // 3) + 1
        )
        protocol.activate(sandbox.client(0), parameters=parameters)

        for i in range(NUM_NODES):
            sandbox.add_baker(
                i,
                [f'bootstrap{i + 1}'],
                proto=protocol.DAEMON,
                log_levels=constants.TENDERBAKE_BAKER_LOG_LEVELS,
            )

    def test_wait(self):
        time.sleep(TEST_DURATION)

    def test_level(self, sandbox: Sandbox):
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
