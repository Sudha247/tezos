from os import path
import pytest
from client.client import Client
from tools.paths import ACCOUNT_PATH
from tools.utils import assert_run_failure
from .contract_paths import CONTRACT_PATH


BAKE_ARGS = ['--max-priority', '512', '--minimal-timestamp']
TRANSFER_ARGS = ['--burn-cap', '0.257']


@pytest.mark.incremental
class TestRawContext:
    def test_delegates(self, client: Client):
        path = '/chains/main/blocks/head/context/raw/bytes/delegates/?depth=3'
        res = client.rpc('get', path)
        expected = {
            "ed25519": {
                "02": {"29": None},
                "a9": {"ce": None},
                "c5": {"5c": None},
                "da": {"c9": None},
                "e7": {"67": None},
            }
        }
        assert res == expected

    def test_no_service_1(self, client: Client):
        path = '/chains/main/blocks/head/context/raw/bytes/non-existent'
        with assert_run_failure('No service found at this URL'):
            client.rpc('get', path)

    def test_no_service_2(self, client: Client):
        path = (
            '/chains/main/blocks/head/context/raw/bytes/'
            'non-existent?depth=-1'
        )
        expected = 'Command failed : Extraction depth -1 is invalid'
        with assert_run_failure(expected):
            client.rpc('get', path)

    def test_no_service_3(self, client: Client):
        path = '/chains/main/blocks/head/context/raw/bytes/non-existent?depth=0'
        with assert_run_failure('No service found at this URL'):
            client.rpc('get', path)

    def test_bake(self, client: Client):
        client.bake('bootstrap4', BAKE_ARGS)

    def test_gen_keys(self, client: Client, session):
        session['keys'] = ['foo', 'bar', 'boo']
        sigs = [None, 'secp256k1', 'ed25519']
        for key, sig in zip(session['keys'], sigs):
            args = [] if sig is None else ['--sig', sig]
            client.gen_key(key, args)

    def test_transfers(self, client: Client, session):
        client.transfer(1000, 'bootstrap1', session['keys'][0], TRANSFER_ARGS)
        client.bake('bootstrap1', BAKE_ARGS)
        client.transfer(2000, 'bootstrap1', session['keys'][1], TRANSFER_ARGS)
        client.bake('bootstrap1', BAKE_ARGS)
        client.transfer(3000, 'bootstrap1', session['keys'][2], TRANSFER_ARGS)
        client.bake('bootstrap1', BAKE_ARGS)

    def test_balances(self, client: Client, session):
        assert client.get_balance(session['keys'][0]) == 1000
        assert client.get_balance(session['keys'][1]) == 2000
        assert client.get_balance(session['keys'][2]) == 3000

    def test_transfer_bar_foo(self, client: Client, session):
        client.transfer(
            1000,
            session['keys'][1],
            session['keys'][0],
            ['--fee', '0', '--force-low-fee'],
        )
        client.bake(
            'bootstrap1',
            BAKE_ARGS
            + [
                '--minimal-fees',
                '0',
                '--minimal-nanotez-per-byte',
                '0',
                '--minimal-nanotez-per-gas-unit',
                '0',
            ],
        )

    def test_balances_bar_foo(self, client: Client, session):
        assert client.get_balance(session['keys'][0]) == 2000
        assert client.get_balance(session['keys'][1]) == 1000

    def test_transfer_foo_bar(self, client: Client, session):
        client.transfer(
            1000, session['keys'][0], session['keys'][1], ['--fee', '0.05']
        )
        client.bake('bootstrap1', BAKE_ARGS)

    def test_balances_foo_bar(self, client: Client, session):
        assert client.get_balance(session['keys'][0]) == 999.95
        assert client.get_balance(session['keys'][1]) == 2000

    def test_transfer_failure(self, client: Client, session):
        with pytest.raises(Exception):
            client.transfer(999.95, session['keys'][0], session['keys'][1])

    def test_originate_contract_noop(self, client: Client):
        contract = path.join(CONTRACT_PATH, 'opcodes', 'noop.tz')
        client.remember('noop', contract)
        client.typecheck(contract)
        client.originate(
            'noop', 1000, 'bootstrap1', contract, ['--burn-cap', '0.295']
        )
        client.bake('bootstrap1', BAKE_ARGS)

    def test_transfer_to_noop(self, client: Client):
        client.transfer(10, 'bootstrap1', 'noop', ['--arg', 'Unit'])
        client.bake('bootstrap1', BAKE_ARGS)

    def test_contract_hardlimit(self, client: Client):
        contract = path.join(CONTRACT_PATH, 'mini_scenarios', 'hardlimit.tz')
        client.originate(
            'hardlimit',
            1000,
            'bootstrap1',
            contract,
            ['--init', '3', '--burn-cap', '0.341'],
        )
        client.bake('bootstrap1', BAKE_ARGS)
        client.transfer(10, 'bootstrap1', 'hardlimit', ['--arg', 'Unit'])
        client.bake('bootstrap1', BAKE_ARGS)
        client.transfer(10, 'bootstrap1', 'hardlimit', ['--arg', 'Unit'])
        client.bake('bootstrap1', BAKE_ARGS)

    def test_transfers_bootstraps5_bootstrap1(self, client: Client):
        assert client.get_balance('bootstrap5') == 4000000
        client.transfer(
            400000,
            'bootstrap5',
            'bootstrap1',
            ['--fee', '0', '--force-low-fee'],
        )
        client.bake('bootstrap1', BAKE_ARGS)
        client.transfer(
            400000,
            'bootstrap1',
            'bootstrap5',
            ['--fee', '0', '--force-low-fee'],
        )
        client.bake('bootstrap1', BAKE_ARGS)
        assert client.get_balance('bootstrap5') == 4000000

    def test_activate_accounts(self, client: Client, session):
        account = f"{ACCOUNT_PATH}/king_commitment.json"
        session['keys'] += ['king', 'queen']
        client.activate_account(session['keys'][3], account)
        client.bake('bootstrap1', BAKE_ARGS)
        account = f"{ACCOUNT_PATH}/queen_commitment.json"
        client.activate_account(session['keys'][4], account)
        client.bake('bootstrap1', BAKE_ARGS)
        assert client.get_balance(session['keys'][3]) == 23932454.669343
        assert client.get_balance(session['keys'][4]) == 72954577.464032

    def test_transfer_king_queen(self, client: Client, session):
        keys = session['keys']
        client.transfer(10, keys[3], keys[4], TRANSFER_ARGS)
        client.bake('bootstrap1', BAKE_ARGS)

    def test_duplicate_alias(self, client: Client):
        client.add_address("baz", "foo", force=True)
        show_foo = client.show_address("foo", show_secret=True)
        assert show_foo.secret_key is not None


class TestRememberContract:
    @pytest.mark.parametrize(
        "contract_name,non_originated_contract_address",
        [
            ("test", "KT1BuEZtb68c1Q4yjtckcNjGELqWt56Xyesc"),
            ("test-2", "KT1TZCh8fmUbuDqFxetPWC2fsQanAHzLx4W9"),
        ],
    )
    def test_non_originated_contract_no_forcing_not_saved_before(
        self,
        client,
        contract_name,
        non_originated_contract_address,
    ):
        client.remember_contract(contract_name, non_originated_contract_address)

    # As it is always the same client, the contracts have been saved
    # before
    @pytest.mark.parametrize(
        "contract_name,non_originated_contract_address",
        [
            ("test", "KT1BuEZtb68c1Q4yjtckcNjGELqWt56Xyesc"),
            ("test-2", "KT1TZCh8fmUbuDqFxetPWC2fsQanAHzLx4W9"),
        ],
    )
    def test_non_originated_contract_with_forcing_and_saved_before(
        self,
        client,
        contract_name,
        non_originated_contract_address,
    ):
        client.remember_contract(
            contract_name, non_originated_contract_address, force=True
        )

    # As it is always the same client, the contracts have been saved
    # before
    @pytest.mark.parametrize(
        "contract_name,non_originated_contract_address",
        [
            ("test", "KT1BuEZtb68c1Q4yjtckcNjGELqWt56Xyesc"),
            ("test-2", "KT1TZCh8fmUbuDqFxetPWC2fsQanAHzLx4W9"),
        ],
    )
    def test_non_originated_contract_no_forcing_and_saved_before(
        self,
        client,
        contract_name,
        non_originated_contract_address,
    ):
        expected_error = f"The contract alias {contract_name} already exists"

        with assert_run_failure(expected_error):
            client.remember_contract(
                contract_name, non_originated_contract_address, force=False
            )

    # Test operation size.
    def test_operation_size_originate_byte_contract(self, client: Client):
        contract = path.join(CONTRACT_PATH, 'opcodes', 'bytes.tz')
        client.remember('bytes', contract)
        client.typecheck(contract)
        client.originate(
            'bytes', 1000, 'bootstrap1', contract, ['--burn-cap', '0.295']
        )
        client.bake('bootstrap1', BAKE_ARGS)

    # Test that operations under 16KB can be injected in the node.
    def test_operation_size_small(self, client: Client):
        bytes_arg = "0x" + ("00" * 6 * 1024)  # 6 KB of data.

        client.transfer(10, 'bootstrap1', 'bytes', ['--arg', bytes_arg])
        client.bake('bootstrap1', BAKE_ARGS)

    # Test that operations between 16KB and 32KB can be injected in the node.
    def test_operation_size_medium(self, client: Client):
        bytes_arg = "0x" + ("00" * 24 * 1024)  # 24 KB of data.

        client.transfer(10, 'bootstrap1', 'bytes', ['--arg', bytes_arg])
        client.bake('bootstrap1', BAKE_ARGS)

    # Test that operations above 32KB fail to be injected.
    def test_operation_size_oversized(self, client: Client):
        bytes_arg = "0x" + ("00" * 36 * 1024)  # 36 KB of data.

        expected_error = "Oversized operation"
        with assert_run_failure(expected_error):
            client.transfer(10, 'bootstrap1', 'bytes', ['--arg', bytes_arg])

    # Test operation size with various data types.
    def test_operation_size_originate_munch_contract(self, client: Client):
        contract = path.join(CONTRACT_PATH, 'opcodes', 'munch.tz')
        client.remember('munch', contract)
        client.typecheck(contract)
        client.originate(
            'munch', 1000, 'bootstrap1', contract, ['--burn-cap', '0.295']
        )
        client.bake('bootstrap1', BAKE_ARGS)

    # Test that a large operation under 32KB can be injected in the node
    # (variant using a lambda with deep nesting).
    def test_operation_size_with_lambda_ok(self, client: Client):
        # Each pair of braces is encoded on 5 bytes so this takes
        # 5 * 6 * 1024 = 30 KB < 32KB
        big_arg = ("{" * 6 * 1024) + ("}" * 6 * 1024)

        client.transfer(
            10,
            'bootstrap1',
            'munch',
            ['--arg', big_arg, "--entrypoint", "lambda"],
        )
        client.bake('bootstrap1', BAKE_ARGS)

    # Test that a large operation over 32KB cannot be injected in the node,
    # and the error is not a stack overflow
    # (variant using a lambda with deep nesting).
    def test_operation_size_with_lambda_fail(self, client: Client):
        # Each pair of braces is encoded on 5 bytes so this takes
        # 5 * 7 * 1024 = 35 KB > 32KB
        big_arg = ("{" * 7 * 1024) + ("}" * 7 * 1024)

        expected_error = "Oversized operation"
        with assert_run_failure(expected_error):
            client.transfer(
                10,
                'bootstrap1',
                'munch',
                ['--arg', big_arg, "--entrypoint", "lambda"],
            )

    # Test that a large operation under 32KB can be injected in the node
    # (variant using a long list).
    def test_operation_size_with_list_ok(self, client: Client):
        # Each element in the list takes 2 bytes so about 30KB in total
        big_arg = "{" + ("0; " * 15 * 1024) + "}"

        client.transfer(
            10,
            'bootstrap1',
            'munch',
            ['--arg', big_arg, "--entrypoint", "list_nat"],
        )
        client.bake('bootstrap1', BAKE_ARGS)

    # Test that a large operation over 32KB cannot be injected in the node,
    # and the error is not a stack overflow
    # (variant using a long list).
    def test_operation_size_with_list_fail(self, client: Client):
        # Each element in the list takes 2 bytes so about 34KB in total
        big_arg = "{" + ("0; " * 17 * 1024) + "}"

        expected_error = "Oversized operation"
        with assert_run_failure(expected_error):
            client.transfer(
                10,
                'bootstrap1',
                'munch',
                ['--arg', big_arg, "--entrypoint", "list_nat"],
            )

    # Test that a large operation under 32KB can be injected in the node
    # (variant using a big nat).
    def test_operation_size_with_nat_ok(self, client: Client):
        # The encoding for nat uses a byte to encode 7 bits of the number
        # so the size of 2 ** (7 * n) is about n bytes
        big_arg = 2 ** (7 * 30 * 1024)

        client.transfer(
            10,
            'bootstrap1',
            'munch',
            ['--arg', f"{big_arg}", "--entrypoint", "nat"],
        )
        client.bake('bootstrap1', BAKE_ARGS)

    # Test that a large operation over 32KB cannot be injected in the node,
    # and the error is not a stack overflow
    # (variant using a big nat).
    def test_operation_size_with_nat_fail(self, client: Client):
        # The encoding for nat uses a byte to encode 7 bits of the number
        # so the size of 2 ** (7 * n) is about n bytes
        big_arg = 2 ** (7 * 33 * 1024)

        expected_error = "Oversized operation"
        with assert_run_failure(expected_error):
            client.transfer(
                10,
                'bootstrap1',
                'munch',
                ['--arg', f"{big_arg}", "--entrypoint", "nat"],
            )
