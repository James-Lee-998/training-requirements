import unittest
from tic_tac_toe_board import board
import pandas as pd


class MyTestCase(unittest.TestCase):
    def test_board_modification(self):
        test_board = board()
        test_board.modify_board(2, 2, "X")
        expected_board = pd.DataFrame([[".", ".", "."],
                                   [".", ".", "."],
                                   [".", ".", "X"]])
        self.assertTrue(test_board.board.equals(expected_board))  # add assertion here

    def test_board_win_condition(self):
        test_board = board()
        test_board.board = pd.DataFrame([[".", ".", "X"],
                                   [".", ".", "X"],
                                   [".", ".", "X"]])
        self.assertTrue(test_board.determine_win_state())

    def test_board_null_state(self):
        test_board = board()
        test_board.board = pd.DataFrame([[".", "O", "."],
                                   [".", ".", "X"],
                                   [".", ".", "X"]])
        self.assertTrue(not test_board.determine_win_state())

    def test_board_win_condition_diagonal(self):
        test_board = board()
        test_board.board = pd.DataFrame([["O", ".", "X"],
                                   [".", "X", "X"],
                                   ["X", ".", "O"]])
        self.assertTrue(test_board.determine_win_state())

    def test_board_is_full(self):
        test_board = board()
        test_board.board = pd.DataFrame([["O", "O", "X"],
                                   ["O", "X", "X"],
                                   ["X", "O", "O"]])
        self.assertTrue(test_board.determine_if_board_is_full())


if __name__ == '__main__':
    unittest.main()
