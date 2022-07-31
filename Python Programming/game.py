import pandas as pd
from tic_tac_toe_board import board
import random


class game(object):

    def __init__(self, marker="X", computer_marker='O'):
        self.board_object = board()
        self.user_marker = marker
        self.computer_marker = computer_marker

    def user_turn(self, x_position, y_position):
        """

        :param x_position:
        :param y_position:
        :return: handles user input - modifies the board
        """
        try:
            x_position = int(x_position)
            y_position = int(y_position)
        except ValueError:
            print(f"Invalid value you did not supply an integer")
            pass
        while x_position not in [0, 1, 2]:
            print(f'Invalid row input: {x_position}', sep='\n')
            print('Please pick in range of 0 and 2')
            x_position = int(input('Pick a row for input: '))
        while y_position not in [0, 1, 2]:
            print(f'Invalid column input: {y_position}', sep='\n')
            print('Please pick in range of 0 and 2')
            y_position = int(input('Pick a column for input: '))
        while not self.board_object.modify_board(x_position, y_position, self.user_marker):
            print("There is already a value in that position. Please try again")
            x_position = int(input('Pick a row for input: '))
            y_position = int(input('Pick a column for input: '))
            while x_position not in [0, 1, 2]:
                print(f'Invalid row input: {x_position}', sep='\n')
                print('Please pick in range of 0 and 2')
                x_position = int(input('Pick a row for input: '))
            while y_position not in [0, 1, 2]:
                print(f'Invalid row input: {y_position}', sep='\n')
                print('Please pick in range of 0 and 2')
                y_position = int(input('Pick a column for input: '))
            print(self.board_object.board.to_string(), sep='\n')
            if self.board_object.modify_board(x_position, y_position, self.user_marker):
                break

    def computer_turn(self):
        x_position, y_position = random.choice(self.board_object.get_board_empty_coordinates())
        self.board_object.modify_board(x_position, y_position, self.computer_marker)

    @staticmethod
    def determine_score(temp_board):
        if temp_board.determine_win_state():
            return 1
        return 0

    def best_move_algo(self):
        list_of_moves_and_scores = []
        for x, y in self.board_object.get_board_empty_coordinates():
            temp_board = board()
            temp_board.board = self.board_object.board.copy()

            temp_board.modify_board(x, y, self.computer_marker)
            if self.determine_score(temp_board):
                # if win then score is 1
                list_of_moves_and_scores.append({'x': x, 'y': y, 'score': 2})  # to prioritize win state over draw
            else:
                for i, j in temp_board.get_board_empty_coordinates():
                    temp_temp_board = board()
                    temp_temp_board.board = temp_board.board.copy()
                    temp_temp_board.modify_board(i, j, self.user_marker)
                    if self.determine_score(temp_temp_board):
                        # to stop an opponent win
                        list_of_moves_and_scores.append({'x': i, 'y': j, 'score': 1})
                    else:
                        list_of_moves_and_scores.append({'x': x, 'y': y, 'score': 0})

        return list_of_moves_and_scores

    def computer_best_move(self, hard_mode=True):
        if pd.DataFrame(self.best_move_algo()).empty:
            self.computer_turn()
        else:
            if hard_mode:
                all_data = pd.DataFrame(self.best_move_algo()).sort_values(by=['score'], ascending=False).reset_index(
                    drop=True)
            else:
                all_data = pd.DataFrame(self.best_move_algo()).sort_values(by=['score'], ascending=True).reset_index(
                    drop=True)
            # print(all_data.to_string())
            self.board_object.modify_board(all_data.iat[0, 0], all_data.iat[0, 1], self.computer_marker)
