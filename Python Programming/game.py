from tic_tac_toe_board import board
import random


class game(object):

    def __init__(self, marker="X", computer_marker='O'):
        self.board_object = board()
        self.user_marker = marker
        self.computer_marker = computer_marker

    def user_turn(self, x_position, y_position):
        while x_position not in [0, 1, 2]:
            print(f'Invalid row input: {x_position}', sep='\n')
            print('Please pick in range of 0 and 2')
            x_position = int(input('Pick a row for input: '))
        while y_position not in [0, 1, 2]:
            print(f'Invalid row input: {y_position}', sep='\n')
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
        list_of_coordinates = []
        for index, row in self.board_object.board.iterrows():
            for i in row:
                if i == '.':
                    list_of_coordinates.append((index, list(row).index(i)))
        x_position, y_position = random.choice(list_of_coordinates)
        self.board_object.modify_board(x_position, y_position, self.computer_marker)
