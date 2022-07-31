import pandas as pd


class board(object):

    def __init__(self):
        self.board = pd.DataFrame([[".", ".", "."],
                                   [".", ".", "."],
                                   [".", ".", "."]])

    def modify_board(self, x_position, y_position, marker):
        if self.board.iat[x_position, y_position] == '.':
            self.board.iat[x_position, y_position] = marker
            return True
        else:
            return False

    def determine_win_state(self):
        # check rows
        for index, row in self.board.iterrows():
            if len(set(row)) == 1 and set(row) != {'.'}:
                return True
        # check columns
        for index, column in self.board.iteritems():
            if len(set(column)) == 1 and set(column) != {'.'}:
                return True
        # check diagonals
        ttt_dict = self.board.to_dict('index')

        # check equal diagonals
        equal_diagonals = [v for k, v in {k: v[k] for k, v in ttt_dict.items()}.items()]
        if len(set(equal_diagonals)) == 1 and set(equal_diagonals) != {'.'}:
            return True

        # check diagonals summing to 2
        unequal_diagonals = [v for k, v in {k: v[abs(2-k)] for k, v in ttt_dict.items()}.items()]
        if len(set(unequal_diagonals)) == 1 and set(unequal_diagonals) != {'.'}:
            return True

        return False

    def determine_if_board_is_full(self):
        return self.board.isin(['.']).equals(pd.DataFrame([[False, False, False],
                                                           [False, False, False],
                                                           [False, False, False]]))

    def get_board_empty_coordinates(self):
        list_of_coordinates = []
        for index, row in self.board.iterrows():
            for count, i in enumerate(row):
                if i == '.':
                    list_of_coordinates.append((index, count))
        return list_of_coordinates


if __name__ == '__main__':
    x = board()
    x.board = pd.DataFrame([['.','.','.'],
                            ['x','.','x'],
                            ['x','x','x']])
    print(x.get_board_empty_coordinates())
