from game import game


def main():

    marker_list = ['X', 'O']
    mode_list = ['Y', 'N']
    user_marker = input('Would you like to be X or O? ')
    while user_marker.upper() not in marker_list:
        print(f"Please choose between X or O not {user_marker}")
        user_marker = input('Would you like to be X or O? ')
    user_marker = user_marker.upper()
    marker_list.remove(user_marker)
    computer_marker = marker_list[0]

    mode = input("Hard mode? (Y/N)")
    while mode.upper() not in mode_list:
        print(f"Please choose either Y or N")
        mode = input("Hard mode? (Y/N)")
    if mode.upper() == 'Y':
        hard = True
    else:
        hard = False
    game_instance = game(user_marker, computer_marker)
    counter = 1
    while not game_instance.board_object.determine_win_state():
        print(f"Round {counter}", sep='\n')
        if counter == 1 and user_marker == 'X':
            print(game_instance.board_object.board.to_string(), sep='\n')
        if user_marker == 'O':
            print(f'Computer turn {counter}')
            if counter == 1:
                game_instance.computer_turn()
            else:
                game_instance.computer_best_move(hard)
            print(game_instance.board_object.board.to_string(), sep='\n')
            if game_instance.board_object.determine_win_state():
                print('Computer has won')
                break
            if not game_instance.board_object.get_board_empty_coordinates():
                print('Game has ended in a draw')
                break
        print(f"Player turn {counter}", sep='\n')
        x_position = input('Pick a row for input: ')
        y_position = input('Pick a column for input: ')
        game_instance.user_turn(x_position, y_position)
        print(game_instance.board_object.board.to_string(), sep='\n')
        if game_instance.board_object.determine_win_state():
            print('Player has won', sep='\n')
            break
        if not game_instance.board_object.get_board_empty_coordinates():
            print('Game has ended in a draw')
            break
        if user_marker == 'X':
            print(f'Computer turn {counter}')
            if counter == 1:
                game_instance.computer_turn()
            else:
                game_instance.computer_best_move(hard)
            print(game_instance.board_object.board.to_string(), sep='\n')
            if game_instance.board_object.determine_win_state():
                print('Computer has won')
                break
            if not game_instance.board_object.get_board_empty_coordinates():
                print('Game has ended in a draw')
                break
        counter += 1


if __name__ == '__main__':
    main()
