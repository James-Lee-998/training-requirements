if (interactive()) {
  con <- stdin()
} else {
  con <- "stdin"
}
cat("X or O? ")
symbol <- readLines(con = con, n = 1)

if (symbol == 'X'|| symbol == 'O' || symbol == 'x' || symbol == 'o') { 
  rep = FALSE} else {
    rep = TRUE
  }

while (rep) {
  cat("Invalid selection. Pick either X or O")
  cat("\n")
  cat("X or O? ")
  symbol <- readLines(con = con, n = 1)
  if (symbol == 'X' || symbol == 'O') {
    break
  }
} 

if (symbol == 'X' || symbol == 'x') {
  FIRST = TRUE
} else {
  FIRST = FALSE
}

WIN = FALSE
LOSE = FALSE

win_checker_X = function(x) {
  X_counter = 0
  sequence_of_moves_X = c()
  X_WIN = FALSE
  for (i in x) {
    X_counter = X_counter + 1
    if (('X' %in% i)==TRUE) {
      sequence_of_moves_X = c(sequence_of_moves_X, X_counter)
    }
  }
  for (i in 1:ncol(win_scenarios)) {
    G = win_scenarios[i][[1]][1]
    H = win_scenarios[i][[1]][2]
    D = win_scenarios[i][[1]][3]
    if (G %in% sequence_of_moves_X == TRUE && H %in% sequence_of_moves_X == TRUE && D %in% sequence_of_moves_X == TRUE){
      X_WIN = TRUE
    } 
  }
  return(X_WIN)
}

win_checker_O = function(x) {
  O_counter = 0
  sequence_of_moves_O = c()
  O_WIN = FALSE
  for (i in x) {
    O_counter = O_counter + 1
    if (('O' %in% i)==TRUE) {
      sequence_of_moves_O = c(sequence_of_moves_O, O_counter)
    }
  }
  for (i in 1:ncol(win_scenarios)) {
    G = win_scenarios[i][[1]][1]
    H = win_scenarios[i][[1]][2]
    D = win_scenarios[i][[1]][3]
    if (G %in% sequence_of_moves_O == TRUE && H %in% sequence_of_moves_O == TRUE && D %in% sequence_of_moves_O == TRUE){
      O_WIN = TRUE
    } 
  }
  return(O_WIN)
}

win_scenarios = data.frame(y1 = c(1,4,7), y2 = c(1,2,3), y3 = c(1,4,9), y4 = c(2,5,8), y5 = c(3,6,9), y6 = c(4,5,6), y7 = c(7,8,9), y8 = c(3,5,7))
mat.data = rep(NA, times = 9)
mat = matrix(mat.data, nrow = 3, ncol = 3) 

i = 1

while (WIN == FALSE && LOSE == FALSE) {
  if (FIRST == TRUE) { 
    cat("\n")
    cat("\n")
    cat("############################################################")
    cat("\t \t \t ROUND#", as.character(i), " START")
    cat("\n")
    cat("############################################################")
    cat("\n")
    cat("\n")
    cat("\t \t \t Current Board:")
    cat("\n")
    cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
    cat("\n")
    
    print(mat)
    
    cat("\n")
    cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
    cat("\n")
    cat("\n")
    cat("Your move:")
    cat("\n")
    cat("Row:")
    rowwy = as.numeric(readLines(con = con, n = 1))
    cat("Column:")
    collumny = as.numeric(readLines(con = con, n = 1))
    cat("So your move is:", as.character(rowwy), ",", as.character(collumny), "? [y/n]")
    check  = readLines(con = con, n = 1)
    if ((rowwy < 0) == TRUE || (collumny <0) == TRUE || (rowwy > 3) == TRUE || (collumny > 3) == TRUE|| is.numeric(rowwy) == FALSE || is.numeric(collumny) == FALSE) {
      while ((rowwy < 0) == TRUE || (collumny <0) == TRUE || (rowwy > 3) == TRUE || (collumny > 3) == TRUE|| is.numeric(rowwy) == FALSE || is.numeric(collumny) == FALSE) {
        cat("Invalid move")
        cat("\n")
        cat("Your move:")
        cat("\n")
        cat("Row:")
        rowwy = as.numeric(readLines(con = con, n = 1))
        cat("Column:")
        collumny = as.numeric(readLines(con = con, n = 1))
        cat("So your move is:", as.character(rowwy), ",", as.character(collumny), "? [y/n]")
        check  = readLines(con = con, n = 1) 
        if ((check == 'N') == TRUE || (check = 'n') == TRUE) {
          cat("Redo move")
          cat("\n")
          cat("Your move:")
          cat("\n")
          cat("Row:")
          rowwy = as.numeric(readLines(con = con, n = 1))
          cat("Column:")
          collumny = as.numeric(readLines(con = con, n = 1))
          cat("So your move is:", as.character(rowwy), ",", as.character(collumny), "? [y/n]")
          check  = readLines(con = con, n = 1) 
        } else {  
            if (rowwy <0 && collumny <0 && rowwy>3 && collumny>3 && is.numeric(rowwy) == TRUE && is.numeric(collumny) == TRUE && ((check == 'y') == TRUE || (check = 'Y') == TRUE)) {
            break
            }
        }
      }
    }
    P = mat[rowwy,collumny]
    while (is.na(P) == FALSE) {
      cat("Invalid move: move has already been placed in that position")
      cat("\n")
      cat("Your move:")
      cat("\n")
      cat("Row:")
      rowwy = as.numeric(readLines(con = con, n = 1))
      cat("Column:")
      collumny = as.numeric(readLines(con = con, n = 1))
      cat("So your move is:", as.character(rowwy), ",", as.character(collumny), "? [y/n]")
      check  = readLines(con = con, n = 1) 
      P = mat[rowwy,collumny]
      if ((check == 'Y'||check == 'y') == TRUE) {
        if (is.na(P) == TRUE) {
          break
        }
      }
    } 
    if ((check == 'N'|| check == 'n') == TRUE) {
      while (is.na(P) == FALSE) {
        cat("Invalid move: move has already been placed in that position")
        cat("\n")
        cat("Your move:")
        cat("\n")
        cat("Row:")
        rowwy = as.numeric(readLines(con = con, n = 1))
        cat("Column:")
        collumny = as.numeric(readLines(con = con, n = 1))
        cat("So your move is:", as.character(rowwy), ",", as.character(collumny), "? [y/n]")
        check  = readLines(con = con, n = 1) 
        P = mat[rowwy,collumny]
        if ((check == 'Y'|| check == 'y') == TRUE) {
          if (is.na(P) == TRUE) {
            break
          }
        }
      }
        if ((check == 'N'|| check == 'n') == TRUE) {
          cat("Redo your move:")
          cat("\n")
          cat("Row:")
          rowwy = as.numeric(readLines(con = con, n = 1))
          cat("Column:")
          collumny = as.numeric(readLines(con = con, n = 1))
          cat("So your move is:", as.character(rowwy), ",", as.character(collumny), "? [y/n]")
          check  = readLines(con = con, n = 1)
          P = mat[rowwy,collumny]
          if ((is.na(P) == TRUE) && (check == 'Y' || check == 'y') == TRUE) {
            break
          }
        } else {
            break
        }
      }
    mat[rowwy,collumny] = 'X'
    list_of_potential_moves = which(is.na(mat), arr.ind = TRUE)
    Computer_move = list_of_potential_moves[1,]
    R = Computer_move[1]
    C = Computer_move[2]
    mat[R,C] = 'O'
    X_WIN = win_checker_X(mat)
    O_WIN = win_checker_O(mat)
    cat("\n")
    cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
    cat("\n")
    
    print(mat)
    
    cat("\n")
    cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
    cat("\n")
    Sys.sleep(0.5)
    if ((X_WIN == TRUE) || (O_WIN == TRUE)) {
      if ((X_WIN == TRUE)) {
        cat("\n")
        cat("YOU WIN BOI!!!")
        cat("\n")
        WIN = TRUE
        break
      } else {
          cat("\n")
          cat("COMPUTER WIN BOI!!!")
          cat("\n")
          LOSE = TRUE
          break
      }
    } else {  
        cat("\n")
        cat("END OF ROUND#", as.character(i))
        cat("\n")
        i = i + 1
        Sys.sleep(2)
    }
  } else {
      list_of_potential_moves = which(is.na(mat), arr.ind = TRUE)
      Computer_move = list_of_potential_moves[1,]
      R = Computer_move[1]
      C = Computer_move[2]
      mat[R,C] = 'X'
      X_WIN = win_checker_X(mat)
      cat("\n")
      cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
      cat("\n")
      
      print(mat)
      
      cat("\n")
      cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
      cat("\n")
      Sys.sleep(0.5)
      if ((X_WIN == TRUE)) {
        cat("\n")
        cat("COMPUTER WIN BOI!!!")
        cat("\n")
        LOSE = TRUE
        break
      }
      cat("\n")
      cat("############################################################")
      cat("\n")
      cat("\t \t \t ROUND#", as.character(i), " START")
      cat("\n")
      cat("############################################################")
      cat("\n")
      cat("\n")
      cat("\t \t \t Current Board:")
      cat("\n")
      cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
      cat("\n")
    
      print(mat)
    
      cat("\n")
      cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
      cat("\n")
      cat("\n")
      cat("Your move:")
      cat("\n")
      cat("Row:")
      rowwy = as.numeric(readLines(con = con, n = 1))
      cat("Column:")
      collumny = as.numeric(readLines(con = con, n = 1))
      cat("So your move is:", as.character(rowwy), ",", as.character(collumny), "? [y/n]")
      check  = readLines(con = con, n = 1)
      if ((rowwy < 0) == TRUE || (collumny <0) == TRUE || (rowwy > 3) == TRUE || (collumny > 3) == TRUE|| is.numeric(rowwy) == FALSE || is.numeric(collumny) == FALSE) {
        while ((rowwy < 0) == TRUE || (collumny <0) == TRUE || (rowwy > 3) == TRUE || (collumny > 3) == TRUE|| is.numeric(rowwy) == FALSE || is.numeric(collumny) == FALSE) {
          cat("Invalid move")
          cat("\n")
          cat("Your move:")
          cat("\n")
          cat("Row:")
          rowwy = as.numeric(readLines(con = con, n = 1))
          cat("Column:")
          collumny = as.numeric(readLines(con = con, n = 1))
          cat("So your move is:", as.character(rowwy), ",", as.character(collumny), "? [y/n]")
          check  = readLines(con = con, n = 1) 
          if ((check == 'N') == TRUE || (check = 'n') == TRUE) {
            cat("Redo move")
            cat("\n")
            cat("Your move:")
            cat("\n")
            cat("Row:")
            rowwy = as.numeric(readLines(con = con, n = 1))
            cat("Column:")
            collumny = as.numeric(readLines(con = con, n = 1))
            cat("So your move is:", as.character(rowwy), ",", as.character(collumny), "? [y/n]")
            check  = readLines(con = con, n = 1) 
          } else {  
            if (rowwy <0 && collumny <0 && rowwy>3 && collumny>3 && is.numeric(rowwy) == TRUE && is.numeric(collumny) == TRUE && ((check == 'y') == TRUE || (check = 'Y') == TRUE)) {
              break
            }
          }
        }
      }
      P = mat[rowwy,collumny]
      while (is.na(P) == FALSE) {
        cat("Invalid move: move has already been placed in that position")
        cat("\n")
        cat("Your move:")
        cat("\n")
        cat("Row:")
        rowwy = as.numeric(readLines(con = con, n = 1))
        cat("Column:")
        collumny = as.numeric(readLines(con = con, n = 1))
        cat("So your move is:", as.character(rowwy), ",", as.character(collumny), "? [y/n]")
        check  = readLines(con = con, n = 1) 
        P = mat[rowwy,collumny]
        if ((check == 'Y'||check == 'y') == TRUE) {
          if (is.na(P) == TRUE) {
            break
          }
        }
      } 
      while ((check == 'N'|| check == 'n') == TRUE) {
        while (is.na(P) == FALSE) {
          cat("Invalid move: move has already been placed in that position")
          cat("\n")
          cat("Your move:")
          cat("\n")
          cat("Row:")
          rowwy = as.numeric(readLines(con = con, n = 1))
          cat("Column:")
          collumny = as.numeric(readLines(con = con, n = 1))
          cat("So your move is:", as.character(rowwy), ",", as.character(collumny), "? [y/n]")
          check  = readLines(con = con, n = 1) 
          P = mat[rowwy,collumny]
          if ((check == 'Y'|| check == 'y') == TRUE) {
            if (is.na(P) == TRUE) {
              break
            }
          }
        }
        if ((check == 'N'|| check == 'n') == TRUE) {
          cat("Redo your move:")
          cat("\n")
          cat("Row:")
          rowwy = as.numeric(readLines(con = con, n = 1))
          cat("Column:")
          collumny = as.numeric(readLines(con = con, n = 1))
          cat("So your move is:", as.character(rowwy), ",", as.character(collumny), "? [y/n]")
          check  = readLines(con = con, n = 1)
          P = mat[rowwy,collumny]
          if ((is.na(P) == TRUE) && (check == 'Y' || check == 'y') == TRUE) {
            break
          }
        } else {
            break
        }
      }
      mat[rowwy,collumny] = 'O'
      O_WIN = win_checker_O(mat)
      cat("\n")
      cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
      cat("\n")
      
      print(mat)
      
      cat("\n")
      cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
      cat("\n")
      Sys.sleep(0.5)
      if ((O_WIN == TRUE)) {
        cat("\n")
        cat("YOU WIN BOI!!!")
        cat("\n")
        WIN = TRUE
        break
      }
      cat("\n")
      cat("END OF ROUND#", as.character(i))
      cat("\n")
      i = i + 1
      Sys.sleep(2)
    }
}
  

if (WIN == TRUE) {
  cat("Player wins")
} else {
  cat("Computer wins")
}