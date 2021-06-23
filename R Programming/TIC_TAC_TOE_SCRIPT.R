suppressMessages(library(dplyr))


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

LIST_OF_POTENTIAL_MOVES = c('1,1','1,2','1,3','2,1','2,2','2,3','3,1', '3,2','3,3')

'%!in%' <- function(x,y)!('%in%'(x,y))

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
    rowwy = readLines(con = con, n = 1) # asd
    cat("Column:")
    collumny = readLines(con = con, n = 1) # df
    
    
    while (rowwy %!in% seq(1:3) || collumny %!in% seq(1:3)) {
      
      cat("Ummmm.... right.... ")
      cat("\n")
      Sys.sleep(5)
      cat("\n")
      cat("Ummmm.. Try again?")
      cat("Redo your move:")
      cat("\n")
      cat("Row:")
      rowwy = readLines(con = con, n = 1)
      cat("Column:")
      collumny = readLines(con = con, n = 1)
      
      move = paste(rowwy,collumny, sep = ",")
      
      if (is.numeric(rowwy) == FALSE && is.numeric(collumny) == FALSE) {
        break
      }
      
    }
    cat("So your move is:", as.character(rowwy), ",", as.character(collumny), "? [y/n]")
    check  = readLines(con = con, n = 1) 
    move = paste(rowwy,collumny, sep = ",")
    while ((move %!in% LIST_OF_POTENTIAL_MOVES) && (check != 'Y' || check != 'y')) {
      if (rowwy %!in% seq(1:3) || collumny %!in% seq(1:3)) {
        
        cat("Move is out of range...") 
        cat("\n")
        cat("Redo your move:")
        cat("\n")
        cat("Row:")
        rowwy = readLines(con = con, n = 1)
        cat("Column:")
        collumny = readLines(con = con, n = 1)
        cat("So your move is:", as.character(rowwy), ",", as.character(collumny), "? [y/n]")
        check  = readLines(con = con, n = 1) 
        move = paste(rowwy,collumny, sep = ",")
        
      } else {
        
        if (check != 'Y' || check != 'y') {
          
          cat("I will let you try again...")
          cat("\n")
          cat("Your move:")
          cat("\n")
          cat("Row:")
          rowwy = readLines(con = con, n = 1)
          cat("Column:")
          collumny = readLines(con = con, n = 1)
          cat("So your move is:", as.character(rowwy), ",", as.character(collumny), "? [y/n]")
          check  = readLines(con = con, n = 1) 
          move = paste(rowwy,collumny, sep = ",")
        }
      }    
      
      if (move %in% LIST_OF_POTENTIAL_MOVES && (check == 'Y' || check == 'y')) {
        move = paste(rowwy,collumny, sep = ",")
        break
        
      }
    }
    mat[as.numeric(rowwy),as.numeric(collumny)] = 'X'
    LIST_OF_POTENTIAL_MOVES = LIST_OF_POTENTIAL_MOVES[LIST_OF_POTENTIAL_MOVES != move]
    list_of_potential_moves = data.frame(which(is.na(mat), arr.ind = TRUE))
    Computer_move = sample_n(list_of_potential_moves, 1)
    R = as.numeric(Computer_move[1])
    C = as.numeric(Computer_move[2])
    mat[R,C] = 'O'
    Computer_move = sprintf("%d,%d", R,C)
    LIST_OF_POTENTIAL_MOVES = LIST_OF_POTENTIAL_MOVES[LIST_OF_POTENTIAL_MOVES != Computer_move]
    cat("\n")
    cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
    cat("\n")
    
    print(mat)
    
    cat("\n")
    cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
    cat("\n")
    Sys.sleep(0.5)
    X_WIN = win_checker_X(mat)
    O_WIN = win_checker_O(mat)
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
    list_of_potential_moves = data.frame(which(is.na(mat), arr.ind = TRUE))
    Computer_move = sample_n(list_of_potential_moves, 1)
    R = as.numeric(Computer_move[1])
    C = as.numeric(Computer_move[2])
    mat[R,C] = 'X'
    Computer_move = sprintf("%d,%d", R,C)
    LIST_OF_POTENTIAL_MOVES = LIST_OF_POTENTIAL_MOVES[LIST_OF_POTENTIAL_MOVES != Computer_move]
    cat("\n")
    cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
    cat("\n")
    
    print(mat)
    
    cat("\n")
    cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
    cat("\n")
    X_WIN = win_checker_X(mat)
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
    rowwy = readLines(con = con, n = 1)
    cat("Column:")
    collumny = readLines(con = con, n = 1)
    
    while (rowwy %!in% seq(1:3) || collumny %!in% seq(1:3)) {
      
      cat("Ummmm.... right.... ")
      cat("\n")
      Sys.sleep(5)
      cat("\n")
      cat("Ummmm.. Try again?")
      cat("Redo your move:")
      cat("\n")
      cat("Row:")
      rowwy = readLines(con = con, n = 1)
      cat("Column:")
      collumny = readLines(con = con, n = 1)
      move = paste(rowwy,collumny, sep = ",")
      
      if (is.numeric(rowwy) == FALSE && is.numeric(collumny) == FALSE) {
        break
      }
      
    }
    cat("So your move is:", as.character(rowwy), ",", as.character(collumny), "? [y/n]")
    check  = readLines(con = con, n = 1) 
    move = paste(rowwy,collumny, sep = ",")
    
    while (move %!in% LIST_OF_POTENTIAL_MOVES && (check != 'Y' || check != 'y')) {
      if (rowwy %!in% seq(1:3) || collumny %!in% seq(1:3)) {
        
        cat("Move is out of range...") 
        cat("\n")
        cat("Redo your move:")
        cat("\n")
        cat("Row:")
        rowwy = readLines(con = con, n = 1)
        cat("Column:")
        collumny = readLines(con = con, n = 1)
        cat("So your move is:", as.character(rowwy), ",", as.character(collumny), "? [y/n]")
        check  = readLines(con = con, n = 1) 
        move = paste(rowwy,collumny, sep = ",")
        
      } else {
        
        if (check != 'Y' || check != 'y') {
          
          cat("I will let you try again...")
          cat("\n")
          cat("Your move:")
          cat("\n")
          cat("Row:")
          rowwy = readLines(con = con, n = 1)
          cat("Column:")
          collumny = readLines(con = con, n = 1)
          cat("So your move is:", as.character(rowwy), ",", as.character(collumny), "? [y/n]")
          check  = readLines(con = con, n = 1) 
          move = paste(rowwy,collumny, sep = ",")
        }
      }    
      
      if (move %in% LIST_OF_POTENTIAL_MOVES && (check == 'Y' || check == 'y')) {
        move = paste(rowwy,collumny, sep = ",")
        break
        
      }
    } 
    mat[as.numeric(rowwy),as.numeric(collumny)] = 'O'
    LIST_OF_POTENTIAL_MOVES = LIST_OF_POTENTIAL_MOVES[LIST_OF_POTENTIAL_MOVES != move]
    cat("\n")
    cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
    cat("\n")
    
    print(mat)
    
    cat("\n")
    cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
    cat("\n")
    O_WIN = win_checker_O(mat)
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