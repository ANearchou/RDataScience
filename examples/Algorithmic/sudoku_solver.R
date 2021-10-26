
# SUDOKU BACKTRACKING SOLVER

game_c <- c(
  1,0,4,  0,8,3,  9,6,7,
  6,0,5,  0,0,0,  0,2,0,
  7,0,3,  6,1,2,  5,0,8,
  
  3,0,0,  0,0,0,  8,0,4,
  0,0,0,  8,2,0,  0,3,9,
  0,6,0,  0,0,4,  0,0,0,
  
  8,0,0,  0,0,0,  0,9,0,
  0,3,0,  4,6,0,  7,0,0,
  2,7,0,  0,5,8,  0,0,0
)

game <- matrix(game_c, 9, 9, byrow = TRUE)


check_if_possible <- function(x, y, n, game){
  
  if (game[x,y] == 0){
    for (i in 1:9){
      if (game[x,i] == n) {
        return(FALSE)
      }
    }
    for (j in 1:9){
      if (game[j,y] == n) {
        return(FALSE)
      }
    }
    square_x_from <- (ceiling(x/3) - 1 )* 3 + 1 
    square_x_to <- square_x_from + 2
    square_y_from <- (ceiling(y/3) - 1 )* 3 + 1
    square_y_to <- square_y_from + 2
    
    for (ii in square_x_from:square_x_to){
      for (jj in square_y_from:square_y_to){
        if (game[ii,jj] == n) {
          return(FALSE)
        }
      }
    }
  } else {
    print("There is already a number on that cell!")
  }
  return(TRUE)
}


solve_sudoku <- function(game){

  if (length(game[game == 0]) == 0){
    result <<- game
    print(result)
    return(T)
  }
  
  for (i in 1:9){
    for (j in 1:9){
      if (game[i,j] == 0){
        for (try in 1:9){
          if (check_if_possible(i,j,try,game)){
            game2  <- copy(game) 
            game2[i,j] <- try
            if (solve_sudoku(game2)){
              return(T)
            }
          }
        }
        return(FALSE)
      }
    }
  }
  
}

solve_sudoku(game)
