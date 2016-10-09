# Notes about encoding:
# // "white" == 0
# // "red" == 1
# // "blue" == 2
#
# // "move" == 1
# // "stay" == 0

createBMLGrid = function(r = 100, c = 99, ncars = c(red = 100, blue = 100), rho = NULL, p = 0.5, seed = 1) {
  set.seed(seed)
  n = r*c
  cells = sample(x = n, size = n, replace = FALSE)
  if (is.null(rho)) {
    num.cars = ncars[1] + ncars[2]
    num.red.cars = ncars[1]
  } else {
    num.cars = as.integer(r*c*rho)
    num.red.cars = as.integer(num.cars*p)
  }
  colors = cut(x = cells, breaks = c(0, num.red.cars, num.cars, r*c), labels = c("red", "blue", "white"))
  trans = c("white" = 0L, "red" = 1L, "blue" = 2L)
  BML.Grid = matrix(data = trans[as.character(colors)], nrow = r, ncol = c)
  class(BML.Grid) = c("BML", class(BML.Grid))
  return(BML.Grid)
}

MoveMatrix = function(matrix, direction = NULL) {
  # this function moves matrix to a direction (and put the last row/col to the first row/col)
  num.row = nrow(matrix)
  num.col = ncol(matrix)
  if (direction == "up") {
    matrix.out = matrix[c(2:num.row,1), ]
  } else if (direction == "down") {
    matrix.out = matrix[c(num.row, 1:(num.row-1)), ]
  } else if (direction == "left") {
    matrix.out = matrix[, c(2:num.col,1)]
  } else if (direction == "right") {
    matrix.out = matrix[, c(num.col,1:(num.col-1))]
  }
  return(matrix.out)
}

MoveMatrix.up = function(matrix) {
  # this function moves matrix up
  num.row = nrow(matrix)
  num.col = ncol(matrix)
  matrix.out = matrix[c(2:num.row,1), ]
  return(matrix.out)
}

MoveMatrix.down = function(matrix) {
  # this function moves matrix down
  num.row = nrow(matrix)
  num.col = ncol(matrix)
  matrix.out = matrix[c(num.row, 1:(num.row-1)), ]
  return(matrix.out)
}

MoveMatrix.left = function(matrix) {
  # this function moves matrix left
  num.row = nrow(matrix)
  num.col = ncol(matrix)
  matrix.out = matrix[, c(2:num.col,1)]
  return(matrix.out)
}

MoveMatrix.right = function(matrix) {
  # this function moves matrix right
  num.row = nrow(matrix)
  num.col = ncol(matrix)
  matrix.out = matrix[, c(num.col,1:(num.col-1))]
  return(matrix.out)
}

MoveCars = function(BML.Grid, move = "red", velocity = FALSE) {
  # red cars move to right (x direction)
  # blue cars move to "up" (y direction)
  # Analyze the grid
  matrix.spaces = BML.Grid == 0L
  matrix.red.cars = BML.Grid == 1L
  matrix.blue.cars = BML.Grid == 2L

  if (move == "red") {
    matrix.check = MoveMatrix.left(matrix.spaces)
    matrix.move = (matrix.check & matrix.red.cars)  # matrix.move means to do the same thing as matrix.note in the Slower_Vertion. It keeps track of which cars move.
    if (velocity == TRUE) return( sum(matrix.move)/sum(matrix.red.cars) )
    matrix.stay = (!matrix.check & matrix.red.cars)
    # Move the red cars
    matrix.moved = MoveMatrix.right(matrix = matrix.move)
    matrix.red.cars.new = (matrix.moved | matrix.stay)
    matrix.blue.cars.new = matrix.blue.cars
  } else if (move == "blue") {
    matrix.check = MoveMatrix.up(matrix.spaces)
    matrix.move = (matrix.check & matrix.blue.cars)  # matrix.move means to do the same thing as matrix.note in the Slower_Vertion. It keeps track of which cars move.
    if (velocity == TRUE) return( sum(matrix.move)/sum(matrix.blue.cars) )
    matrix.stay = (!matrix.check & matrix.blue.cars)
    # Move the blue cars
    matrix.moved = MoveMatrix.down(matrix = matrix.move)  # in matrix terms, blue cars are moving "down", when we plot this, the blue cars are moving "up".
    matrix.red.cars.new = matrix.red.cars
    matrix.blue.cars.new = (matrix.moved | matrix.stay)
  }

  # Make the new grid
  BML.Grid.out = BML.Grid
  BML.Grid.out[,] = 0L
  BML.Grid.out[matrix.red.cars.new] = 1L
  BML.Grid.out[matrix.blue.cars.new] = 2L

  return(BML.Grid.out)
}

velocityBMLGrid = function(BML.Grid, move.first = "red") {
  if (move.first == "red") {
    move.first.code = 1L
    move.second = "blue"
    move.second.code = 2L
  } else if (move.first == "blue") {
    move.first.code = 2L
    move.second = "red"
    move.second.code = 1L
  }

  vel1 = MoveCars(BML.Grid = BML.Grid, move = move.first, velocity = TRUE)
  grid.temp = MoveCars(BML.Grid = BML.Grid, move = move.first)
  vel2 = MoveCars(BML.Grid = BML.Grid, move = move.second, velocity = TRUE)
  num.first = sum(BML.Grid == move.first.code)
  num.second = sum(BML.Grid == move.second.code)
  vel.average = weighted.mean(x = c(vel1, vel2), w = c(num.first, num.second))
  return(vel.average)
}

recordVelBML = function(BML.Grid, numSteps = 10000, measureEvery = 10) {
  records.num = as.integer(numSteps/measureEvery)
  v = vector(length = records.num + 1)
  v[1] = format(x = velocityBMLGrid(BML.Grid), digits = 3)

  for (i in 1:numSteps) {
    if (i %% measureEvery == 0) {
      n = as.integer(i/measureEvery)
      v[n+1] = format(x = velocityBMLGrid(BML.Grid), digits = 3)
    }

    if (i %% 2 == 1)
      BML.Grid = MoveCars(BML.Grid = BML.Grid, move = "red")
    else BML.Grid = MoveCars(BML.Grid = BML.Grid, move = "blue")
  }

  df.velocity = cbind(steps = 0:records.num * measureEvery, velocity = v)
  return(df.velocity)
}

runBMLGrid = function (BML.Grid, numSteps = 10000) {
  # run simulation a number of times
  for (i in 1:numSteps) {
    if (i %% 2 == 1)
      BML.Grid = MoveCars(BML.Grid = BML.Grid, move = "red")
    else BML.Grid = MoveCars(BML.Grid = BML.Grid, move = "blue")
  }
  return(BML.Grid)
}


cmoveCars = function(BML.Grid = BML.Grid, move = "red") {
  # This is a wrapper function for C routines to move cars.
  r = as.integer(nrow(BML.Grid))
  c = as.integer(ncol(BML.Grid))
  matrix_note = matrix(0L, nrow = r, ncol = c)

  if (move == "red")
    Grid.out = .C("move_red", BML.Grid, r, c, matrix_note, BML.Grid)[[5]]
  else if (move == "blue")
    Grid.out = .C("move_blue", BML.Grid, r, c, matrix_note, BML.Grid)[[5]]

  return(Grid.out)
}


crunBMLGrid = function (BML.Grid, numSteps = 10000) {
  # This function calls the function "cmoveCars()"
  for (i in 1:numSteps) {
    if (i %% 2 == 1)
      BML.Grid = cmoveCars(BML.Grid = BML.Grid, move = "red")
    else BML.Grid = cmoveCars(BML.Grid = BML.Grid, move = "blue")
  }
  return(BML.Grid)
}
