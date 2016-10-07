# Appendix E
# functions in package

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
  BML.Grid = matrix(data = colors, nrow = r, ncol = c)
  class(BML.Grid) = c("BML", class(BML.Grid))
  return(BML.Grid)
}

MoveMatrix = function(matrix, direction = NA) {
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

MoveCars = function(BML.Grid, move = "red", velocity = FALSE) {
  # red cars move to right (x direction)
  # blue cars move to "up" (y direction)
  # Analyze the grid
  matrix.spaces = BML.Grid == "white"
  matrix.red.cars = BML.Grid == "red"
  matrix.blue.cars = BML.Grid == "blue"
  
  if (move == "red") {
    matrix.check = MoveMatrix(matrix.spaces, direction = "left")
    matrix.move = (matrix.check & matrix.red.cars)  # matrix.move means to do the same thing as matrix.note in the Slower_Vertion. It keeps track of which cars move.
    if (velocity == TRUE) return( sum(matrix.move)/sum(matrix.red.cars) )
    matrix.stay = (!matrix.check & matrix.red.cars)
    # Move the red cars
    matrix.moved = MoveMatrix(matrix = matrix.move, direction = "right")
    matrix.red.cars.new = (matrix.moved | matrix.stay)
    matrix.blue.cars.new = matrix.blue.cars
  } else if (move == "blue") {
    matrix.check = MoveMatrix(matrix.spaces, direction = "up")
    matrix.move = (matrix.check & matrix.blue.cars)  # matrix.move means to do the same thing as matrix.note in the Slower_Vertion. It keeps track of which cars move.
    if (velocity == TRUE) return( sum(matrix.move)/sum(matrix.blue.cars) )
    matrix.stay = (!matrix.check & matrix.blue.cars)
    # Move the blue cars
    matrix.moved = MoveMatrix(matrix = matrix.move, direction = "down")  # in matrix terms, blue cars are moving "down", when we plot this, the blue cars are moving "up".
    matrix.red.cars.new = matrix.red.cars
    matrix.blue.cars.new = (matrix.moved | matrix.stay)
  }
  
  # Make the new grid
  BML.Grid.out = BML.Grid
  BML.Grid.out[,] = "white"
  BML.Grid.out[matrix.red.cars.new] = "red"
  BML.Grid.out[matrix.blue.cars.new] = "blue"
  
  return(BML.Grid.out)
}

velocityBMLGrid = function(BML.Grid, move.first = "red") {
  if (move.first == "red")
    move.second = "blue"
  else if (move.first == "blue")
    move.second = "red"
  
  vel1 = MoveCars(BML.Grid = BML.Grid, move = move.first, velocity = TRUE)
  grid.temp = MoveCars(BML.Grid = BML.Grid, move = move.first)
  vel2 = MoveCars(BML.Grid = BML.Grid, move = move.second, velocity = TRUE)
  num.first = sum(BML.Grid == move.first)
  num.second = sum(BML.Grid == move.second)
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