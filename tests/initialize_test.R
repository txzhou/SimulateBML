library(SimulateBML)

g = createBMLGrid(r = 100, c = 99, rho = 0.5)

stopifnot("BML" %in% class(g))